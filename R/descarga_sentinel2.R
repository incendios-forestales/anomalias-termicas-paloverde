# Descubrimiento y descarga de escenas Sentinel-2 L2A para los eventos
# documentados (R/evento.R): el insumo de la cicatriz por dNBR, la medición
# independiente que falta entre el área quemada de 500 m y la cifra oficial.
#
# Por qué Sentinel-2 y no otro producto: a 20 m resuelve la cicatriz de un
# typhal que el píxel de 500 m de MCD64A1/VNP64A1 pierde (Catalina: 706 ha en
# el producto frente a ~4000 oficiales), y el par de escenas se obtiene sin
# credenciales. Es una medición por evento, no una serie: cada evento declara
# en EVENTOS su escena previa y posterior.
#
# APIs:
#   Búsqueda (Earth Search, STAC, pública):
#     POST {STAC_EARTH_SEARCH}  {"collections":["sentinel-2-l2a"], "bbox":..., "datetime":...}
#   Lectura (COG públicos en AWS, sin credenciales):
#     /vsicurl/https://sentinel-cogs.s3.us-west-2.amazonaws.com/.../B8A.tif
#
# La descarga no baja la tesela completa (110 km de lado, ~100 MB por banda)
# sino la ventana del parque con margen: GDAL lee por HTTP solo los bloques
# del COG que la cubren y terra escribe un GeoTIFF local de unos pocos MB.

# Solicitud POST al STAC con reintentos ante errores transitorios, mismo
# patrón que cmr_solicitar(). Retorna el cuerpo ya parseado.
stac_solicitar <- function(cuerpo, max_intentos = 5L, espera_s = 30) {
  intentos <- 0L
  repeat {
    intentos <- intentos + 1L
    resp <- httr2::request(STAC_EARTH_SEARCH) |>
      httr2::req_body_json(cuerpo) |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_perform()
    estado <- httr2::resp_status(resp)
    if (!estado %in% c(429L, 500L, 502L, 503L)) break
    if (intentos >= max_intentos) {
      stop("Earth Search siguió rechazando la solicitud tras ", intentos,
           " intentos (HTTP ", estado, ").", call. = FALSE)
    }
    message(glue::glue(
      "[espera] Earth Search no disponible (HTTP {estado}); reintento en {espera_s} s ({intentos}/{max_intentos})"
    ))
    Sys.sleep(espera_s)
  }
  if (estado != 200L) {
    stop("HTTP ", estado, " de Earth Search: ",
         substr(httr2::resp_body_string(resp), 1, 200), call. = FALSE)
  }
  # Listas puras (sin simplificar a data.frame): las geometrías y los assets
  # de cada ítem se recorren mejor como listas anidadas.
  jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = FALSE)
}

# Escena L2A de UNA fecha que cubre el parque completo. El bbox del parque
# suele tocar dos teselas MGRS (en Palo Verde, 16PFS y 16PGS) y ambas aparecen
# como ítems distintos de la misma pasada; se conserva la que contiene al
# polígono, porque una cicatriz partida entre teselas obligaría a mosaicar.
# Retorna una fila por banda de S2_BANDAS con la URL de su COG.
stac_escena_s2 <- function(fecha, parque) {
  caja <- sf::st_bbox(sf::st_transform(parque, CRS_WGS84))
  cuerpo <- list(
    collections = list(S2_COLECCION),
    bbox = as.numeric(caja[c("xmin", "ymin", "xmax", "ymax")]),
    datetime = paste0(format(fecha, "%Y-%m-%d"), "T00:00:00Z/",
                      format(fecha, "%Y-%m-%d"), "T23:59:59Z"),
    limit = 20L
  )
  items <- stac_solicitar(cuerpo)$features
  if (length(items) == 0) {
    stop("Earth Search no tiene escenas Sentinel-2 L2A del ", fecha,
         " sobre el parque.", call. = FALSE)
  }
  escenas <- purrr::map(items, function(item) {
    geom <- sf::st_as_sfc(
      as.character(jsonlite::toJSON(item$geometry, auto_unbox = TRUE,
                                    digits = NA)),
      GeoJSON = TRUE
    )
    sf::st_crs(geom) <- CRS_WGS84
    list(id = item$id, geom = geom,
         nubes = item$properties[["eo:cloud_cover"]],
         assets = item$assets)
  })
  cubre <- purrr::map_lgl(escenas, function(e) {
    isTRUE(sf::st_contains(e$geom, sf::st_union(sf::st_transform(parque, CRS_WGS84)),
                           sparse = FALSE)[1, 1])
  })
  if (!any(cubre)) {
    stop("Ninguna escena del ", fecha, " contiene al parque completo: ",
         paste(purrr::map_chr(escenas, "id"), collapse = ", "), call. = FALSE)
  }
  escena <- escenas[cubre][[1]]
  tibble::tibble(
    fecha = fecha,
    id = escena$id,
    nubes_tesela = escena$nubes,
    asset = names(S2_BANDAS),
    banda = unname(S2_BANDAS),
    url = purrr::map_chr(names(S2_BANDAS), function(a) escena$assets[[a]]$href)
  )
}

# Las dos escenas (previa y posterior) de un evento, una fila por banda, en
# un data frame agrupado por fila para branching dinámico: cada banda se
# descarga y se cachea por separado, y si Earth Search reprocesa una escena
# cambia su id y solo esas ramas se invalidan.
escenas_s2_evento <- function(clave_evento, parque) {
  e <- evento(clave_evento)
  if (is.na(e$s2_previa) || is.na(e$s2_post)) {
    stop("El evento '", clave_evento, "' no declara s2_previa/s2_post.",
         call. = FALSE)
  }
  dplyr::bind_rows(
    dplyr::mutate(stac_escena_s2(e$s2_previa, parque), momento = "previa"),
    dplyr::mutate(stac_escena_s2(e$s2_post, parque), momento = "post")
  ) |>
    dplyr::mutate(evento = clave_evento,
                  nombre = paste0(id, "_", banda, ".tif")) |>
    dplyr::relocate(evento, momento) |>
    dplyr::group_by(id, banda) |>
    targets::tar_group()
}

# Ventana de recorte: el parque con S2_MARGEN_KM de margen, en el CRS de la
# escena (UTM) y alineada a la rejilla de S2_RES_M para que las dos fechas
# compartan píxeles exactos y el dNBR se calcule sin remuestreo.
ventana_s2 <- function(parque, crs_escena) {
  caja <- parque |>
    sf::st_transform(crs_escena) |>
    sf::st_bbox()
  m <- S2_MARGEN_KM * 1000
  r <- S2_RES_M
  terra::ext(floor((caja["xmin"] - m) / r) * r, ceiling((caja["xmax"] + m) / r) * r,
             floor((caja["ymin"] - m) / r) * r, ceiling((caja["ymax"] + m) / r) * r)
}

# Descarga UNA banda (data frame de 1 fila de escenas_s2_evento()) recortada
# a la ventana del parque. Idempotente como descargar_granulo_ba(): si el
# GeoTIFF ya existe con contenido lo retorna sin leer la red; escritura a un
# temporal y rename para no dejar archivos a medias. Verifica que la ventana
# quede dentro de la tesela: un recorte parcial pasaría inadvertido y daría
# una cicatriz truncada. Retorna el path (target con format = "file").
descargar_banda_s2 <- function(fila, parque, dir, max_intentos = 3L,
                               espera_s = 20) {
  dest <- file.path(dir, fila$nombre)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  if (file.exists(dest) && file.info(dest)$size > 0) {
    message(glue::glue("[cache] {basename(dest)} ya existe"))
    return(dest)
  }
  message(glue::glue("[descarga] {fila$id} {fila$banda} -> {dest}"))
  intentos <- 0L
  repeat {
    intentos <- intentos + 1L
    resultado <- try({
      cog <- terra::rast(paste0("/vsicurl/", fila$url))
      ventana <- ventana_s2(parque, terra::crs(cog))
      tesela <- as.vector(terra::ext(cog))
      v <- as.vector(ventana)
      if (v["xmin"] < tesela["xmin"] || v["xmax"] > tesela["xmax"] ||
          v["ymin"] < tesela["ymin"] || v["ymax"] > tesela["ymax"]) {
        stop("La ventana del parque se sale de la tesela ", fila$id,
             "; reduzca S2_MARGEN_KM o mosaique teselas.", call. = FALSE)
      }
      recorte <- terra::crop(cog, ventana, snap = "out")
      names(recorte) <- fila$banda
      temporal <- paste0(dest, ".part.tif")
      terra::writeRaster(
        recorte, temporal, overwrite = TRUE,
        datatype = if (fila$banda == "SCL") "INT1U" else "INT2U",
        gdal = c("COMPRESS=DEFLATE", "TILED=YES")
      )
      file.rename(temporal, dest)
    }, silent = TRUE)
    if (!inherits(resultado, "try-error")) break
    if (intentos >= max_intentos || grepl("se sale de la tesela", resultado)) {
      stop(as.character(resultado), call. = FALSE)
    }
    message(glue::glue("[espera] fallo al leer {fila$id} {fila$banda}; reintento en {espera_s} s ({intentos}/{max_intentos})"))
    Sys.sleep(espera_s)
  }
  dest
}
