# Descubrimiento y descarga de granulos MCD64A1 v6.1 (área quemada mensual
# MODIS, 500 m): el complemento de "área quemada" de las detecciones de fuego
# activo de FIRMS.
#
# Por qué NO se usa el API de FIRMS para el área quemada: FIRMS lista BA_MODIS
# en data_availability y la muestra como capa WMS en su visor, pero su API de
# área acepta la colección y responde siempre vacío (verificado el 2026-07-29
# incluso con megaincendios: Australia dic-2019, California sep-2020). El dato
# analítico es el producto original MCD64A1 de LP DAAC, en formato HDF4-EOS2
# con la banda "Burn Date" (día del año de la quema por píxel; 0 = sin quema,
# negativos = agua o datos insuficientes).
#
# APIs:
#   Búsqueda (CMR, pública):
#     GET {CMR_BASE}/granules.json?short_name=MCD64A1&version=061&...
#   Descarga (LP DAAC, requiere token Bearer de Earthdata Login):
#     GET https://data.lpdaac.earthdatacloud.nasa.gov/lp-prod-protected/...hdf
#
# Los nombres de archivo incluyen el timestamp de producción (p. ej.
# MCD64A1.A2020306.h09v07.061.2021307220158.hdf), impredecible desde el
# calendario: el descubrimiento vía CMR es obligatorio, no una conveniencia.

# Solicitud GET al API CMR con reintentos ante errores transitorios.
# `search_after` es el valor del encabezado CMR-Search-After de la página
# anterior (paginación sin offset). Retorna la respuesta httr2 completa:
# el llamador necesita cuerpo Y encabezados.
cmr_solicitar <- function(url, search_after = NULL, max_intentos = 5L,
                          espera_s = 30) {
  intentos <- 0L
  repeat {
    intentos <- intentos + 1L
    solicitud <- httr2::request(url) |>
      httr2::req_error(is_error = function(r) FALSE)
    if (!is.null(search_after)) {
      solicitud <- httr2::req_headers(solicitud,
                                      `CMR-Search-After` = search_after)
    }
    resp <- httr2::req_perform(solicitud)
    estado <- httr2::resp_status(resp)
    if (!estado %in% c(429L, 500L, 502L, 503L)) break
    if (intentos >= max_intentos) {
      stop("El API CMR siguió rechazando la solicitud tras ", intentos,
           " intentos (HTTP ", estado, ").", call. = FALSE)
    }
    message(glue::glue(
      "[espera] CMR no disponible (HTTP {estado}); reintento en {espera_s} s ({intentos}/{max_intentos})"
    ))
    Sys.sleep(espera_s)
  }
  if (estado != 200L) {
    stop("HTTP ", estado, " del API CMR: ",
         substr(httr2::resp_body_string(resp), 1, 200), call. = FALSE)
  }
  resp
}

# Granulos MCD64A1 disponibles para la tesela dentro del rango de fechas.
# Pagina con CMR-Search-After hasta agotar los resultados (hoy ~300 granulos
# caben en una página, pero el bucle evita un truncado silencioso futuro).
# Los meses aún no publicados (el producto sale con ~1-2 meses de rezago)
# simplemente no aparecen: el cue "always" del target los incorpora cuando
# existan. Retorna un data frame agrupado por fila para branching dinámico,
# con la clave de grupo `aniomes` (estable ante meses nuevos; si LP DAAC
# reprocesa un granulo cambia su nombre/URL y solo esa rama se invalida).
cmr_granulos_mcd64a1 <- function(fecha_inicio, fecha_fin,
                                 tesela = TESELA_MCD64A1) {
  url <- glue::glue(
    "{CMR_BASE}/granules.json",
    "?short_name={MCD64A1_SHORT_NAME}&version={MCD64A1_VERSION}",
    "&readable_granule_name=*{tesela}*",
    "&options[readable_granule_name][pattern]=true",
    "&page_size=2000&sort_key=start_date"
  )
  paginas <- list()
  search_after <- NULL
  repeat {
    resp <- cmr_solicitar(url, search_after)
    entradas <- yyjsonr::read_json_str(httr2::resp_body_string(resp))$feed$entry
    if (length(entradas) == 0 || nrow(entradas) == 0) break
    paginas <- c(paginas, list(entradas))
    search_after <- httr2::resp_header(resp, "CMR-Search-After")
    if (is.null(search_after)) break
  }
  if (length(paginas) == 0) {
    stop("CMR no devolvió granulos MCD64A1 para la tesela ", tesela,
         "; verifique TESELA_MCD64A1.", call. = FALSE)
  }

  granulos <- purrr::list_rbind(purrr::map(paginas, function(entradas) {
    data.frame(
      aniomes = lubridate::floor_date(as.Date(substr(entradas$time_start, 1, 10)),
                                      "month"),
      # yyjsonr entrega `links` como lista de data.frames (un data.frame de
      # enlaces por granulo); se toma el primer enlace https de descarga .hdf.
      url = purrr::map_chr(entradas$links, function(enlaces) {
        urls <- if (is.data.frame(enlaces)) enlaces$href
                else purrr::map_chr(enlaces, "href")
        hdf <- urls[grepl("^https://", urls) & grepl("[.]hdf$", urls)]
        if (length(hdf) == 0) NA_character_ else hdf[[1]]
      })
    )
  })) |>
    dplyr::filter(!is.na(url)) |>
    dplyr::mutate(nombre = basename(url)) |>
    dplyr::filter(aniomes >= lubridate::floor_date(fecha_inicio, "month"),
                  aniomes <= fecha_fin)

  if (nrow(granulos) == 0) {
    stop("Ningún granulo MCD64A1 en el rango ", fecha_inicio, " a ", fecha_fin,
         ".", call. = FALSE)
  }
  granulos |>
    dplyr::group_by(aniomes) |>
    targets::tar_group()
}

# Descarga UN granulo (data frame de 1 fila con columnas aniomes, nombre, url).
# Idempotente y atómica, como descargar_firms_fragmento():
#   - si el HDF ya existe con contenido, lo retorna sin descargar;
#   - escritura .part -> rename;
#   - valida el número mágico HDF4: un HTML de error guardado como .hdf
#     rompería terra mucho después, lejos de la causa.
# Un HTTP 401 es terminal (el token de Earthdata expira a los ~60 días);
# reintentar no lo arregla.
# Retorna el path al HDF (target con format = "file").
descargar_granulo_mcd64a1 <- function(granulo, dir = "data/raw/mcd64a1",
                                      max_intentos = 5L, espera_s = 30) {
  destino <- file.path(dir, granulo$nombre[[1]])
  if (file.exists(destino) && file.info(destino)$size > 0) {
    message(glue::glue("[cache] {basename(destino)} ya existe"))
    return(destino)
  }
  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  message(glue::glue("[descarga] {basename(destino)}"))

  intentos <- 0L
  repeat {
    intentos <- intentos + 1L
    resp <- httr2::request(granulo$url[[1]]) |>
      httr2::req_auth_bearer_token(earthdata_token()) |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_perform()
    estado <- httr2::resp_status(resp)
    if (!estado %in% c(429L, 500L, 502L, 503L)) break
    if (intentos >= max_intentos) {
      stop("LP DAAC siguió rechazando la descarga de ", basename(destino),
           " tras ", intentos, " intentos (HTTP ", estado, ").", call. = FALSE)
    }
    message(glue::glue(
      "[espera] LP DAAC no disponible (HTTP {estado}); reintento en {espera_s} s ({intentos}/{max_intentos})"
    ))
    Sys.sleep(espera_s)
  }
  if (estado == 401L) {
    stop("LP DAAC rechazó el token (HTTP 401) al descargar ", basename(destino),
         ". Los tokens de Earthdata expiran a los ~60 días: regenere el suyo ",
         "en https://urs.earthdata.nasa.gov/ y actualice EARTHDATA_TOKEN en .Renviron.",
         call. = FALSE)
  }
  if (estado != 200L) {
    stop("HTTP ", estado, " de LP DAAC al descargar ", basename(destino), ": ",
         substr(httr2::resp_body_string(resp), 1, 200), call. = FALSE)
  }

  cuerpo <- httr2::resp_body_raw(resp)
  magia_hdf4 <- as.raw(c(0x0e, 0x03, 0x13, 0x01))
  if (length(cuerpo) < 4L || !identical(cuerpo[1:4], magia_hdf4)) {
    stop("La respuesta de LP DAAC para ", basename(destino),
         " no es un HDF4 (posible página de error).", call. = FALSE)
  }

  temporal <- paste0(destino, ".part")
  writeBin(cuerpo, temporal)
  file.rename(temporal, destino)
  destino
}
