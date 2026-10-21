# Cicatriz de un evento documentado medida con Sentinel-2: el dNBR entre la
# escena previa y la posterior, y el perímetro de la mancha quemada.
#
# Es la tercera medición del evento, independiente de las otras dos. Las
# detecciones de fuego activo dicen dónde hubo llama al pasar el satélite y
# el área quemada de 500 m dónde persistió un cambio de reflectancia semanas
# después; el dNBR a 20 m ve la cicatriz al tamaño del typhal. Para Catalina
# el sondeo que precedió a este módulo (2026-08-06) dio un perímetro de
# ~3300 ha, entre las 706 ha de VNP64A1 y las 4000 oficiales, y mostró que
# el núcleo severo (dNBR > 0,44) coincide casi exactamente con lo que el
# producto de 500 m sí marcó: el producto no falla al azar, captura el núcleo
# y pierde el resto.
#
# NBR = (B8A − B12) / (B8A + B12); dNBR = NBR(previa) − NBR(post). Los
# umbrales 0,10 / 0,27 / 0,44 son los de la escala de severidad de Key &
# Benson (FIREMON) para cambio detectable, moderado y alto.
#
# El perímetro NO es un umbral suelto sino una mancha conexa: un umbral solo
# confunde reverdecimiento y suelo descubierto con quema (la laguna de Palo
# Verde tiene 650 ha con dNBR > 0,10 que son marisma reverdecida, no fuego).
# Criterio: cambio (> 0,10) abierto con ventana 3×3, conservado solo a
# ≤ CICATRIZ_ANCLAJE_M de un píxel con severidad moderada, cerrado con
# ventana CICATRIZ_CIERRE_PX, componente conexo mayor y huecos rellenados
# (los islotes de bosque que el fuego rodeó forman parte del perímetro, que es
# lo que un perímetro de incendio significa). El anclaje es lo que impide que
# el cierre morfológico salte a marismas vecinas que solo reverdecieron; el
# resultado varía < 2 % si la distancia se mueve entre 400 y 800 m.

# Clases de la banda SCL que invalidan un píxel en cualquiera de las dos
# fechas: sin dato, saturado, sombra, sombra de nube, nube media y alta, cirro.
SCL_INVALIDAS <- c(0, 1, 2, 3, 8, 9, 10)

DNBR_CAMBIO   <- 0.10
DNBR_MODERADO <- 0.27
DNBR_ALTO     <- 0.44

CICATRIZ_ANCLAJE_M <- 500
CICATRIZ_CIERRE_PX <- 11L   # 11 px a 20 m: radio de ~100 m

# Humedal de control: el que más se parece a una quema sin serlo (reverdece
# con las lluvias entre las dos fechas). Las cifras lo reportan para que la
# prosa pueda decir que el perímetro no se derrama hacia él.
HUMEDAL_CONTROL <- "Humedal Palo Verde 1"

# dNBR de un evento a partir de las bandas descargadas (escenas_s2_evento() y
# descargar_banda_s2()), enmascarado donde cualquiera de las dos fechas tiene
# nube, sombra o datos faltantes. Escribe un GeoTIFF en el CRS de la escena
# (UTM) y retorna el path (target con format = "file").
dnbr_evento <- function(escenas, archivos, dest) {
  escenas <- dplyr::ungroup(escenas)
  banda <- function(momento, nombre) {
    archivo <- archivos[basename(archivos) ==
                          escenas$nombre[escenas$momento == momento &
                                           escenas$banda == nombre]]
    terra::rast(archivo)
  }
  nbr <- function(momento) {
    nir <- banda(momento, "B8A")
    swir <- banda(momento, "B12")
    (nir - swir) / (nir + swir)
  }
  invalido <- (banda("previa", "SCL") %in% SCL_INVALIDAS) |
    (banda("post", "SCL") %in% SCL_INVALIDAS)
  d <- terra::mask(nbr("previa") - nbr("post"), invalido, maskvalues = 1)
  names(d) <- "dnbr"
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  terra::writeRaster(d, dest, overwrite = TRUE, datatype = "FLT4S",
                     gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
  dest
}

# Operador morfológico binario sobre un raster 0/1 sin NA: erosión (min) o
# dilatación (max) con ventana cuadrada de `w` píxeles; el borde se rellena
# con 0 como hace scipy.ndimage.
morfologia <- function(x, w, fun) {
  terra::focal(x, w = w, fun = fun, fillvalue = 0, expand = FALSE)
}

# Perímetro de la cicatriz y cifras del evento. `dnbr` es el path del GeoTIFF
# de dnbr_evento(). Escribe el perímetro como GeoPackage en CRTM05 (para un
# SIG) y retorna una lista con el polígono (sf), las cifras (tibble en
# hectáreas) y el path.
cicatriz_evento <- function(clave_evento, dnbr, parque, humedales, dest) {
  e <- evento(clave_evento)
  d <- terra::rast(dnbr)
  crs_d <- terra::crs(d)
  px_ha <- prod(terra::res(d)) / 1e4
  rasterizar <- function(x) {
    terra::rasterize(terra::vect(sf::st_transform(x, crs_d)), d, field = 1,
                     background = 0)
  }
  ha <- function(m) as.numeric(terra::global(m, "sum", na.rm = TRUE)) * px_ha

  parque_r <- rasterizar(parque)
  sector <- sf::st_union(humedales[grepl(e$sector, humedales$nom_hum,
                                         fixed = TRUE), ])
  control <- sf::st_intersection(
    sf::st_union(humedales[humedales$nom_hum == HUMEDAL_CONTROL, ]),
    sf::st_union(sf::st_transform(parque, sf::st_crs(humedales)))
  )
  sector_r <- rasterizar(sector)
  control_r <- rasterizar(control)

  # Sin NA para la morfología: un píxel inválido cuenta como "sin cambio",
  # igual que en el sondeo (nan_to_num a −9).
  dd <- terra::ifel(is.na(d), -9, d)
  en_parque <- parque_r == 1
  cambio <- (dd > DNBR_CAMBIO) & en_parque
  nucleo <- (dd > DNBR_MODERADO) & en_parque

  abierto <- morfologia(morfologia(cambio, 3, "min"), 3, "max")
  cerca <- terra::as.polygons(terra::ifel(nucleo, 1, NA), dissolve = TRUE) |>
    sf::st_as_sf() |>
    sf::st_union() |>
    sf::st_buffer(CICATRIZ_ANCLAJE_M)
  anclado <- abierto & (rasterizar(cerca) == 1)
  cerrado <- morfologia(morfologia(anclado, CICATRIZ_CIERRE_PX, "max"),
                        CICATRIZ_CIERRE_PX, "min")

  # Componente conexo mayor (8 vecinos) y relleno de huecos: el polígono se
  # reconstruye solo con los anillos exteriores.
  parches <- terra::patches(terra::ifel(cerrado == 1, 1, NA), directions = 8)
  frecuencias <- terra::freq(parches)
  mayor <- frecuencias$value[which.max(frecuencias$count)]
  mancha <- terra::as.polygons(terra::ifel(parches == mayor, 1, NA),
                               dissolve = TRUE) |>
    sf::st_as_sf() |>
    sf::st_geometry() |>
    sf::st_union() |>
    sf::st_cast("POLYGON")
  perimetro <- sf::st_sfc(
    lapply(mancha, function(pg) sf::st_polygon(list(pg[[1]]))),
    crs = sf::st_crs(mancha)
  ) |>
    sf::st_union()
  perimetro_r <- rasterizar(perimetro)
  dentro <- perimetro_r == 1

  # Cambio disperso fuera del perímetro: el término de ruido (barras del río,
  # potreros, marismas que reverdecen), caracterizado por su fragmentación.
  disperso <- cambio & !dentro
  parches_fuera <- terra::patches(terra::ifel(disperso, 1, NA), directions = 8)
  tam_fuera <- terra::freq(parches_fuera)$count * px_ha

  cifras <- tibble::tibble(
    perimetro_ha      = ha(dentro),
    quemado_ha        = ha(dentro & cambio),
    islotes_ha        = ha(dentro & !cambio),
    moderado_ha       = ha(dentro & nucleo),
    alto_ha           = ha(dentro & (dd > DNBR_ALTO)),
    sector_ha         = ha(dentro & (sector_r == 1)),
    sector_total_ha   = as.numeric(sf::st_area(sector)) / 1e4,
    fuera_sector_ha   = ha(dentro & (sector_r == 0)),
    control_ha        = ha(dentro & (control_r == 1)),
    control_cambio_ha = ha(cambio & (control_r == 1)),
    control_moderado_ha = ha(nucleo & (control_r == 1)),
    disperso_ha       = ha(disperso),
    disperso_parches  = length(tam_fuera),
    disperso_mediana_ha = if (length(tam_fuera)) stats::median(tam_fuera) else NA_real_,
    cambio_parque_ha  = ha(cambio),
    nubes_parque_ha   = ha(is.na(d) & en_parque),
    fondo_mediana     = as.numeric(terra::global(terra::mask(d, parque_r,
                                                             maskvalues = 0),
                                                 \(v) stats::median(v, na.rm = TRUE)))
  )

  perimetro_sf <- sf::st_sf(
    evento = clave_evento,
    s2_previa = e$s2_previa, s2_post = e$s2_post,
    hectareas = round(cifras$perimetro_ha),
    geometry = sf::st_transform(perimetro, CRS_CRTM05)
  )
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(dest)) file.remove(dest)
  sf::st_write(perimetro_sf, dest, layer = "perimetro", quiet = TRUE)

  list(evento = e, perimetro = perimetro_sf, cifras = cifras, archivo = dest)
}

# Cifras de la cicatriz en CSV (formato largo, hectáreas con un decimal).
tabla_cicatriz_csv <- function(cicatriz, dest) {
  tabla <- tidyr::pivot_longer(cicatriz$cifras, dplyr::everything(),
                               names_to = "medida", values_to = "valor") |>
    dplyr::mutate(valor = round(valor, 3))
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(tabla, dest)
  dest
}

# Pie de las figuras hechas con Sentinel-2. La atribución es la que pide la
# licencia de Copernicus.
PIE_SENTINEL2 <- paste0("Datos: Copernicus Sentinel-2 L2A (Earth Search / AWS) y SINAC. ",
                        "Contains modified Copernicus Sentinel data")

# Mapa de la cicatriz: el dNBR sobre el sector del evento con el perímetro
# encima, mismo encuadre que las otras figuras del evento. La escala es
# divergente en torno a cero porque el signo importa: negativo es
# reverdecimiento (la marisma que se inundó entre las fechas), positivo es
# pérdida de vegetación; los cortes marcan los umbrales de severidad.
grafico_cicatriz_evento <- function(cicatriz, dnbr, parque, humedales, dest,
                                    fuente_datos) {
  e <- cicatriz$evento
  sector <- humedales[grepl(e$sector, humedales$nom_hum, fixed = TRUE), ] |>
    sf::st_transform(CRS_CRTM05)
  parque <- sf::st_transform(parque, CRS_CRTM05)
  caja <- sf::st_bbox(sector)
  margen <- 0.12 * max(caja["xmax"] - caja["xmin"], caja["ymax"] - caja["ymin"])
  ventana <- terra::ext(caja["xmin"] - margen, caja["xmax"] + margen,
                        caja["ymin"] - margen, caja["ymax"] + margen)
  # Al CRS de los mapas (CRTM05) y recortado al encuadre antes de pasarlo a
  # ggplot: el raster completo del parque son 1,7 millones de celdas.
  d <- terra::rast(dnbr) |>
    terra::project(CRS_CRTM05, method = "bilinear", res = S2_RES_M) |>
    terra::crop(ventana)
  celdas <- terra::as.data.frame(d, xy = TRUE, na.rm = TRUE)
  celdas$dnbr <- pmin(pmax(celdas$dnbr, -0.3), 0.7)

  p <- ggplot2::ggplot() +
    ggplot2::geom_raster(data = celdas, ggplot2::aes(x, y, fill = dnbr)) +
    ggplot2::scale_fill_gradient2(
      low = "#2166ac", mid = "grey97", high = "#b2182b", midpoint = 0,
      limits = c(-0.3, 0.7),
      breaks = c(-0.3, 0, DNBR_CAMBIO, DNBR_MODERADO, DNBR_ALTO, 0.7),
      labels = c("−0,3", "0", "0,10", "0,27", "0,44", "0,7"),
      name = "dNBR"
    ) +
    ggplot2::geom_sf(data = parque, fill = NA, color = "grey30",
                     linewidth = 0.5) +
    ggplot2::geom_sf(data = sf::st_union(sector), fill = NA, color = "grey15",
                     linewidth = 0.7, linetype = "22") +
    ggplot2::geom_sf(data = cicatriz$perimetro, fill = NA,
                     color = COLOR_CICATRIZ, linewidth = 0.8) +
    ggplot2::coord_sf(
      xlim = c(caja["xmin"] - margen, caja["xmax"] + margen),
      ylim = c(caja["ymin"] - margen, caja["ymax"] + margen),
      datum = sf::st_crs(CRS_WGS84), expand = FALSE
    ) +
    ggplot2::labs(
      title = paste0("La cicatriz del ", e$nombre, " en Sentinel-2"),
      subtitle = paste0(
        "dNBR entre el ", format(e$s2_previa, "%d/%m/%Y"), " y el ",
        format(e$s2_post, "%d/%m/%Y"), " a 20 m: positivo es pérdida de",
        " vegetación, negativo es reverdecimiento.\nEl trazo azul es el",
        " perímetro de la cicatriz (", format(round(cicatriz$cifras$perimetro_ha),
                                             big.mark = " "),
        " ha, islotes incluidos); el discontinuo delimita el ", e$sector,
        "\ndel Registro Nacional de Humedales"
      ),
      x = NULL, y = NULL, caption = fuente_datos
    ) +
    ggplot2::guides(fill = ggplot2::guide_colorbar(barwidth = 18,
                                                   barheight = 0.6)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_line(color = "grey92", linewidth = 0.3),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 9, lineheight = 1.2),
      legend.position = "bottom",
      plot.caption = ggplot2::element_text(size = 7, color = "grey40")
    )

  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(dest, p, width = 7, height = 6.8, dpi = 150)
  dest
}
