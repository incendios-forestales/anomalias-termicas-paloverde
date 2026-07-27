# Cobertura de la tierra (ESA WorldCover 10 m, 2021) en los footprints de
# las detecciones MODIS.
#
# Método: cada detección MODIS es un píxel de ~1 km (mayor fuera del nadir);
# las columnas scan/track dan sus dimensiones reales en km. Asignar la clase
# del punto exacto sobre un mapa de 10 m sería precisión espuria, por lo que
# se calcula la composición de clases dentro de un footprint elíptico de ejes
# scan x track (aproximado como alineado a los ejes: el eje de barrido de MODIS
# es aproximadamente este-oeste en estas latitudes) y se reporta la fracción
# por clase y la clase dominante.
#
# Limitación documentada en el reporte: WorldCover es una foto fija de 2021
# frente a un registro de detecciones 2001-2026.

WORLDCOVER_BASE <- "https://esa-worldcover.s3.eu-central-1.amazonaws.com/v200/2021/map"

# Clases WorldCover v200 (valor del píxel -> etiqueta en español).
CLASES_WORLDCOVER <- c(
  `10`  = "Bosque",
  `20`  = "Matorral",
  `30`  = "Pastizal",
  `40`  = "Cultivos",
  `50`  = "Zonas construidas",
  `60`  = "Suelo desnudo / vegetación escasa",
  `70`  = "Nieve y hielo",
  `80`  = "Cuerpos de agua",
  `90`  = "Humedal herbáceo",
  `95`  = "Manglar",
  `100` = "Musgos y líquenes"
)

# Paleta oficial de WorldCover (mismos valores de píxel que CLASES_WORLDCOVER).
COLORES_WORLDCOVER <- c(
  `10`  = "#006400",
  `20`  = "#ffbb22",
  `30`  = "#ffff4c",
  `40`  = "#f096ff",
  `50`  = "#fa0000",
  `60`  = "#b4b4b4",
  `70`  = "#f0f0f0",
  `80`  = "#0064c8",
  `90`  = "#0096a0",
  `95`  = "#00cf75",
  `100` = "#fae6a0"
)

# Raster WorldCover recortado a un bbox WGS84 c(oeste, sur, este, norte).
recortar_worldcover <- function(archivos, bbox) {
  capa <- if (length(archivos) > 1) {
    terra::vrt(archivos)
  } else {
    terra::rast(archivos)
  }
  terra::crop(
    capa,
    terra::ext(bbox["oeste"], bbox["este"], bbox["sur"], bbox["norte"])
  )
}

# Fondo de cobertura para el mapa animado: imagen RGBA pre-renderizada en
# CRTM05 (una capa de celdas por cuadro haría lentísimo el render de ~300
# cuadros de gganimate; annotation_raster dibuja un bitmap y es barato).
# Se agrega a ~40 m (modal) — suficiente para un lienzo de 800 px — y se
# atenúa con transparencia para no competir con los puntos de detección.
fondo_cobertura_animacion <- function(archivos, bbox, alfa = 0.5) {
  recorte <- recortar_worldcover(archivos, bbox) |>
    terra::aggregate(fact = 4, fun = "modal", na.rm = TRUE) |>
    terra::project(CRS_CRTM05, method = "near")
  celdas <- terra::as.matrix(recorte, wide = TRUE)
  colores <- grDevices::adjustcolor(
    COLORES_WORLDCOVER[as.character(celdas)], alpha.f = alfa
  )
  colores[is.na(celdas)] <- "#00000000"
  extension <- terra::ext(recorte)
  list(
    imagen = grDevices::as.raster(matrix(colores, nrow = nrow(celdas))),
    xmin = extension$xmin, xmax = extension$xmax,
    ymin = extension$ymin, ymax = extension$ymax
  )
}

# Nombres de las teselas de 3x3 grados que intersecan un bbox WGS84
# c(oeste, sur, este, norte) — el formato de bbox_con_buffer()
# (esquina suroeste, p. ej. "N09W087").
teselas_worldcover <- function(bbox) {
  lons <- seq(floor(bbox["oeste"] / 3) * 3, floor(bbox["este"] / 3) * 3, by = 3)
  lats <- seq(floor(bbox["sur"] / 3) * 3, floor(bbox["norte"] / 3) * 3, by = 3)
  rejilla <- expand.grid(lon = lons, lat = lats)
  sprintf(
    "%s%02d%s%03d",
    ifelse(rejilla$lat < 0, "S", "N"), abs(rejilla$lat),
    ifelse(rejilla$lon < 0, "W", "E"), abs(rejilla$lon)
  )
}

# Descarga cacheada de las teselas WorldCover que cubren el bbox.
# Retorna los archivos locales.
descargar_worldcover <- function(bbox_wgs84, dir_destino = "data/raw/worldcover") {
  vapply(teselas_worldcover(bbox_wgs84), function(tesela) {
    nombre <- glue::glue("ESA_WorldCover_10m_2021_v200_{tesela}_Map.tif")
    download_if_missing(
      glue::glue("{WORLDCOVER_BASE}/{nombre}"),
      file.path(dir_destino, nombre)
    )
  }, character(1), USE.NAMES = FALSE)
}

# Footprint elíptico de cada detección: ejes scan (E-O) x track (N-S) en km,
# construido en CRTM05 (métrico) escalando un círculo unitario.
footprints_detecciones <- function(puntos) {
  centros <- sf::st_transform(puntos, CRS_CRTM05)
  geoms <- sf::st_geometry(centros)
  circulos <- sf::st_buffer(geoms, dist = 1)  # radio 1 m, se escala por fila
  elipses <- mapply(function(circulo, centro, scan_km, track_km) {
    (circulo - centro) * diag(c(scan_km, track_km) * 1000 / 2) + centro
  }, circulos, geoms, centros$scan, centros$track, SIMPLIFY = FALSE)
  sf::st_set_geometry(
    centros,
    sf::st_sfc(elipses, crs = sf::st_crs(centros))
  )
}

# Fracción de cada clase de cobertura dentro del footprint de cada detección
# y clase dominante. Retorna el data frame de detecciones (sin geometría) con
# las columnas nuevas: una fila por detección y clase presente en su footprint.
extraer_cobertura <- function(puntos, archivos_worldcover) {
  capa <- if (length(archivos_worldcover) > 1) {
    terra::vrt(archivos_worldcover)
  } else {
    terra::rast(archivos_worldcover)
  }
  elipses <- footprints_detecciones(puntos) |>
    sf::st_transform(sf::st_crs(capa))

  fracciones <- exactextractr::exact_extract(
    capa, elipses, fun = "frac", progress = FALSE
  )
  # exact_extract("frac") retorna una columna frac_<valor> por clase presente
  cobertura <- fracciones |>
    dplyr::mutate(
      id_deteccion = dplyr::row_number(),
      acq_date = puntos$acq_date,
      frp = puntos$frp
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("frac_"),
      names_to = "clase_valor", names_prefix = "frac_",
      values_to = "fraccion"
    ) |>
    dplyr::filter(fraccion > 0) |>
    dplyr::mutate(clase = CLASES_WORLDCOVER[clase_valor])
  cobertura
}

# Clase dominante (mayor fracción del footprint) por detección.
clase_dominante <- function(cobertura) {
  cobertura |>
    dplyr::slice_max(fraccion, n = 1, by = id_deteccion, with_ties = FALSE) |>
    dplyr::select(id_deteccion, acq_date, frp, clase, fraccion)
}

# Resumen por clase: detecciones donde la clase domina el footprint y
# fracción promedio del footprint que ocupa (sobre todas las detecciones).
resumen_cobertura <- function(cobertura) {
  dominantes <- clase_dominante(cobertura) |>
    dplyr::count(clase, name = "detecciones_dominante")
  n_detecciones <- dplyr::n_distinct(cobertura$id_deteccion)
  cobertura |>
    dplyr::summarise(
      fraccion_promedio = sum(fraccion) / n_detecciones,
      .by = clase
    ) |>
    dplyr::left_join(dominantes, by = "clase") |>
    dplyr::mutate(
      detecciones_dominante = dplyr::coalesce(detecciones_dominante, 0L)
    ) |>
    dplyr::arrange(dplyr::desc(fraccion_promedio))
}

# Tabla CSV del resumen por clase (target con format = "file").
tabla_cobertura_csv <- function(cobertura, dest) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  resumen_cobertura(cobertura) |>
    dplyr::mutate(fraccion_promedio = round(fraccion_promedio, 3)) |>
    readr::write_csv(dest)
  dest
}

# Gráfico de barras: detecciones por clase de cobertura dominante.
crear_grafico_cobertura <- function(cobertura, interactivo = FALSE) {
  datos <- resumen_cobertura(cobertura) |>
    dplyr::filter(detecciones_dominante > 0) |>
    dplyr::mutate(
      clase = stats::reorder(clase, detecciones_dominante),
      etiqueta = paste0(
        "Clase: ", clase,
        "<br>Detecciones: ", detecciones_dominante,
        "<br>Fracción promedio del footprint: ",
        round(fraccion_promedio * 100), "%"
      )
    )
  p <- ggplot2::ggplot(datos, ggplot2::aes(x = detecciones_dominante, y = clase,
                                           text = etiqueta)) +
    ggplot2::geom_col(fill = COLOR_DETECCIONES, width = 0.7) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(x = "Detecciones", y = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "grey92")
    )
  if (interactivo) {
    plotly::ggplotly(p, tooltip = "text") |>
      configurar_plotly(
        "Detecciones por cobertura de la tierra",
        "Clase dominante en el footprint de cada detección — WorldCover 2021"
      )
  } else {
    p + ggplot2::labs(
      title = "Detecciones por cobertura de la tierra",
      subtitle = "Clase dominante en el footprint de cada detección — WorldCover 2021",
      caption = "Datos: NASA FIRMS (MODIS_SP) y ESA WorldCover 2021"
    ) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  }
}

# PNG del gráfico de cobertura (target con format = "file").
grafico_cobertura <- function(cobertura, dest) {
  p <- crear_grafico_cobertura(cobertura, interactivo = FALSE)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(dest, p, width = 8, height = 4.5, dpi = 200)
  dest
}
