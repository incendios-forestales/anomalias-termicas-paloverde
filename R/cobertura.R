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

# --- Contraste con las capas nacionales (SIREFOR) ---------------------------
# WorldCover clasifica como "Pastizal" buena parte de la marisma de Palo Verde.
# Se contrasta cada footprint contra el Registro Nacional de Humedales para
# verificar si esas detecciones ocurren en humedal registrado.
#
# Criterio: mismo footprint elíptico del análisis WorldCover (no el punto), y
# se considera "en humedal" si el footprint interseca algún polígono del
# registro; se reporta además la fracción del footprint cubierta por humedal.
#
# Limitación: las capas nacionales también son fotos fijas (cobertura forestal
# 2023; registro de humedales sin fecha uniforme) frente a 2001-2026.
cruzar_con_humedales <- function(puntos, humedales) {
  elipses <- footprints_detecciones(puntos)
  humedales <- a_crtm05(humedales)

  interseccion <- sf::st_intersection(
    sf::st_make_valid(elipses[, c("acq_date", "frp")] |>
                        dplyr::mutate(id_deteccion = dplyr::row_number())),
    sf::st_make_valid(humedales[, c("nom_hum", "tipo_hum", "clase_hum")])
  )
  areas <- elipses |>
    sf::st_area() |>
    as.numeric()

  resumen <- interseccion |>
    dplyr::mutate(area_humedal = as.numeric(sf::st_area(interseccion))) |>
    sf::st_drop_geometry() |>
    dplyr::summarise(
      # Clase del humedal que más área aporta al footprint
      clase_hum = clase_hum[which.max(area_humedal)],
      tipo_hum  = tipo_hum[which.max(area_humedal)],
      nom_hum   = nom_hum[which.max(area_humedal)],
      area_humedal = sum(area_humedal),
      .by = id_deteccion
    ) |>
    dplyr::mutate(fraccion_humedal = pmin(area_humedal / areas[id_deteccion], 1))

  data.frame(id_deteccion = seq_len(nrow(elipses))) |>
    dplyr::left_join(resumen, by = "id_deteccion") |>
    dplyr::mutate(
      en_humedal = !is.na(clase_hum),
      fraccion_humedal = tidyr::replace_na(fraccion_humedal, 0)
    )
}

# Contraste WorldCover vs. humedales registrados: por clase dominante de
# WorldCover, cuántas detecciones caen en humedal y con qué cobertura.
contraste_humedales <- function(cobertura, humedales_detecciones) {
  clase_dominante(cobertura) |>
    dplyr::left_join(humedales_detecciones, by = "id_deteccion") |>
    dplyr::summarise(
      detecciones = dplyr::n(),
      # `pct` antes de crear la columna `en_humedal`: summarise() permite
      # referirse a columnas recién creadas y el conteo enmascararía al lógico.
      pct_en_humedal = round(100 * mean(en_humedal), 1),
      en_humedal = sum(en_humedal),
      fraccion_humedal_promedio = round(mean(fraccion_humedal), 3),
      .by = clase
    ) |>
    dplyr::relocate(en_humedal, .after = detecciones) |>
    dplyr::arrange(dplyr::desc(detecciones))
}

# Tabla CSV del contraste (target con format = "file").
tabla_contraste_csv <- function(cobertura, humedales_detecciones, dest) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  contraste_humedales(cobertura, humedales_detecciones) |>
    readr::write_csv(dest)
  dest
}

# Tabla CSV del resumen por clase (target con format = "file").
tabla_cobertura_csv <- function(cobertura, dest) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  resumen_cobertura(cobertura) |>
    dplyr::mutate(fraccion_promedio = round(fraccion_promedio, 3)) |>
    readr::write_csv(dest)
  dest
}

# Color de la condición "en humedal": el mismo teal de la capa de humedales
# del mapa interactivo, para lectura consistente entre productos. El de
# "fuera de humedal" es COLOR_DETECCIONES, referido dentro de la función
# porque tar_source() carga este archivo antes que visualizacion.R.
COLOR_EN_HUMEDAL <- "#0096a0"

# Detecciones por clase de cobertura dominante, divididas según caigan o no
# en un humedal registrado. El humedal no es un tipo de vegetación que compita
# con "pastizal" o "bosque" (las capas del SINAC no cubren todo el territorio,
# ver README): es una condición del sitio, y por eso se representa como
# partición de cada barra y no como una clase más.
conteo_cobertura_humedal <- function(cobertura, humedales_detecciones) {
  clase_dominante(cobertura) |>
    dplyr::left_join(humedales_detecciones, by = "id_deteccion") |>
    dplyr::count(clase, en_humedal, name = "detecciones") |>
    dplyr::mutate(
      condicion = factor(
        ifelse(en_humedal, "En humedal registrado", "Fuera de humedal"),
        levels = c("En humedal registrado", "Fuera de humedal")
      )
    )
}

# Gráfico de barras apiladas: detecciones por clase de cobertura dominante,
# segmentadas por condición de humedal.
crear_grafico_cobertura <- function(cobertura, humedales_detecciones,
                                    interactivo = FALSE) {
  conteos <- conteo_cobertura_humedal(cobertura, humedales_detecciones)
  orden <- conteos |>
    dplyr::summarise(total = sum(detecciones), .by = clase) |>
    dplyr::arrange(total)
  datos <- conteos |>
    dplyr::mutate(
      clase = factor(clase, levels = orden$clase),
      etiqueta = paste0(
        "Clase: ", clase,
        "<br>", condicion,
        "<br>Detecciones: ", detecciones
      )
    )
  p <- ggplot2::ggplot(datos, ggplot2::aes(x = detecciones, y = clase,
                                           fill = condicion, text = etiqueta)) +
    # El separador blanco entre segmentos evita que se lean como una sola barra.
    # reverse = TRUE: el segmento "en humedal" arranca en cero, en el mismo
    # orden en que aparece en la leyenda.
    ggplot2::geom_col(width = 0.7, linewidth = 0.7, color = "white",
                      position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::scale_fill_manual(
      values = c("En humedal registrado" = COLOR_EN_HUMEDAL,
                 "Fuera de humedal" = COLOR_DETECCIONES),
      name = NULL
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(x = "Detecciones", y = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "grey92"),
      legend.position = "top"
    )
  if (interactivo) {
    plotly::ggplotly(p, tooltip = "text") |>
      configurar_plotly(
        "Detecciones por cobertura de la tierra y condición de humedal",
        paste("Clase dominante en el footprint — WorldCover 2021;",
              "humedales: Registro Nacional (SINAC)"),
        fuente = "Datos: NASA FIRMS (MODIS_SP), ESA WorldCover 2021 y SINAC",
        # El eje x lleva rótulo ("Detecciones"), por eso la fuente baja más
        margen_superior = 130, margen_inferior = 110,
        desplazamiento_fuente = -78
      ) |>
      plotly::layout(legend = list(orientation = "h", x = 0,
                                   y = 1.02, yanchor = "bottom"))
  } else {
    p + ggplot2::labs(
      title = "Detecciones por cobertura de la tierra y condición de humedal",
      subtitle = paste("Clase dominante en el footprint — WorldCover 2021;",
                       "humedales: Registro Nacional (SINAC)"),
      caption = "Datos: NASA FIRMS (MODIS_SP), ESA WorldCover 2021 y SINAC"
    ) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  }
}

# PNG del gráfico de cobertura (target con format = "file").
grafico_cobertura <- function(cobertura, humedales_detecciones, dest) {
  p <- crear_grafico_cobertura(cobertura, humedales_detecciones,
                               interactivo = FALSE)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(dest, p, width = 8, height = 4.5, dpi = 200)
  dest
}
