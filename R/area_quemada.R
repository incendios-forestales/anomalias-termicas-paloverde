# Procesamiento del área quemada MCD64A1: lectura de la banda "Burn Date",
# recorte al parque, serie mensual de hectáreas y visualizaciones.
#
# Cada granulo mensual etiqueta únicamente las quemas detectadas en su propio
# mes: el valor del píxel es el día del año (del año del granulo) en que el
# algoritmo sitúa la quema, a partir del cambio persistente de reflectancia.
# El píxel de 500 m (~21.5 ha) es la unidad mínima: una quema menor que el
# píxel puede no detectarse, y la superficie reportada es la del píxel
# completo, no la de la cicatriz interna. Por eso las hectáreas mensuales son
# una estimación gruesa, complementaria (y NUNCA sumable) al conteo de
# detecciones de fuego activo.

# Color de la serie y capa de área quemada (morado: distinto del naranja de
# las detecciones y de los tonos verdes/celestes de las capas de contexto).
COLOR_AREA_QUEMADA <- "#54278f"

FUENTE_MCD64A1 <- "Datos: NASA LP DAAC (MCD64A1 v6.1)"

# Abre la banda "Burn Date" de un HDF4-EOS2 de MCD64A1 como SpatRaster
# (CRS sinusoidal MODIS). Se resuelve por nombre de variable con
# terra::describe(sds = TRUE) en lugar de armar la cadena GDAL
# "HDF4_EOS:EOS_GRID:..." a mano (el nombre del grid interno es un detalle
# del formato) o de usar names(terra::sds()), que para estos HDF devuelve el
# nombre del archivo, no el de la banda. En `var` los nombres con espacios
# conservan las comillas del HDF ('"Burn Date"'); se comparan sin ellas.
leer_burn_date <- function(path_hdf) {
  if (!"HDF4" %in% terra::gdal(drivers = TRUE)$name) {
    stop("El GDAL disponible no tiene el driver HDF4, necesario para leer ",
         "MCD64A1. Ejecute el pipeline en el contenedor del proyecto ",
         "(rocker/geospatial lo incluye).", call. = FALSE)
  }
  subdatasets <- terra::describe(path_hdf, sds = TRUE)
  indice <- which(gsub('"', "", subdatasets$var) == "Burn Date")
  if (length(indice) != 1) {
    stop("No se encontró el subdataset 'Burn Date' en ", basename(path_hdf),
         " (disponibles: ", paste(subdatasets$var, collapse = ", "), ").",
         call. = FALSE)
  }
  terra::rast(subdatasets$name[indice])
}

# Extrae los píxeles quemados de todos los granulos dentro del parque.
# Recibe el vector completo de paths (como leer_y_unir_csv con los CSV):
# recorrer ~300 HDF con recorte al parque toma minutos y no amerita una rama
# de procesamiento por granulo, casi todas vacías.
#
# Decisiones:
#   - El recorte usa el criterio centro-de-píxel (mask de terra), coherente
#     con el recorte estricto por punto de las detecciones.
#   - area_ha se calcula con cellSize() EN LA PROYECCIÓN SINUSOIDAL (de igual
#     área, ~21.5 ha/píxel) antes de reproyectar: así la suma mensual no
#     depende de distorsiones de reproyección.
#   - Se retornan polígonos de píxel (no centroides): a 500 m el píxel es un
#     objeto de área real y el polígono comunica eso honestamente en el mapa.
#
# Retorna sf de polígonos de píxel en CRTM05 con columnas
# fecha, anio, mes, aniomes, area_ha.
extraer_quemas <- function(paths_hdf, parque) {
  raster_inicial <- leer_burn_date(paths_hdf[[1]])
  parque_sin <- terra::vect(sf::st_transform(parque, terra::crs(raster_inicial)))
  ext_r <- terra::ext(raster_inicial)
  ext_p <- terra::ext(parque_sin)
  if (ext_p$xmin > ext_r$xmax || ext_p$xmax < ext_r$xmin ||
      ext_p$ymin > ext_r$ymax || ext_p$ymax < ext_r$ymin) {
    stop("El granulo ", basename(paths_hdf[[1]]), " no cubre el parque: ",
         "revise TESELA_MCD64A1 en R/constantes.R.", call. = FALSE)
  }

  quemas <- purrr::map(paths_hdf, function(path) {
    # El nombre del granulo (MCD64A1.A2020306...) trae el año y día juliano
    # del primer día del mes que cubre.
    fecha_granulo <- as.Date(sub(".*\\.A(\\d{7})\\..*", "\\1", basename(path)),
                             format = "%Y%j")
    r <- terra::crop(leer_burn_date(path), parque_sin, mask = TRUE)
    r[r <= 0] <- NA
    if (all(is.na(terra::values(r)))) return(NULL)
    names(r) <- "dia"
    tamano <- terra::cellSize(r, unit = "ha")
    names(tamano) <- "area_ha"

    pixeles <- terra::as.polygons(c(r, tamano), aggregate = FALSE,
                                  na.rm = TRUE) |>
      sf::st_as_sf() |>
      dplyr::mutate(
        fecha = as.Date(glue::glue("{lubridate::year(fecha_granulo)}-01-01")) +
          dia - 1
      )
    fuera_de_mes <- lubridate::floor_date(pixeles$fecha, "month") != fecha_granulo
    if (any(fuera_de_mes)) {
      warning("Granulo ", basename(path), ": ", sum(fuera_de_mes),
              " píxel(es) con fecha de quema fuera de su mes.", call. = FALSE)
    }
    dplyr::select(pixeles, fecha, area_ha)
  }) |>
    purrr::compact()

  if (length(quemas) == 0) {
    return(sf::st_sf(
      fecha = as.Date(character()), anio = integer(), mes = integer(),
      aniomes = as.Date(character()), area_ha = numeric(),
      geometry = sf::st_sfc(crs = CRS_CRTM05)
    ))
  }

  do.call(rbind, quemas) |>
    a_crtm05() |>
    dplyr::mutate(
      anio    = lubridate::year(fecha),
      mes     = lubridate::month(fecha),
      aniomes = lubridate::floor_date(fecha, "month")
    ) |>
    dplyr::select(fecha, anio, mes, aniomes, area_ha)
}

# Serie mensual de hectáreas quemadas, con meses en 0 completados (mismo
# patrón que agregar_mensual). `rango_meses` extiende el eje temporal para
# alinearlo con la serie de detecciones en los gráficos comparativos; los
# meses con quemas fuera de ese rango no se descartan.
agregar_mensual_quemas <- function(quemas, rango_meses = NULL) {
  mensual <- quemas |>
    sf::st_drop_geometry() |>
    dplyr::summarise(hectareas = sum(area_ha), .by = aniomes)
  limites <- range(c(mensual$aniomes,
                     lubridate::floor_date(as.Date(rango_meses), "month")))
  data.frame(aniomes = seq(limites[1], limites[2], by = "month")) |>
    dplyr::left_join(mensual, by = "aniomes") |>
    dplyr::mutate(
      hectareas = tidyr::replace_na(hectareas, 0),
      anio = lubridate::year(aniomes),
      mes  = lubridate::month(aniomes)
    )
}

# --- Visualizaciones (mismo precedente que cobertura.R: los gráficos del
#     módulo viven en el módulo; reutilizan configurar_plotly y MESES_ES) ----

# Serie mensual interactiva de hectáreas quemadas (widget plotly).
crear_serie_area_quemada <- function(quemas_mensual) {
  datos <- quemas_mensual |>
    dplyr::mutate(etiqueta = paste0(
      "Mes: ", MESES_ES[mes], " ", format(aniomes, "%Y"),
      "<br>Área quemada: ", round(hectareas), " ha"
    ))
  p <- ggplot2::ggplot(datos, ggplot2::aes(x = aniomes, y = hectareas,
                                           text = etiqueta)) +
    ggplot2::geom_col(fill = COLOR_AREA_QUEMADA, width = 25) +
    ggplot2::scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(x = NULL, y = "Hectáreas quemadas por mes") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "grey92")
    )
  plotly::ggplotly(p, tooltip = "text") |>
    configurar_plotly(
      "Área quemada mensual",
      "Parque Nacional Palo Verde — MCD64A1 (píxeles de 500 m)",
      fuente = FUENTE_MCD64A1
    )
}

# Comparación fuego activo vs. área quemada: dos paneles apilados con eje x
# compartido. NUNCA un doble eje y ni barras sumadas: son magnitudes distintas
# (conteo de detecciones vs. superficie) y el doble eje invita a lecturas
# espurias de proporcionalidad.
crear_comparacion_series <- function(firms_mensual, quemas_mensual) {
  tema <- ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "grey92")
    )
  datos_firms <- firms_mensual |>
    dplyr::mutate(etiqueta = paste0(
      "Mes: ", MESES_ES[mes], " ", format(aniomes, "%Y"),
      "<br>Detecciones: ", detecciones
    ))
  datos_quemas <- quemas_mensual |>
    dplyr::mutate(etiqueta = paste0(
      "Mes: ", MESES_ES[mes], " ", format(aniomes, "%Y"),
      "<br>Área quemada: ", round(hectareas), " ha"
    ))
  p_firms <- ggplot2::ggplot(datos_firms,
                             ggplot2::aes(x = aniomes, y = detecciones,
                                          text = etiqueta)) +
    ggplot2::geom_col(fill = COLOR_DETECCIONES, width = 25) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(x = NULL, y = "Detecciones") +
    tema
  p_quemas <- ggplot2::ggplot(datos_quemas,
                              ggplot2::aes(x = aniomes, y = hectareas,
                                           text = etiqueta)) +
    ggplot2::geom_col(fill = COLOR_AREA_QUEMADA, width = 25) +
    ggplot2::scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(x = NULL, y = "Hectáreas quemadas") +
    tema
  plotly::subplot(
    plotly::ggplotly(p_firms, tooltip = "text"),
    plotly::ggplotly(p_quemas, tooltip = "text"),
    nrows = 2, shareX = TRUE, titleY = TRUE, margin = 0.05
  ) |>
    configurar_plotly(
      "Fuego activo y área quemada",
      "PN Palo Verde — detecciones FIRMS (arriba) y hectáreas MCD64A1 (abajo)",
      fuente = "Datos: NASA FIRMS (MODIS_SP) y LP DAAC (MCD64A1)"
    )
}

# Climatología comparada: promedios por mes calendario de detecciones (arriba)
# y hectáreas quemadas (abajo), con el eje de meses compartido. Mismos niveles
# de factor en ambos paneles: shareX exige ejes idénticos.
crear_climatologia_comparada <- function(firms_mensual, quemas_mensual) {
  tema <- ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "grey92")
    )
  clima_firms <- firms_mensual |>
    dplyr::summarise(promedio = mean(detecciones), .by = mes) |>
    dplyr::mutate(
      nombre_mes = factor(MESES_ES[mes], levels = MESES_ES),
      etiqueta = paste0("Mes: ", nombre_mes,
                        "<br>Promedio: ", num_es(promedio), " detecciones")
    )
  clima_quemas <- quemas_mensual |>
    dplyr::summarise(promedio = mean(hectareas), .by = mes) |>
    dplyr::mutate(
      nombre_mes = factor(MESES_ES[mes], levels = MESES_ES),
      etiqueta = paste0("Mes: ", nombre_mes,
                        "<br>Promedio: ", num_es(promedio, 0), " ha")
    )
  p_firms <- ggplot2::ggplot(clima_firms,
                             ggplot2::aes(x = nombre_mes, y = promedio,
                                          text = etiqueta)) +
    ggplot2::geom_col(fill = COLOR_DETECCIONES, width = 0.7) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(x = NULL, y = "Detecciones promedio") +
    tema
  p_quemas <- ggplot2::ggplot(clima_quemas,
                              ggplot2::aes(x = nombre_mes, y = promedio,
                                           text = etiqueta)) +
    ggplot2::geom_col(fill = COLOR_AREA_QUEMADA, width = 0.7) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(x = NULL, y = "Hectáreas promedio") +
    tema
  plotly::subplot(
    plotly::ggplotly(p_firms, tooltip = "text"),
    plotly::ggplotly(p_quemas, tooltip = "text"),
    nrows = 2, shareX = TRUE, titleY = TRUE, margin = 0.05
  ) |>
    configurar_plotly(
      "Climatología mensual: fuego activo y área quemada",
      "Promedios por mes calendario — detecciones FIRMS (arriba) y hectáreas MCD64A1 (abajo)",
      fuente = "Datos: NASA FIRMS (MODIS_SP) y LP DAAC (MCD64A1)"
    )
}

# Versiones PNG (targets con format = "file", patrón dest -> dest).
grafico_area_quemada <- function(quemas_mensual, dest) {
  p <- ggplot2::ggplot(quemas_mensual,
                       ggplot2::aes(x = aniomes, y = hectareas)) +
    ggplot2::geom_col(fill = COLOR_AREA_QUEMADA, width = 25) +
    ggplot2::scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(
      title = "Área quemada mensual",
      subtitle = "Parque Nacional Palo Verde — MCD64A1 (píxeles de 500 m)",
      x = NULL, y = "Hectáreas quemadas por mes",
      caption = FUENTE_MCD64A1
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "grey92"),
      plot.title = ggplot2::element_text(face = "bold")
    )
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(dest, p, width = 10, height = 4.5, dpi = 200)
  dest
}

grafico_climatologia_comparada <- function(firms_mensual, quemas_mensual, dest) {
  niveles <- c("Detecciones promedio (FIRMS)",
               "Hectáreas quemadas promedio (MCD64A1)")
  datos <- dplyr::bind_rows(
    firms_mensual |>
      dplyr::summarise(valor = mean(detecciones), .by = mes) |>
      dplyr::mutate(serie = niveles[[1]]),
    quemas_mensual |>
      dplyr::summarise(valor = mean(hectareas), .by = mes) |>
      dplyr::mutate(serie = niveles[[2]])
  ) |>
    dplyr::mutate(
      nombre_mes = factor(MESES_ES[mes], levels = MESES_ES),
      serie = factor(serie, levels = niveles)
    )
  p <- ggplot2::ggplot(datos, ggplot2::aes(x = nombre_mes, y = valor,
                                           fill = serie)) +
    ggplot2::geom_col(width = 0.7, show.legend = FALSE) +
    ggplot2::facet_wrap(~serie, ncol = 1, scales = "free_y") +
    ggplot2::scale_fill_manual(
      values = stats::setNames(c(COLOR_DETECCIONES, COLOR_AREA_QUEMADA),
                               niveles)
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(
      title = "Climatología mensual: fuego activo y área quemada",
      subtitle = "Promedios por mes calendario — PN Palo Verde",
      x = NULL, y = NULL,
      caption = "Datos: NASA FIRMS (MODIS_SP) y LP DAAC (MCD64A1)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "grey92"),
      plot.title = ggplot2::element_text(face = "bold"),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0)
    )
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(dest, p, width = 8, height = 7, dpi = 200)
  dest
}

grafico_comparacion <- function(firms_mensual, quemas_mensual, dest) {
  niveles <- c("Detecciones de fuego activo (FIRMS)",
               "Área quemada en hectáreas (MCD64A1)")
  datos <- dplyr::bind_rows(
    firms_mensual |>
      dplyr::transmute(aniomes, valor = detecciones, serie = niveles[[1]]),
    quemas_mensual |>
      dplyr::transmute(aniomes, valor = hectareas, serie = niveles[[2]])
  ) |>
    dplyr::mutate(serie = factor(serie, levels = niveles))
  p <- ggplot2::ggplot(datos, ggplot2::aes(x = aniomes, y = valor,
                                           fill = serie)) +
    ggplot2::geom_col(width = 25, show.legend = FALSE) +
    ggplot2::facet_wrap(~serie, ncol = 1, scales = "free_y") +
    ggplot2::scale_fill_manual(
      values = stats::setNames(c(COLOR_DETECCIONES, COLOR_AREA_QUEMADA),
                               niveles)
    ) +
    ggplot2::scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(
      title = "Fuego activo y área quemada",
      subtitle = "Parque Nacional Palo Verde — series complementarias, no sumables",
      x = NULL, y = NULL,
      caption = "Datos: NASA FIRMS (MODIS_SP) y LP DAAC (MCD64A1)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "grey92"),
      plot.title = ggplot2::element_text(face = "bold"),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0)
    )
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(dest, p, width = 10, height = 7, dpi = 200)
  dest
}
