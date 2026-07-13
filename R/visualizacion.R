# Visualizaciones: mapa animado (GIF/MP4), mapa interactivo con deslizador
# temporal y gráficos estadísticos.

# Color único para la serie de detecciones (naranja quemado) y rampa
# secuencial perceptualmente uniforme (inferno) para la magnitud FRP.
COLOR_DETECCIONES <- "#bf5b17"

# Mapa animado de detecciones por mes sobre el polígono del parque.
# `mensual` (con meses en 0 incluidos) define la secuencia completa de cuadros;
# los meses sin detecciones se representan con un cuadro vacío mediante puntos
# fantasma invisibles, para que el tiempo avance a ritmo constante.
# El formato de salida se infiere de la extensión de `dest` (.gif o .mp4).
animar_detecciones <- function(puntos, parque, mensual, dest, fps = 4) {
  etiquetas <- format(mensual$aniomes, "%Y-%m")
  puntos <- puntos |>
    dplyr::mutate(cuadro = factor(format(aniomes, "%Y-%m"), levels = etiquetas))

  centro <- sf::st_centroid(sf::st_geometry(sf::st_union(parque)))
  fantasma <- sf::st_sf(
    cuadro = factor(etiquetas, levels = etiquetas),
    frp = NA_real_,
    geometry = rep(centro, length(etiquetas))
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = parque, fill = "grey95", color = "grey40",
                     linewidth = 0.4) +
    ggplot2::geom_sf(data = fantasma, alpha = 0, show.legend = FALSE) +
    ggplot2::geom_sf(data = puntos, ggplot2::aes(color = frp),
                     size = 2, alpha = 0.8) +
    ggplot2::scale_color_viridis_c(option = "inferno", direction = -1,
                                   trans = "sqrt", na.value = "transparent",
                                   name = "FRP (MW)") +
    ggplot2::labs(
      title = "Anomalías térmicas en el Parque Nacional Palo Verde",
      subtitle = "Detecciones MODIS (FIRMS) — mes: {current_frame}",
      caption = "Datos: NASA FIRMS (MODIS_SP) y SINAC"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_line(color = "grey92", linewidth = 0.3),
      plot.title = ggplot2::element_text(face = "bold")
    ) +
    gganimate::transition_manual(cuadro)

  renderizador <- if (grepl("\\.mp4$", dest)) {
    gganimate::av_renderer(dest)
  } else {
    gganimate::gifski_renderer(dest)
  }
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  gganimate::animate(
    p, renderer = renderizador,
    nframes = length(etiquetas), fps = fps,
    width = 800, height = 650, res = 96
  )
  dest
}

# Mapa interactivo leaflet con deslizador de tiempo (leaflet.extras2).
# Retorna el widget (para incrustar en el reporte Quarto).
crear_mapa_temporal <- function(puntos, parque) {
  puntos_wgs84 <- sf::st_transform(puntos, CRS_WGS84) |>
    dplyr::mutate(time = as.POSIXct(acq_date, tz = "UTC")) |>
    dplyr::arrange(time)
  parque_wgs84 <- sf::st_transform(parque, CRS_WGS84)

  m <- leaflet::leaflet() |>
    leaflet::addProviderTiles("CartoDB.Positron", group = "CartoDB") |>
    leaflet::addProviderTiles("Esri.WorldImagery", group = "Imágenes satelitales") |>
    leaflet::addPolygons(
      data = parque_wgs84, fill = FALSE, color = "#2b5876", weight = 2,
      label = "Parque Nacional Palo Verde"
    ) |>
    leaflet.extras2::addTimeslider(
      data = puntos_wgs84,
      radius = 6, color = COLOR_DETECCIONES, stroke = FALSE, fillOpacity = 0.8,
      popupOptions = leaflet::popupOptions(maxWidth = 300),
      popup = ~paste0(
        "<strong>Fecha:</strong> ", acq_date,
        "<br><strong>Hora (UTC):</strong> ", sprintf("%04d", acq_time),
        "<br><strong>FRP (MW):</strong> ", frp,
        "<br><strong>Confianza:</strong> ", confidence,
        "<br><strong>Satélite:</strong> ", satellite
      ),
      options = leaflet.extras2::timesliderOptions(
        position = "bottomleft",
        timeAttribute = "time",
        range = TRUE,
        alwaysShowDate = TRUE
      )
    ) |>
    leaflet::addLayersControl(
      baseGroups = c("CartoDB", "Imágenes satelitales"),
      position = "topright"
    )
  m
}

# Guarda el mapa temporal como HTML autocontenido (target con format = "file").
mapa_leaflet_temporal <- function(puntos, parque, dest) {
  m <- crear_mapa_temporal(puntos, parque)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  htmlwidgets::saveWidget(m, file.path(normalizePath(dirname(dest)), basename(dest)),
                          selfcontained = TRUE)
  dest
}

# Serie temporal mensual de detecciones (una serie: sin leyenda, un solo tono).
grafico_serie_temporal <- function(mensual, dest) {
  p <- ggplot2::ggplot(mensual, ggplot2::aes(x = aniomes, y = detecciones)) +
    ggplot2::geom_col(fill = COLOR_DETECCIONES, width = 25) +
    ggplot2::scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(
      title = "Detecciones mensuales de anomalías térmicas",
      subtitle = "Parque Nacional Palo Verde — MODIS (FIRMS)",
      x = NULL, y = "Detecciones por mes",
      caption = "Datos: NASA FIRMS (MODIS_SP)"
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

# Climatología mensual: promedio de detecciones por mes calendario
# (estacionalidad de la época seca).
grafico_climatologia <- function(mensual, dest) {
  meses_es <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                "Jul", "Ago", "Set", "Oct", "Nov", "Dic")
  clima <- mensual |>
    dplyr::summarise(promedio = mean(detecciones), .by = mes) |>
    dplyr::mutate(nombre_mes = factor(meses_es[mes], levels = meses_es))
  p <- ggplot2::ggplot(clima, ggplot2::aes(x = nombre_mes, y = promedio)) +
    ggplot2::geom_col(fill = COLOR_DETECCIONES, width = 0.7) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(
      title = "Climatología mensual de anomalías térmicas",
      subtitle = "Promedio de detecciones por mes calendario — PN Palo Verde, MODIS (FIRMS)",
      x = NULL, y = "Detecciones promedio",
      caption = "Datos: NASA FIRMS (MODIS_SP)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "grey92"),
      plot.title = ggplot2::element_text(face = "bold")
    )
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(dest, p, width = 8, height = 4.5, dpi = 200)
  dest
}
