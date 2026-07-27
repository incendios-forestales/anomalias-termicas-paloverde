# Visualizaciones: mapa animado (GIF/MP4), mapa interactivo con deslizador
# temporal y gráficos estadísticos.

# Color único para la serie de detecciones (naranja quemado) y rampa
# secuencial perceptualmente uniforme (inferno) para la magnitud FRP.
COLOR_DETECCIONES <- "#bf5b17"

# Nombres de mes abreviados en español (independientes del locale del sistema).
MESES_ES <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
              "Jul", "Ago", "Set", "Oct", "Nov", "Dic")

# Mapa animado de detecciones por mes sobre el polígono del parque.
# `mensual` (con meses en 0 incluidos) define la secuencia completa de cuadros;
# los meses sin detecciones se representan con un cuadro vacío mediante puntos
# fantasma invisibles, para que el tiempo avance a ritmo constante.
# El formato de salida se infiere de la extensión de `dest` (.gif o .mp4).
animar_detecciones <- function(puntos, parque, mensual, archivos_worldcover,
                               bbox, dest, fps = 4) {
  etiquetas <- format(mensual$aniomes, "%Y-%m")
  puntos <- puntos |>
    dplyr::mutate(cuadro = factor(format(aniomes, "%Y-%m"), levels = etiquetas))

  centro <- sf::st_centroid(sf::st_geometry(sf::st_union(parque)))
  fantasma <- sf::st_sf(
    cuadro = factor(etiquetas, levels = etiquetas),
    frp = NA_real_,
    geometry = rep(centro, length(etiquetas))
  )

  fondo <- fondo_cobertura_animacion(archivos_worldcover, bbox)
  p <- ggplot2::ggplot() +
    ggplot2::annotation_raster(fondo$imagen, xmin = fondo$xmin, xmax = fondo$xmax,
                               ymin = fondo$ymin, ymax = fondo$ymax) +
    ggplot2::geom_sf(data = parque, fill = NA, color = "grey30",
                     linewidth = 0.5) +
    ggplot2::geom_sf(data = fantasma, alpha = 0, show.legend = FALSE) +
    # shape 21 con borde oscuro: sin él, las detecciones de FRP bajo (extremo
    # claro de inferno) se camuflan contra el pastizal amarillo del fondo.
    ggplot2::geom_sf(data = puntos, ggplot2::aes(fill = frp),
                     shape = 21, size = 2.4, stroke = 0.3,
                     color = "grey15", alpha = 0.9) +
    ggplot2::scale_fill_viridis_c(option = "inferno", direction = -1,
                                  trans = "sqrt", na.value = "transparent",
                                  name = "FRP (MW)") +
    ggplot2::labs(
      title = "Anomalías térmicas en el Parque Nacional Palo Verde",
      subtitle = "Detecciones MODIS (FIRMS) — mes: {current_frame}",
      caption = "Datos: NASA FIRMS (MODIS_SP) y SINAC. Fondo: ESA WorldCover 2021"
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
#
# El plugin del deslizador (leaflet.SliderControl) tiene tres limitaciones que
# se corrigen aquí sin parchar el paquete:
#   1. En modo rango omite el rótulo de fecha inicial (solo lo pinta al arrastrar).
#   2. En modo rango borra el rótulo en CADA mouseup del documento.
#   3. El rótulo se dibuja debajo de la barra, por lo que en "bottomleft" queda
#      recortado por el borde del mapa (por eso el control va en "topright").
# 1 y 2 se resuelven con onRender: se repinta el rótulo con el rango de fechas
# seleccionado (los índices del deslizador corresponden al orden de los puntos).
# `time` se pasa como fecha ISO en texto: es lo que el plugin muestra al arrastrar.
crear_mapa_temporal <- function(puntos, parque, cobertura, archivos_worldcover,
                                bbox) {
  # Clase de cobertura dominante por detección (id_deteccion es el número de
  # fila de `puntos` en el orden en que extraer_cobertura() los recibió, por
  # lo que el join debe hacerse ANTES de reordenar por fecha).
  puntos_wgs84 <- sf::st_transform(puntos, CRS_WGS84) |>
    dplyr::mutate(fila = dplyr::row_number()) |>
    dplyr::left_join(
      clase_dominante(cobertura) |>
        dplyr::select(id_deteccion, clase_cobertura = clase,
                      fraccion_cobertura = fraccion),
      by = c("fila" = "id_deteccion")
    ) |>
    dplyr::arrange(acq_date) |>
    dplyr::mutate(time = as.character(acq_date))
  parque_wgs84 <- sf::st_transform(parque, CRS_WGS84)

  # Capa de cobertura: raster categórico (method = "ngb" para no interpolar
  # entre códigos de clase), oculta al inicio y conmutable desde el control.
  recorte <- recortar_worldcover(archivos_worldcover, bbox)
  valores <- sort(terra::unique(recorte)[[1]])
  paleta <- leaflet::colorFactor(
    unname(COLORES_WORLDCOVER[as.character(valores)]),
    levels = valores, na.color = "transparent"
  )
  GRUPO_COBERTURA <- "Cobertura (WorldCover 2021)"

  m <- leaflet::leaflet() |>
    leaflet::addProviderTiles("CartoDB.Positron", group = "CartoDB") |>
    leaflet::addProviderTiles("Esri.WorldImagery", group = "Imágenes satelitales") |>
    leaflet::addRasterImage(
      recorte, colors = paleta, opacity = 0.7, method = "ngb",
      group = GRUPO_COBERTURA, maxBytes = 10 * 1024^2
    ) |>
    leaflet::addLegend(
      colors = unname(COLORES_WORLDCOVER[as.character(valores)]),
      labels = unname(CLASES_WORLDCOVER[as.character(valores)]),
      title = "Cobertura (2021)", position = "bottomright",
      group = GRUPO_COBERTURA
    ) |>
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
        "<br><strong>Satélite:</strong> ", satellite,
        "<br><strong>Cobertura dominante (2021):</strong> ", clase_cobertura,
        " (", round(fraccion_cobertura * 100), " %)"
      ),
      options = leaflet.extras2::timesliderOptions(
        position = "topright",
        timeAttribute = "time",
        range = TRUE,
        alwaysShowDate = TRUE,
        showAllOnStart = TRUE
      )
    ) |>
    leaflet::addLayersControl(
      baseGroups = c("CartoDB", "Imágenes satelitales"),
      overlayGroups = GRUPO_COBERTURA,
      position = "topright"
    ) |>
    leaflet::hideGroup(GRUPO_COBERTURA) |>
    htmlwidgets::onRender(
      "function(el, x, data) {
        var fechas = data.fechas;
        // El plugin fija el máximo del deslizador en la cantidad de fechas
        // ÚNICAS, pero usa los valores como índices de TODOS los puntos: con
        // fechas repetidas el tirador superior queda recortado. Restaurar el
        // máximo real y el rango completo.
        $('#leaflet-slider').slider('option', 'max', fechas.length - 1);
        $('#leaflet-slider').slider('values', [0, fechas.length - 1]);
        var pintar = function(vals) {
          var a = fechas[vals[0]], b = fechas[vals[1]];
          $('#slider-timestamp')
            .css({padding: '2px 6px', 'font-size': '12px', color: '#333'})
            .html(a === b ? a : a + ' \\u2013 ' + b);
        };
        // setTimeout(0): correr DESPUÉS del mouseup del plugin, que borra el rótulo.
        var repintar = function() {
          var vals = $('#leaflet-slider').slider('values');
          setTimeout(function() { pintar(vals); }, 0);
        };
        pintar([0, fechas.length - 1]);
        $(document).on('mouseup', repintar);
        $('#leaflet-slider').on('slidechange', repintar);
        // El rótulo desborda el control del deslizador; separar el control de
        // capas (mismo rincón topright) para que no lo tape.
        $(el).find('.leaflet-control-layers').css('margin-top', '40px');
      }",
      data = list(fechas = puntos_wgs84$time)
    )
  m
}

# Guarda el mapa temporal como HTML autocontenido (target con format = "file").
mapa_leaflet_temporal <- function(puntos, parque, cobertura, archivos_worldcover,
                                  bbox, dest) {
  m <- crear_mapa_temporal(puntos, parque, cobertura, archivos_worldcover, bbox)
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
  clima <- mensual |>
    dplyr::summarise(promedio = mean(detecciones), .by = mes) |>
    dplyr::mutate(nombre_mes = factor(MESES_ES[mes], levels = MESES_ES))
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

# --- Versiones interactivas (plotly) ----------------------------------------
# Los constructores retornan el widget (para incrustar en el reporte Quarto);
# los envoltorios grafico_*_html() lo guardan como HTML autocontenido.
# ggplotly no traslada subtitle/caption de ggplot, por lo que el subtítulo va
# como segunda línea del título (<sup>) y la fuente como anotación.

# Configuración común: barra de herramientas mínima y UI en español.
configurar_plotly <- function(w, titulo, subtitulo, fuente = "Datos: NASA FIRMS (MODIS_SP)") {
  w |>
    plotly::layout(
      title = list(
        text = paste0("<b>", titulo, "</b><br><sup>", subtitulo, "</sup>"),
        x = 0, xanchor = "left", font = list(size = 15)
      ),
      margin = list(t = 70, b = 60),
      annotations = list(
        text = fuente, xref = "paper", yref = "paper",
        x = 1, y = -0.18, xanchor = "right", yanchor = "top",
        showarrow = FALSE, font = list(size = 10, color = "grey")
      )
    ) |>
    plotly::config(
      locale = "es", displaylogo = FALSE,
      modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d")
    )
}

# Serie temporal mensual interactiva (zoom/pan sobre los 25 años de registro).
crear_serie_interactiva <- function(mensual) {
  datos <- mensual |>
    dplyr::mutate(etiqueta = paste0(
      "Mes: ", MESES_ES[mes], " ", format(aniomes, "%Y"),
      "<br>Detecciones: ", detecciones
    ))
  p <- ggplot2::ggplot(datos, ggplot2::aes(x = aniomes, y = detecciones,
                                           text = etiqueta)) +
    ggplot2::geom_col(fill = COLOR_DETECCIONES, width = 25) +
    ggplot2::scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(x = NULL, y = "Detecciones por mes") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "grey92")
    )
  plotly::ggplotly(p, tooltip = "text") |>
    configurar_plotly(
      "Detecciones mensuales de anomalías térmicas",
      "Parque Nacional Palo Verde — MODIS (FIRMS)"
    )
}

# Climatología mensual interactiva.
crear_climatologia_interactiva <- function(mensual) {
  clima <- mensual |>
    dplyr::summarise(promedio = mean(detecciones), .by = mes) |>
    dplyr::mutate(
      nombre_mes = factor(MESES_ES[mes], levels = MESES_ES),
      etiqueta = paste0("Mes: ", nombre_mes,
                        "<br>Promedio: ", round(promedio, 1))
    )
  p <- ggplot2::ggplot(clima, ggplot2::aes(x = nombre_mes, y = promedio,
                                           text = etiqueta)) +
    ggplot2::geom_col(fill = COLOR_DETECCIONES, width = 0.7) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(x = NULL, y = "Detecciones promedio") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "grey92")
    )
  plotly::ggplotly(p, tooltip = "text") |>
    configurar_plotly(
      "Climatología mensual de anomalías térmicas",
      "Promedio de detecciones por mes calendario — PN Palo Verde, MODIS (FIRMS)"
    )
}

# Guardan los gráficos interactivos como HTML autocontenido
# (targets con format = "file", mismo patrón que mapa_leaflet_temporal).
grafico_serie_html <- function(mensual, dest) {
  w <- crear_serie_interactiva(mensual)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  htmlwidgets::saveWidget(w, file.path(normalizePath(dirname(dest)), basename(dest)),
                          selfcontained = TRUE)
  dest
}

grafico_climatologia_html <- function(mensual, dest) {
  w <- crear_climatologia_interactiva(mensual)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  htmlwidgets::saveWidget(w, file.path(normalizePath(dirname(dest)), basename(dest)),
                          selfcontained = TRUE)
  dest
}
