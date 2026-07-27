# Tablas resumen de detecciones.

# Resumen por año: total de detecciones, mes pico y FRP.
resumen_anual <- function(puntos) {
  puntos |>
    sf::st_drop_geometry() |>
    dplyr::summarise(
      detecciones = dplyr::n(),
      frp_promedio = round(mean(frp, na.rm = TRUE), 1),
      frp_maximo = max(frp, na.rm = TRUE),
      .by = anio
    ) |>
    dplyr::arrange(anio)
}

tabla_resumen_csv <- function(puntos, dest) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(resumen_anual(puntos), dest)
  dest
}

# Retorna el widget DT (para incrustar en el reporte Quarto).
crear_tabla_resumen <- function(puntos) {
  DT::datatable(
    resumen_anual(puntos),
    colnames = c("Año", "Detecciones", "FRP promedio (MW)", "FRP máximo (MW)"),
    caption = "Anomalías térmicas por año — PN Palo Verde, MODIS (FIRMS)",
    options = list(pageLength = 30, dom = "t"),
    rownames = FALSE
  )
}

# Contraste WorldCover vs. Registro Nacional de Humedales (widget DT).
crear_tabla_contraste <- function(cobertura, humedales_detecciones) {
  DT::datatable(
    contraste_humedales(cobertura, humedales_detecciones),
    colnames = c("Clase WorldCover", "Detecciones", "En humedal",
                 "% en humedal", "Fracción del footprint en humedal"),
    caption = paste("Detecciones por clase de WorldCover y su coincidencia con",
                    "el Registro Nacional de Humedales (SINAC)"),
    options = list(dom = "t"),
    rownames = FALSE
  )
}

tabla_resumen_html <- function(puntos, dest) {
  tabla <- crear_tabla_resumen(puntos)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  htmlwidgets::saveWidget(tabla, file.path(normalizePath(dirname(dest)), basename(dest)),
                          selfcontained = TRUE)
  dest
}
