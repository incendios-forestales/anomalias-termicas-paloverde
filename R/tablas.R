# Tablas resumen de detecciones.

# Resumen por año: total de detecciones y FRP; con `quemas`, además las
# hectáreas quemadas (MCD64A1). full_join: un año presente en una sola fuente
# no se pierde; el FRP queda NA en años sin detecciones (honesto: no hubo).
resumen_anual <- function(puntos, quemas = NULL) {
  resumen <- puntos |>
    sf::st_drop_geometry() |>
    dplyr::summarise(
      detecciones = dplyr::n(),
      frp_promedio = round(mean(frp, na.rm = TRUE), 1),
      frp_maximo = max(frp, na.rm = TRUE),
      .by = anio
    )
  if (!is.null(quemas)) {
    hectareas <- quemas |>
      sf::st_drop_geometry() |>
      dplyr::summarise(hectareas = round(sum(area_ha)), .by = anio)
    resumen <- resumen |>
      dplyr::full_join(hectareas, by = "anio") |>
      dplyr::mutate(
        detecciones = tidyr::replace_na(detecciones, 0L),
        hectareas = tidyr::replace_na(hectareas, 0)
      ) |>
      dplyr::relocate(hectareas, .after = detecciones)
  }
  dplyr::arrange(resumen, anio)
}

tabla_resumen_csv <- function(puntos, quemas, dest) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(resumen_anual(puntos, quemas), dest)
  dest
}

# Retorna el widget DT (para incrustar en el reporte Quarto).
crear_tabla_resumen <- function(puntos, quemas = NULL) {
  DT::datatable(
    resumen_anual(puntos, quemas),
    colnames = c("Año", "Detecciones",
                 if (!is.null(quemas)) "Hectáreas quemadas",
                 "FRP promedio (MW)", "FRP máximo (MW)"),
    caption = if (is.null(quemas)) {
      "Anomalías térmicas por año — PN Palo Verde, MODIS (FIRMS)"
    } else {
      "Anomalías térmicas y área quemada por año — PN Palo Verde, MODIS (FIRMS) y MCD64A1"
    },
    options = list(pageLength = 30, dom = "t"),
    rownames = FALSE
  )
}

# Contraste WorldCover vs. Registro Nacional de Humedales, para detecciones y
# área quemada (widget DT). colnames alineados 1:1 con contraste_completo().
crear_tabla_contraste <- function(cobertura, humedales_detecciones,
                                  cobertura_quemas, humedales_quemas) {
  DT::datatable(
    contraste_completo(cobertura, humedales_detecciones,
                       cobertura_quemas, humedales_quemas),
    colnames = c("Clase WorldCover", "Detecciones", "En humedal",
                 "% en humedal", "Fracción del footprint en humedal",
                 "Hectáreas quemadas", "% del área quemada en humedal"),
    caption = paste("Detecciones y área quemada por clase de WorldCover y su",
                    "coincidencia con el Registro Nacional de Humedales (SINAC)"),
    options = list(dom = "t"),
    rownames = FALSE
  )
}

# Resumen por año del área quemada: hectáreas totales y mes pico.
resumen_anual_quemas <- function(quemas) {
  quemas |>
    sf::st_drop_geometry() |>
    dplyr::summarise(hectareas = sum(area_ha), .by = c(anio, mes)) |>
    dplyr::summarise(
      mes_pico = MESES_ES[mes[which.max(hectareas)]],
      hectareas = round(sum(hectareas)),
      .by = anio
    ) |>
    dplyr::relocate(anio, hectareas, mes_pico) |>
    dplyr::arrange(anio)
}

tabla_area_quemada_csv <- function(quemas, dest) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(resumen_anual_quemas(quemas), dest)
  dest
}

tabla_resumen_html <- function(puntos, quemas, dest) {
  tabla <- crear_tabla_resumen(puntos, quemas)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  htmlwidgets::saveWidget(tabla, file.path(normalizePath(dirname(dest)), basename(dest)),
                          selfcontained = TRUE)
  dest
}
