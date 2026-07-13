# Procesamiento de los datos de FIRMS: lectura y unión de fragmentos,
# conversión a sf, recorte al parque y agregación mensual.

# Lee y une los CSV de todos los fragmentos. Fragmentos sin detecciones
# contienen solo el encabezado y aportan 0 filas.
leer_y_unir_csv <- function(paths) {
  paths |>
    purrr::map(\(p) readr::read_csv(p, show_col_types = FALSE)) |>
    purrr::list_rbind() |>
    dplyr::distinct()
}

# Convierte el data frame crudo a puntos sf en WGS84 y deriva campos temporales.
a_sf_puntos <- function(df) {
  df |>
    dplyr::mutate(
      acq_date = as.Date(acq_date),
      anio     = lubridate::year(acq_date),
      mes      = lubridate::month(acq_date),
      aniomes  = lubridate::floor_date(acq_date, "month")
    ) |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = CRS_WGS84,
                 remove = FALSE)
}

# Recorte ESTRICTO al polígono del parque (la descarga usa un bbox con buffer)
# y reproyección a CRTM05 para análisis y mapas.
recortar_al_parque <- function(puntos, parque) {
  parque_4326 <- sf::st_transform(parque, CRS_WGS84)
  puntos |>
    sf::st_filter(parque_4326, .predicate = sf::st_intersects) |>
    a_crtm05()
}

# Conteo mensual de detecciones, con meses sin detecciones completados en 0
# para series y animaciones continuas en el tiempo.
agregar_mensual <- function(puntos) {
  conteos <- puntos |>
    sf::st_drop_geometry() |>
    dplyr::count(aniomes, name = "detecciones")
  meses <- data.frame(
    aniomes = seq(min(conteos$aniomes), max(conteos$aniomes), by = "month")
  )
  meses |>
    dplyr::left_join(conteos, by = "aniomes") |>
    dplyr::mutate(
      detecciones = tidyr::replace_na(detecciones, 0L),
      anio = lubridate::year(aniomes),
      mes  = lubridate::month(aniomes)
    )
}
