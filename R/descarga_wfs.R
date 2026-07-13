# Descarga del polígono del Parque Nacional Palo Verde desde el WFS del SINAC.

# Descarga idempotente: si el GPKG ya existe, lo reutiliza.
# Retorna el path al GPKG (target con format = "file").
descargar_parque_wfs <- function(dest = "data/raw/wfs/palo_verde.gpkg") {
  if (file.exists(dest) && file.info(dest)$size > 0) {
    message(glue::glue("[cache] {basename(dest)} ya existe"))
    return(dest)
  }
  consulta <- paste0(
    WFS_SINAC,
    "?service=WFS&version=2.0.0&request=GetFeature",
    "&typeNames=", utils::URLencode(WFS_CAPA_ASP, reserved = TRUE),
    "&outputFormat=", utils::URLencode("application/json", reserved = TRUE),
    "&srsName=", utils::URLencode(CRS_CRTM05, reserved = TRUE),
    "&cql_filter=", utils::URLencode(WFS_FILTRO_PARQUE, reserved = TRUE)
  )
  message(glue::glue("[descarga] polígono del parque desde el WFS del SINAC"))
  parque <- sf::st_read(consulta, quiet = TRUE)
  if (nrow(parque) != 1) {
    stop("Se esperaba exactamente 1 feature del parque; el WFS devolvió ", nrow(parque),
         call. = FALSE)
  }
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  sf::st_write(parque, dest, delete_dsn = TRUE, quiet = TRUE)
  dest
}

# Lee el GPKG y garantiza CRS CRTM05.
procesar_parque <- function(path) {
  sf::st_read(path, quiet = TRUE) |> a_crtm05()
}
