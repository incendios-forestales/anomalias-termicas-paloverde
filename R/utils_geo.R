# Utilitarios geoespaciales compartidos (adaptado de asp-black-summer).

# Convierte un sf al CRS de análisis (CRTM05).
a_crtm05 <- function(x) {
  stopifnot(inherits(x, "sf") || inherits(x, "sfc"))
  sf::st_transform(x, CRS_CRTM05)
}

# Descarga un archivo a un path local solo si no existe aún.
# Retorna el path local. Útil para caché idempotente en data/raw/.
download_if_missing <- function(url, dest, mode = "wb", timeout = 1800) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(dest) && file.info(dest)$size > 0) {
    message(glue::glue("[cache] {basename(dest)} ya existe"))
    return(invisible(dest))
  }
  message(glue::glue("[descarga] {url} -> {dest}"))
  old_timeout <- getOption("timeout")
  options(timeout = timeout)
  on.exit(options(timeout = old_timeout), add = TRUE)
  utils::download.file(url, dest, mode = mode, quiet = FALSE)
  invisible(dest)
}

# Bounding box con buffer alrededor de un polígono, en WGS84,
# como vector nombrado c(oeste, sur, este, norte) para el API de FIRMS.
bbox_con_buffer <- function(poligono, buffer_km = FIRMS_BUFFER_KM) {
  b <- poligono |>
    a_crtm05() |>
    sf::st_buffer(buffer_km * 1000) |>
    sf::st_transform(CRS_WGS84) |>
    sf::st_bbox()
  c(oeste = unname(b["xmin"]), sur = unname(b["ymin"]),
    este = unname(b["xmax"]), norte = unname(b["ymax"]))
}
