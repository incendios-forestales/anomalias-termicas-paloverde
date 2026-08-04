# Procesamiento de los datos de FIRMS: lectura y unión de fragmentos,
# conversión a sf, recorte al parque y agregación mensual.

# Lee y une los CSV de todos los fragmentos. Fragmentos sin detecciones
# contienen solo el encabezado y aportan 0 filas; se lee todo como carácter
# (un CSV vacío haría que read_csv adivinara tipos incompatibles al unir)
# y luego se convierten las columnas numéricas presentes.
# `rangos` (de rangos_plataforma()) aporta el nivel de procesamiento de cada
# fuente y, sobre todo, resuelve el traslape SP/NRT POR FECHA: se conservan
# todas las filas del estándar y solo las de tiempo casi real posteriores al
# fin del rango estándar.
#
# Por qué por fecha y no por qué archivos hay en disco: la caché es idempotente
# y acumulativa, así que cuando el estándar avanza quedan CSV del NRT cuyas
# fechas ya están cubiertas por el definitivo. Esos fragmentos dejan de estar
# en `rangos` y por tanto no llegan aquí, pero el filtro por fecha deja la
# lectura correcta aunque alguna vez llegaran.
#
# Por qué contra el `fin` del rango y no contra max(acq_date) del estándar: si
# el último mes del estándar no tuvo detecciones en el bbox, el máximo
# observado retrocedería y dejaría entrar filas NRT ya definitivas.
#
# El nivel se recupera del directorio padre de cada ruta, que es el data_id
# (data/raw/firms/<data_id>/...); no hace falta pasar la tabla de fragmentos.
leer_y_unir_csv <- function(paths, rangos) {
  numericas <- c("latitude", "longitude", "brightness", "bright_t31",
                 "bright_ti4", "bright_ti5", "scan", "track", "frp")
  enteras <- c("acq_time", "type")
  df <- paths |>
    purrr::map(\(p) {
      readr::read_csv(p, col_types = readr::cols(.default = "c")) |>
        dplyr::mutate(data_id = basename(dirname(p)))
    }) |>
    purrr::list_rbind() |>
    dplyr::distinct() |>
    dplyr::left_join(rangos[, c("data_id", "nivel")], by = "data_id")

  fin_sp <- rangos$fin[rangos$nivel == "SP"]
  if (length(fin_sp) == 1) {
    df <- df[df$nivel == "SP" | as.Date(df$acq_date) > fin_sp, ]
  }
  # FIRMS marca el nivel también en `version` ("6.1NRT" frente a "6.1"). Se usa
  # como aserción, no como mecanismo: si alguna vez el nombre de directorio y
  # el contenido discreparan, es preferible fallar que publicar mezclado.
  if ("version" %in% names(df) && nrow(df) > 0) {
    stopifnot(!any(df$nivel == "SP" & grepl("NRT", df$version, fixed = TRUE)))
  }
  df |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(numericas), as.numeric),
      dplyr::across(dplyr::any_of(enteras), as.integer)
    )
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
#
# La rejilla de meses sale de `rangos` (lo observado por el satélite) y no de
# los meses con detecciones: si la cola en tiempo casi real no produce ninguna
# detección dentro del parque —muy posible, porque el recorte estricto elimina
# la quema agrícola del valle— la serie se acortaría en silencio y la cola
# desaparecería del gráfico sin que nada lo advirtiera. Un mes sin detecciones
# es un cero informativo, no una ausencia de dato.
#
# `nivel` distingue los meses cubiertos por el procesamiento estándar de los
# provisionales, y marca como "mixto" el mes en que ocurre el corte (para
# S-NPP, abril de 2026 es estándar hasta el 27 y provisional del 28 al 30).
# Se deriva de `rangos`, no de las detecciones observadas.
agregar_mensual <- function(puntos, rangos) {
  conteos <- puntos |>
    sf::st_drop_geometry() |>
    dplyr::count(aniomes, name = "detecciones")
  meses <- data.frame(
    aniomes = seq(lubridate::floor_date(min(rangos$inicio), "month"),
                  lubridate::floor_date(max(rangos$fin), "month"),
                  by = "month")
  )
  nivel_de <- function(mes) {
    fin_mes <- lubridate::ceiling_date(mes, "month") - 1
    cubre <- rangos$inicio <= fin_mes & rangos$fin >= mes
    niveles <- unique(rangos$nivel[cubre])
    if (length(niveles) > 1) "mixto" else if (length(niveles) == 1) niveles else NA_character_
  }
  meses |>
    dplyr::left_join(conteos, by = "aniomes") |>
    dplyr::mutate(
      detecciones = tidyr::replace_na(detecciones, 0L),
      anio  = lubridate::year(aniomes),
      mes   = lubridate::month(aniomes),
      nivel = vapply(aniomes, nivel_de, character(1))
    )
}
