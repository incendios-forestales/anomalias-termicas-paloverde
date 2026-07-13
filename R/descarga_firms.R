# Cliente del API de área de NASA FIRMS: disponibilidad, rejilla de fragmentos
# y descarga por fragmento, reanudable e idempotente.
#
# API: https://firms.modaps.eosdis.nasa.gov/api/
#   GET {base}/data_availability/csv/{MAP_KEY}/{SOURCE}
#   GET {base}/area/csv/{MAP_KEY}/{SOURCE}/{oeste,sur,este,norte}/{rango_dias}/{fecha_inicio}

# Rango de fechas disponible para una fuente: c(inicio, fin) como Date.
firms_disponibilidad <- function(data_id) {
  url <- glue::glue("{FIRMS_BASE}/data_availability/csv/{firms_map_key()}/{data_id}")
  df <- utils::read.csv(url, stringsAsFactors = FALSE)
  fila <- df[df$data_id == data_id, ]
  if (nrow(fila) != 1) {
    stop("data_availability no devolvió la fuente ", data_id, call. = FALSE)
  }
  c(inicio = as.Date(fila$min_date), fin = as.Date(fila$max_date))
}

# Recorta el rango solicitado al disponible.
clamp_rango <- function(inicio, fin, disponible) {
  rango <- c(inicio = max(inicio, disponible[["inicio"]]),
             fin    = min(fin, disponible[["fin"]]))
  if (rango[["inicio"]] > rango[["fin"]]) {
    stop("El rango solicitado no se traslapa con el disponible (",
         disponible[["inicio"]], " a ", disponible[["fin"]], ").", call. = FALSE)
  }
  rango
}

# Rejilla de fragmentos anclada a ORIGEN_GRILLA: los límites son
# ORIGEN_GRILLA + k * FIRMS_DIAS_FRAGMENTO, independientes del rango pedido.
# Ampliar el rango solo agrega fragmentos en los extremos; los interiores
# conservan sus límites (y por lo tanto su nombre de archivo y su rama).
# Solo el primer y último fragmento se recortan al rango efectivo; el nombre
# del último cambia cuando avanza la disponibilidad, forzando su re-descarga.
# Retorna un data frame agrupado por fila para branching dinámico de targets.
construir_fragmentos <- function(rango, dias = FIRMS_DIAS_FRAGMENTO,
                                 origen = ORIGEN_GRILLA) {
  k_inicio <- floor(as.numeric(rango[["inicio"]] - origen) / dias)
  k_fin    <- floor(as.numeric(rango[["fin"]] - origen) / dias)
  inicio <- origen + (k_inicio:k_fin) * dias
  fin    <- inicio + (dias - 1)
  fragmentos <- data.frame(
    inicio = pmax(inicio, rango[["inicio"]]),
    fin    = pmin(fin, rango[["fin"]])
  )
  # La clave de grupo es la fecha de inicio (estable ante ampliaciones del
  # rango); un número de fila desplazaría todas las ramas al anteponer
  # fragmentos.
  fragmentos |>
    dplyr::group_by(inicio) |>
    targets::tar_group()
}

# Descarga UN fragmento (data frame de 1 fila con columnas inicio, fin).
# Idempotente y reanudable:
#   - si el CSV ya existe con contenido, lo retorna sin descargar;
#   - escritura atómica (.part -> rename): una interrupción nunca deja un CSV
#     truncado que se tome por válido.
# Retorna el path al CSV (target con format = "file").
descargar_firms_fragmento <- function(fragmento, data_id, bbox,
                                      dir = "data/raw/firms") {
  inicio <- fragmento$inicio[[1]]
  fin    <- fragmento$fin[[1]]
  destino <- file.path(dir, data_id, glue::glue("{inicio}_{fin}.csv"))
  if (file.exists(destino) && file.info(destino)$size > 0) {
    message(glue::glue("[cache] {basename(destino)} ya existe"))
    return(destino)
  }
  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)

  rango_dias <- as.integer(fin - inicio) + 1L
  area <- paste(round(bbox[c("oeste", "sur", "este", "norte")], 5), collapse = ",")
  url <- glue::glue(
    "{FIRMS_BASE}/area/csv/{firms_map_key()}/{data_id}/{area}/{rango_dias}/{inicio}"
  )
  message(glue::glue("[descarga] {data_id} {inicio} a {fin}"))
  # Cada solicitud de área consume ~10 transacciones del límite de 5000/10 min;
  # si se agota, el API responde HTTP 400 con "Exceeding allowed transaction
  # limit": se espera 60 s por intento hasta que la ventana se libere.
  resp <- httr2::request(url) |>
    httr2::req_retry(
      max_tries = 12,
      backoff = function(i) 60,
      is_transient = function(r) {
        httr2::resp_status(r) %in% c(429, 500, 502, 503) ||
          (httr2::resp_status(r) == 400 &&
             grepl("transaction limit", httr2::resp_body_string(r), fixed = TRUE))
      }
    ) |>
    httr2::req_perform()
  cuerpo <- httr2::resp_body_string(resp)

  # El API puede devolver mensajes de error como texto con estatus 200:
  # un CSV válido siempre inicia con el encabezado (columna latitude).
  primera_linea <- strsplit(cuerpo, "\n", fixed = TRUE)[[1]][1]
  if (!grepl("latitude", primera_linea, fixed = TRUE)) {
    stop("Respuesta inesperada del API de FIRMS para ", basename(destino), ": ",
         substr(cuerpo, 1, 200), call. = FALSE)
  }

  temporal <- paste0(destino, ".part")
  writeLines(cuerpo, temporal)
  file.rename(temporal, destino)
  Sys.sleep(1)  # ritmo bajo el límite de transacciones (~500 solicitudes/10 min)
  destino
}
