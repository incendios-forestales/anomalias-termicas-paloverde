# Catálogo de plataformas satelitales.
#
# Una fila por plataforma y de ella se deriva TODO lo visible: rótulos de
# figuras, pies de fuente, nombres de capa y rutas de salida. Es la pieza que
# mantiene a las plataformas como hermanas: ninguna es el valor por defecto de
# nada, porque todas salen de la misma tabla. Las funciones de R/ no tienen
# valores por defecto de rótulo justamente para que sea imposible generar un
# producto "de MODIS" por olvido.
#
# Columnas:
#   clave        identificador corto; da nombre a targets, directorios y rutas web
#   etiqueta     nombre completo para prosa y títulos
#   corta        nombre para rótulos de figura, donde el espacio es escaso
#   fuente_sp    colección de FIRMS con procesamiento estándar (NA si no existe)
#   fuente_nrt   colección de FIRMS en tiempo casi real
#   ba_producto  producto de área quemada que acompaña a la plataforma
#   ba_etiqueta  cómo se nombra ese producto en los rótulos (indica la
#                plataforma de origen cuando es prestado)
#   ba_creditos  cómo se nombra en los pies de fuente
#   ba_version   con número de versión, para el pie del producto de área quemada
#
# NOAA-20 y NOAA-21 no tienen producto de área quemada propio (VJ164A1 no está
# publicado; verificado en CMR el 2026-08-04), así que toman el de Suomi-NPP y
# lo declaran en la etiqueta.
PLATAFORMAS <- tibble::tribble(
  ~clave,   ~etiqueta,             ~corta,          ~fuente_sp,        ~fuente_nrt,          ~ba_producto, ~ba_etiqueta,     ~ba_creditos,          ~ba_version,
  "modis",  "MODIS (Terra/Aqua)",  "MODIS",         "MODIS_SP",        "MODIS_NRT",          "MCD64A1",    "MCD64A1",        "MCD64A1",             "MCD64A1 v6.1",
  "viirs",  "VIIRS (Suomi-NPP)",   "VIIRS",         "VIIRS_SNPP_SP",   "VIIRS_SNPP_NRT",     "VNP64A1",    "VNP64A1",        "VNP64A1",             "VNP64A1 v2",
  "noaa20", "VIIRS (NOAA-20)",     "VIIRS NOAA-20", "VIIRS_NOAA20_SP", "VIIRS_NOAA20_NRT",   "VNP64A1",    "VNP64A1, S-NPP", "VNP64A1, Suomi-NPP",  "VNP64A1 v2, Suomi-NPP"
)

# Fila de PLATAFORMAS, con error claro si la clave no existe (un típo en una
# clave produciría si no un data frame vacío y rótulos NA silenciosos).
plataforma <- function(clave) {
  fila <- PLATAFORMAS[PLATAFORMAS$clave == clave, ]
  if (nrow(fila) != 1) {
    stop("Plataforma desconocida: '", clave, "'. Definidas: ",
         paste(PLATAFORMAS$clave, collapse = ", "), call. = FALSE)
  }
  fila
}

# Identificador de la colección de FIRMS que nombra a la plataforma en los pies
# de fuente: el estándar cuando existe y el de tiempo casi real cuando no
# (NOAA-21 nunca ha tenido procesamiento estándar).
id_fuente_principal <- function(clave) {
  p <- plataforma(clave)
  if (is.na(p$fuente_sp)) p$fuente_nrt else p$fuente_sp
}

# Todas las cadenas visibles de una plataforma, derivadas de su fila. Las
# funciones de figuras y tablas reciben estos valores; así un cambio de
# nomenclatura se hace en un solo lugar y no puede quedar a medias entre
# plataformas.
etiquetas_plataforma <- function(clave) {
  p <- plataforma(clave)
  id <- id_fuente_principal(clave)
  # Los pies de fuente nombran TODAS las colecciones que alimentan la serie:
  # desde que se empalma la cola en tiempo casi real, citar solo el
  # procesamiento estándar dejaría sin acreditar los meses más recientes.
  ids <- if (is.na(p$fuente_sp)) p$fuente_nrt else
    paste(p$fuente_sp, "y", p$fuente_nrt)
  list(
    plataforma       = p$etiqueta,
    corta            = p$corta,
    id_fuente        = id,
    ids_fuente       = ids,
    # Rótulo del sensor en subtítulos de figura y encabezados de tabla
    fuente_fig       = paste0(p$corta, " (FIRMS)"),
    etiqueta_ba      = p$ba_etiqueta,
    etiqueta_quemas  = paste0("Área quemada (", p$ba_etiqueta, ")"),
    # Pies de fuente, por combinación de insumos usada en cada producto
    pie_firms        = paste0("Datos: NASA FIRMS (", ids, ")"),
    pie_animacion    = paste0("Datos: NASA FIRMS (", ids,
                              ") y SINAC. Fondo: ESA WorldCover 2021"),
    pie_cobertura    = paste0("Datos: NASA FIRMS (", ids,
                              "), ESA WorldCover 2021 y SINAC"),
    pie_ba           = paste0("Datos: NASA LP DAAC (", p$ba_version, ")"),
    pie_ambos        = paste0("Datos: NASA FIRMS (", ids, ") y LP DAAC (",
                              p$ba_creditos, ")"),
    pie_cobertura_ba = paste0("Datos: NASA LP DAAC (", p$ba_creditos,
                              "), ESA WorldCover 2021 y SINAC")
  )
}
