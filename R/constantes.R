# Constantes y configuración del proyecto.
#
# CRS de referencia:
#   EPSG:4326 — WGS 84 (coordenadas de FIRMS y del API de área)
#   EPSG:5367 — CRTM05 (CRS oficial métrico de Costa Rica, para mapas y áreas)

CRS_WGS84  <- "EPSG:4326"
CRS_CRTM05 <- "EPSG:5367"

# --- SINAC / WFS ---
WFS_SINAC   <- "https://geos1pne.sirefor.go.cr/wfs"
WFS_CAPA_ASP <- "PNE:areas_silvestres_protegidas"
# El parque es una sola feature identificada por estos atributos
WFS_FILTRO_PARQUE <- "nombre_asp='Palo Verde' AND cat_manejo='Parque Nacional'"

# --- NASA FIRMS ---
FIRMS_BASE <- "https://firms.modaps.eosdis.nasa.gov/api"

# Tamaño de fragmento de descarga (días por solicitud). El API de área admite
# rangos pequeños por solicitud (la documentación actual indica 1-5 días).
# NO cambiar una vez iniciada la descarga: la rejilla de fragmentos depende de
# este valor y cambiarlo invalida la caché completa.
FIRMS_DIAS_FRAGMENTO <- 5L

# Origen fijo de la rejilla de fragmentos (inicio del registro MODIS_SP).
# Los límites de fragmento se calculan como ORIGEN_GRILLA + k * FIRMS_DIAS_FRAGMENTO,
# independientes del rango solicitado: ampliar el rango solo agrega fragmentos
# en los extremos sin invalidar los ya descargados.
ORIGEN_GRILLA <- as.Date("2000-11-01")

# Buffer (km) alrededor del parque para el bbox de descarga: la geolocalización
# de MODIS es ~1 km, así se capturan detecciones de borde; el análisis recorta
# estrictamente al polígono.
FIRMS_BUFFER_KM <- 5

# Fuentes de FIRMS. Para incorporar otras (MODIS_NRT, VIIRS_*, BA_*) basta
# activarlas aquí y extender el grafo de targets con las fuentes activas.
FUENTES_FIRMS <- data.frame(
  data_id = c("MODIS_SP", "MODIS_NRT", "VIIRS_SNPP_SP", "VIIRS_SNPP_NRT",
              "VIIRS_NOAA20_SP", "VIIRS_NOAA20_NRT", "VIIRS_NOAA21_NRT"),
  activa  = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
)

# Clave del API de FIRMS, desde .Renviron (no versionado; ver .Renviron.example)
firms_map_key <- function() {
  clave <- Sys.getenv("FIRMS_MAP_KEY")
  if (!nzchar(clave)) {
    stop("Falta FIRMS_MAP_KEY. Copie .Renviron.example a .Renviron e ingrese su clave ",
         "(se solicita en https://firms.modaps.eosdis.nasa.gov/api/map_key/).",
         call. = FALSE)
  }
  clave
}
