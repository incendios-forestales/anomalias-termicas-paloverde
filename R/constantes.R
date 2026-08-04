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

# Capas nacionales de contexto (mismo geoserver). Se descargan recortadas al
# bbox del parque. La cobertura forestal solo mapea clases de BOSQUE (no es
# cobertura completa: sin pastizal ni cultivos); se usa la versión 2023, que
# es la más detallada — la de 2021 clasifica casi todo como bosque secundario.
WFS_CAPA_BOSQUE   <- "PNE:cobertura_forestal_2023"
WFS_CAPA_HUMEDALES <- "PNE:registro_nacional_humedales"

# --- NASA FIRMS ---
FIRMS_BASE <- "https://firms.modaps.eosdis.nasa.gov/api"

# Tamaño de fragmento de descarga (días por solicitud). El API de área admite
# rangos pequeños por solicitud (la documentación actual indica 1-5 días).
# NO cambiar una vez iniciada la descarga: la rejilla de fragmentos depende de
# este valor y cambiarlo invalida la caché completa.
FIRMS_DIAS_FRAGMENTO <- 5L

# Origen fijo de la rejilla de fragmentos (coincide con el inicio del registro
# MODIS_SP, la fuente más antigua). La rejilla es COMÚN a todas las fuentes:
# los límites de fragmento se calculan como ORIGEN_GRILLA + k * FIRMS_DIAS_FRAGMENTO,
# independientes del rango solicitado, y clamp_rango() recorta el arranque de
# las fuentes más recientes (p. ej. VIIRS_SNPP_SP desde 2012) al primer
# fragmento que las contiene. Ampliar el rango solo agrega fragmentos en los
# extremos sin invalidar los ya descargados.
ORIGEN_GRILLA <- as.Date("2000-11-01")

# Buffer (km) alrededor del parque para el bbox de descarga: cubre de sobra la
# geolocalización de los sensores (~1 km en MODIS, ~375 m en VIIRS) para
# capturar detecciones de borde; el análisis recorta estrictamente al polígono.
FIRMS_BUFFER_KM <- 5

# Fuentes de FIRMS: cada fuente activa es una cadena explícita de targets en
# _targets.R (hoy MODIS_SP y VIIRS_SNPP_SP; con una tercera fuente conviene
# migrar a tarchetypes::tar_map). La capa de descarga ya separa la caché por
# data_id (data/raw/firms/<data_id>/).
# El área quemada (BA_MODIS/BA_VIIRS) NO se obtiene de FIRMS: su API acepta
# esas colecciones pero responde vacío siempre; se usan los productos
# originales MCD64A1 y VNP64A1 (ver abajo).

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

# --- LP DAAC (área quemada: MCD64A1 y VNP64A1) ---
# Búsqueda de granulos en el catálogo CMR de NASA (pública, sin credenciales).
CMR_BASE <- "https://cmr.earthdata.nasa.gov/search"

MCD64A1_SHORT_NAME <- "MCD64A1"
MCD64A1_VERSION    <- "061"

# Heredero de MCD64A1 derivado de VIIRS S-NPP (registro desde 2012-03); mismo
# algoritmo, rejilla sinusoidal y formato HDF4. Verificado contra CMR el
# 2026-08-04: versión "002", descargas .hdf en lp-prod-protected.
VNP64A1_SHORT_NAME <- "VNP64A1"
VNP64A1_VERSION    <- "002"

# Tesela de la rejilla sinusoidal MODIS que cubre el PN Palo Verde.
# Es común a MCD64A1 y VNP64A1: ambos productos usan la misma rejilla
# (10.35 N, -85.35 O). extraer_quemas() verifica en tiempo de ejecución que
# la tesela efectivamente cubra el parque.
TESELA_SINUSOIDAL <- "h09v07"

# Token de Earthdata Login, desde .Renviron (no versionado). A diferencia de
# la MAP_KEY de FIRMS, los tokens de Earthdata expiran (~60 días).
earthdata_token <- function() {
  token <- Sys.getenv("EARTHDATA_TOKEN")
  if (!nzchar(token)) {
    stop("Falta EARTHDATA_TOKEN. Genere un token en https://urs.earthdata.nasa.gov/ ",
         "(Generate Token) y agréguelo a .Renviron (ver .Renviron.example).",
         call. = FALSE)
  }
  token
}
