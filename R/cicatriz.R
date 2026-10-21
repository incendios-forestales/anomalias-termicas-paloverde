# Cicatriz de un evento documentado medida con Sentinel-2: el dNBR entre la
# escena previa y la posterior, y el perímetro de la mancha quemada.
#
# Es la tercera medición del evento, independiente de las otras dos. Las
# detecciones de fuego activo dicen dónde hubo llama al pasar el satélite y
# el área quemada de 500 m dónde persistió un cambio de reflectancia semanas
# después; el dNBR a 20 m ve la cicatriz al tamaño del typhal. Para Catalina
# el sondeo que precedió a este módulo (2026-08-06) dio un perímetro de
# ~3300 ha, entre las 706 ha de VNP64A1 y las 4000 oficiales, y mostró que
# el núcleo severo (dNBR > 0,44) coincide casi exactamente con lo que el
# producto de 500 m sí marcó: el producto no falla al azar, captura el núcleo
# y pierde el resto.
#
# NBR = (B8A − B12) / (B8A + B12); dNBR = NBR(previa) − NBR(post). Los
# umbrales 0,10 / 0,27 / 0,44 son los de la escala de severidad de Key &
# Benson (FIREMON) para cambio detectable, moderado y alto.
#
# El perímetro NO es un umbral suelto sino una mancha conexa: un umbral solo
# confunde reverdecimiento y suelo descubierto con quema (la laguna de Palo
# Verde tiene 650 ha con dNBR > 0,10 que son marisma reverdecida, no fuego).
# Criterio: cambio (> 0,10) abierto con ventana 3×3, conservado solo a
# ≤ CICATRIZ_ANCLAJE_M de un píxel con severidad moderada, cerrado con
# ventana CICATRIZ_CIERRE_PX, componente conexo mayor y huecos rellenados
# (los islotes de bosque que el fuego rodeó forman parte del perímetro, que es
# lo que un perímetro de incendio significa). El anclaje es lo que impide que
# el cierre morfológico salte a marismas vecinas que solo reverdecieron; el
# resultado varía < 2 % si la distancia se mueve entre 400 y 800 m.

# Clases de la banda SCL que invalidan un píxel en cualquiera de las dos
# fechas: sin dato, saturado, sombra, sombra de nube, nube media y alta, cirro.
SCL_INVALIDAS <- c(0, 1, 2, 3, 8, 9, 10)

DNBR_CAMBIO   <- 0.10
DNBR_MODERADO <- 0.27
DNBR_ALTO     <- 0.44

CICATRIZ_ANCLAJE_M <- 500
CICATRIZ_CIERRE_PX <- 11L   # 11 px a 20 m: radio de ~100 m

# Humedal de control: el que más se parece a una quema sin serlo (reverdece
# con las lluvias entre las dos fechas). Las cifras lo reportan para que la
# prosa pueda decir que el perímetro no se derrama hacia él.
HUMEDAL_CONTROL <- "Humedal Palo Verde 1"

# dNBR de un evento a partir de las bandas descargadas (escenas_s2_evento() y
# descargar_banda_s2()), enmascarado donde cualquiera de las dos fechas tiene
# nube, sombra o datos faltantes. Escribe un GeoTIFF en el CRS de la escena
# (UTM) y retorna el path (target con format = "file").
dnbr_evento <- function(escenas, archivos, dest) {
  escenas <- dplyr::ungroup(escenas)
  banda <- function(momento, nombre) {
    archivo <- archivos[basename(archivos) ==
                          escenas$nombre[escenas$momento == momento &
                                           escenas$banda == nombre]]
    terra::rast(archivo)
  }
  nbr <- function(momento) {
    nir <- banda(momento, "B8A")
    swir <- banda(momento, "B12")
    (nir - swir) / (nir + swir)
  }
  invalido <- (banda("previa", "SCL") %in% SCL_INVALIDAS) |
    (banda("post", "SCL") %in% SCL_INVALIDAS)
  d <- terra::mask(nbr("previa") - nbr("post"), invalido, maskvalues = 1)
  names(d) <- "dnbr"
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  terra::writeRaster(d, dest, overwrite = TRUE, datatype = "FLT4S",
                     gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
  dest
}

# Operador morfológico binario sobre un raster 0/1 sin NA: erosión (min) o
# dilatación (max) con ventana cuadrada de `w` píxeles; el borde se rellena
# con 0 como hace scipy.ndimage.
morfologia <- function(x, w, fun) {
  terra::focal(x, w = w, fun = fun, fillvalue = 0, expand = FALSE)
}

# Perímetro de la cicatriz y cifras del evento. `dnbr` es el path del GeoTIFF
# de dnbr_evento(). Escribe el perímetro como GeoPackage en CRTM05 (para un
# SIG) y retorna una lista con el polígono (sf), las cifras (tibble en
# hectáreas) y el path.
cicatriz_evento <- function(clave_evento, dnbr, parque, humedales, dest) {
  e <- evento(clave_evento)
  d <- terra::rast(dnbr)
  crs_d <- terra::crs(d)
  px_ha <- prod(terra::res(d)) / 1e4
  rasterizar <- function(x) {
    terra::rasterize(terra::vect(sf::st_transform(x, crs_d)), d, field = 1,
                     background = 0)
  }
  ha <- function(m) as.numeric(terra::global(m, "sum", na.rm = TRUE)) * px_ha

  parque_r <- rasterizar(parque)
  sector <- sf::st_union(humedales[grepl(e$sector, humedales$nom_hum,
                                         fixed = TRUE), ])
  control <- sf::st_intersection(
    sf::st_union(humedales[humedales$nom_hum == HUMEDAL_CONTROL, ]),
    sf::st_union(sf::st_transform(parque, sf::st_crs(humedales)))
  )
  sector_r <- rasterizar(sector)
  control_r <- rasterizar(control)

  # Sin NA para la morfología: un píxel inválido cuenta como "sin cambio",
  # igual que en el sondeo (nan_to_num a −9).
  dd <- terra::ifel(is.na(d), -9, d)
  en_parque <- parque_r == 1
  cambio <- (dd > DNBR_CAMBIO) & en_parque
  nucleo <- (dd > DNBR_MODERADO) & en_parque

  abierto <- morfologia(morfologia(cambio, 3, "min"), 3, "max")
  cerca <- terra::as.polygons(terra::ifel(nucleo, 1, NA), dissolve = TRUE) |>
    sf::st_as_sf() |>
    sf::st_union() |>
    sf::st_buffer(CICATRIZ_ANCLAJE_M)
  anclado <- abierto & (rasterizar(cerca) == 1)
  cerrado <- morfologia(morfologia(anclado, CICATRIZ_CIERRE_PX, "max"),
                        CICATRIZ_CIERRE_PX, "min")

  # Componente conexo mayor (8 vecinos) y relleno de huecos: el polígono se
  # reconstruye solo con los anillos exteriores.
  parches <- terra::patches(terra::ifel(cerrado == 1, 1, NA), directions = 8)
  frecuencias <- terra::freq(parches)
  mayor <- frecuencias$value[which.max(frecuencias$count)]
  mancha <- terra::as.polygons(terra::ifel(parches == mayor, 1, NA),
                               dissolve = TRUE) |>
    sf::st_as_sf() |>
    sf::st_geometry() |>
    sf::st_union() |>
    sf::st_cast("POLYGON")
  perimetro <- sf::st_sfc(
    lapply(mancha, function(pg) sf::st_polygon(list(pg[[1]]))),
    crs = sf::st_crs(mancha)
  ) |>
    sf::st_union()
  perimetro_r <- rasterizar(perimetro)
  dentro <- perimetro_r == 1

  # Cambio disperso fuera del perímetro: el término de ruido (barras del río,
  # potreros, marismas que reverdecen), caracterizado por su fragmentación.
  disperso <- cambio & !dentro
  parches_fuera <- terra::patches(terra::ifel(disperso, 1, NA), directions = 8)
  tam_fuera <- terra::freq(parches_fuera)$count * px_ha

  cifras <- tibble::tibble(
    perimetro_ha      = ha(dentro),
    quemado_ha        = ha(dentro & cambio),
    islotes_ha        = ha(dentro & !cambio),
    moderado_ha       = ha(dentro & nucleo),
    alto_ha           = ha(dentro & (dd > DNBR_ALTO)),
    sector_ha         = ha(dentro & (sector_r == 1)),
    sector_total_ha   = as.numeric(sf::st_area(sector)) / 1e4,
    fuera_sector_ha   = ha(dentro & (sector_r == 0)),
    control_ha        = ha(dentro & (control_r == 1)),
    control_cambio_ha = ha(cambio & (control_r == 1)),
    control_moderado_ha = ha(nucleo & (control_r == 1)),
    disperso_ha       = ha(disperso),
    disperso_parches  = length(tam_fuera),
    disperso_mediana_ha = if (length(tam_fuera)) stats::median(tam_fuera) else NA_real_,
    cambio_parque_ha  = ha(cambio),
    nubes_parque_ha   = ha(is.na(d) & en_parque),
    fondo_mediana     = as.numeric(terra::global(terra::mask(d, parque_r,
                                                             maskvalues = 0),
                                                 \(v) stats::median(v, na.rm = TRUE)))
  )

  perimetro_sf <- sf::st_sf(
    evento = clave_evento,
    s2_previa = e$s2_previa, s2_post = e$s2_post,
    hectareas = round(cifras$perimetro_ha),
    geometry = sf::st_transform(perimetro, CRS_CRTM05)
  )
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(dest)) file.remove(dest)
  sf::st_write(perimetro_sf, dest, layer = "perimetro", quiet = TRUE)

  list(evento = e, perimetro = perimetro_sf, cifras = cifras, archivo = dest)
}

# Cifras de la cicatriz en CSV (formato largo, hectáreas con un decimal).
tabla_cicatriz_csv <- function(cicatriz, dest) {
  tabla <- tidyr::pivot_longer(cicatriz$cifras, dplyr::everything(),
                               names_to = "medida", values_to = "valor") |>
    dplyr::mutate(valor = round(valor, 3))
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(tabla, dest)
  dest
}
