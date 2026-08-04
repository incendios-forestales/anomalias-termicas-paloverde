# Video animado tipo cartel (estilo Milos Popovic): mapa oscuro con relieve
# sombreado, detecciones de fuego con resplandor, píxeles de área quemada,
# fecha animada y contadores acumulados. Un cuadro por mes, sobre el registro
# completo de la fuente (etiquetas y rango de años parametrizados por fuente
# vía etiquetas_video()).
#
# Decisiones metodológicas:
#
# - Render manual cuadro por cuadro (PNG numerados + av::av_encode_video) en
#   lugar de gganimate: los contadores, la fecha grande y el resplandor
#   multicapa cambian texto y número de capas en cada cuadro, cosas que
#   transition_manual no permite. El bucle manual además deja congelar el
#   cuadro final y renderizar cuadros sueltos para depurar.
#
# - Relieve: teselas Terrarium de AWS (s3://elevation-tiles-prod), públicas y
#   sin autenticación (a diferencia de LP DAAC, cuyo token expira ~60 días).
#   Cada PNG codifica la elevación en sus canales RGB:
#     elevación (m) = R * 256 + G + B / 256 - 32768
#   A zoom 13 la resolución es ~19 m/px en esta latitud, suficiente para un
#   lienzo de 1080 px sobre ~28 km.
#
# - El fondo (hillshade coloreado + máscara fuera del parque) se aplana UNA
#   sola vez a una imagen RGBA (mismo patrón que fondo_cobertura_animacion en
#   R/cobertura.R) y cada cuadro lo pinta con annotation_raster, que es un
#   blit barato; una capa raster de ggplot por cuadro haría lentísimos los
#   ~300 cuadros.
#
# - Colores: se conserva la semántica del proyecto (naranja = detecciones,
#   púrpura = área quemada, magnitudes complementarias que NUNCA se suman)
#   pero con más luminancia que COLOR_DETECCIONES/COLOR_AREA_QUEMADA, porque
#   el fondo oscuro exige colores brillantes. Cada contador hereda el matiz
#   de su capa —a diferencia del video de referencia, donde ambos son
#   naranja— para reforzar que son magnitudes distintas.

TERRARIUM_BASE <- "https://s3.amazonaws.com/elevation-tiles-prod/terrarium"

# Lienzo del video (px) y encuadre en CRTM05 (m). El encuadre centra el
# parque (x 345514-368174, y 1133956-1155588) y reserva arriba una banda de
# encabezado ENCABEZADO_PX para título, contadores y fecha.
ANCHO_PX      <- 1080
ALTO_PX       <- 1236
ENCABEZADO_PX <- 300
XLIM_VIDEO    <- c(343000, 370800)
YMIN_VIDEO    <- 1132726

FUENTE_VIDEO <- "Nimbus Sans"  # única sans con bold en rocker/geospatial

COLOR_FONDO_VIDEO  <- "#0d1520"  # azul marino casi negro
COLOR_TEXTO_VIDEO  <- "#e8edf2"  # texto principal
COLOR_TEXTO_SUAVE  <- "#93a1b0"  # rótulos secundarios y créditos
COLOR_RETICULA     <- "#2c3a4a"
COLOR_LIMITE_VIDEO <- "#8fa3b8"  # límite del parque
COLOR_FUEGO_NUCLEO <- "#ffe066"  # centro del resplandor
COLOR_FUEGO_HALO   <- "#ff8c1a"  # halo, estela y acentos naranja
COLOR_QUEMA_VIDEO  <- "#9d7bd8"  # púrpura claro (pariente de COLOR_AREA_QUEMADA)

# Alfa por antigüedad en meses (índice 1 = mes actual): el mes vigente pleno
# y una estela que se desvanece en los cinco meses siguientes.
ALFAS_ESTELA <- c(1, 0.55, 0.35, 0.22, 0.14, 0.08)

# Paleta oscura por clase WorldCover para el fondo del video (mismos códigos
# de píxel que CLASES_WORLDCOVER). No es la paleta oficial: sobre fondo
# oscuro los colores oficiales competirían con el naranja de las detecciones
# y el púrpura de las quemas, así que cada clase aporta solo un matiz tenue
# (verde = bosque, verde azulado = humedal/manglar, caqui = pastizal,
# azul = agua) y la luminancia la pone el hillshade.
PALETA_COBERTURA_VIDEO <- c(
  `10`  = "#26452e",  # Bosque
  `20`  = "#374430",  # Matorral
  `30`  = "#4a4832",  # Pastizal
  `40`  = "#453d2b",  # Cultivos
  `50`  = "#493a3c",  # Zonas construidas
  `60`  = "#463f35",  # Suelo desnudo
  `70`  = "#3c4348",  # Nieve y hielo (no ocurre)
  `80`  = "#183451",  # Cuerpos de agua
  `90`  = "#1c454c",  # Humedal herbáceo
  `95`  = "#1a5348",  # Manglar
  `100` = "#3f4436"   # Musgos y líquenes (no ocurre)
)

# Etiquetas geográficas (WGS84); se proyectan a CRTM05 en base_video().
# Posiciones con respaldo en datos: la laguna es el st_point_on_surface()
# del polígono "Humedal Palo Verde 1" del Registro Nacional de Humedales
# (SINAC); los dos refugios, el st_point_on_surface() de su polígono oficial
# de áreas silvestres protegidas recortado al encuadre del video; el
# Tempisque, el píxel de agua de WorldCover más cercano al centro de su
# tramo suroeste fuera del parque (cauce casi horizontal ahí: ángulo 0). El
# Bebedero y la estación biológica (OET) están curados a ojo sobre el cuadro
# de prueba.
# `hjust` ancla el texto (0,5 = centrado; 0 = a la derecha del punto) y
# `punto` dibuja además un marcador (para hitos puntuales como la estación).
ETIQUETAS_VIDEO <- data.frame(
  nombre = c("Laguna Palo Verde", "Río Tempisque", "Río Bebedero",
             "Lomas de Barbudal", "RNVS Mata Redonda", "RNVS Cipancí",
             "Estación Biológica Palo Verde"),
  lon    = c(-85.3424, -85.3441, -85.198, -85.365, -85.4205, -85.4303, -85.3365),
  lat    = c( 10.3343,  10.3180,  10.315,  10.445,  10.3207,  10.3331,  10.3450),
  angulo = c(0, 0, -75, 0, 0, 0, 0),
  # Los refugios se anclan a la derecha de su punto (hjust = 0): sus puntos
  # caen junto al borde izquierdo del lienzo y centrados quedarían recortados.
  hjust  = c(0.5, 0.5, 0.5, 0.5, 0, 0, 0),
  punto  = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
)

# --- Descarga y decodificación del DEM -------------------------------------

# Índices de tesela XYZ estándar (Web Mercator).
lon_a_tesela_x <- function(lon, zoom) floor((lon + 180) / 360 * 2^zoom)
lat_a_tesela_y <- function(lat, zoom) {
  floor((1 - asinh(tan(lat * pi / 180)) / pi) / 2 * 2^zoom)
}

# Descarga cacheada de las teselas Terrarium que cubren el bbox WGS84
# c(oeste, sur, este, norte). Retorna los PNG locales; los índices z/x/y
# quedan codificados en el nombre de archivo para la decodificación.
descargar_dem_terrarium <- function(bbox_wgs84, zoom = 13,
                                    dir_destino = "data/raw/dem/terrarium") {
  xs <- lon_a_tesela_x(bbox_wgs84["oeste"], zoom):lon_a_tesela_x(bbox_wgs84["este"], zoom)
  ys <- lat_a_tesela_y(bbox_wgs84["norte"], zoom):lat_a_tesela_y(bbox_wgs84["sur"], zoom)
  rejilla <- expand.grid(x = xs, y = ys)
  vapply(seq_len(nrow(rejilla)), function(i) {
    x <- rejilla$x[i]; y <- rejilla$y[i]
    download_if_missing(
      glue::glue("{TERRARIUM_BASE}/{zoom}/{x}/{y}.png"),
      file.path(dir_destino, glue::glue("z{zoom}_x{x}_y{y}.png"))
    )
  }, character(1))
}

# Una tesela PNG -> SpatRaster de elevación georreferenciado en EPSG:3857.
# El PNG no trae georreferencia: se calcula del índice de tesela (el origen
# XYZ es la esquina noroeste del mundo en Web Mercator).
decodificar_terrarium <- function(archivo, x, y, zoom) {
  bandas <- terra::rast(archivo)
  elevacion <- bandas[[1]] * 256 + bandas[[2]] + bandas[[3]] / 256 - 32768
  mundo <- 2 * pi * 6378137          # circunferencia en el ecuador (m)
  tam <- mundo / 2^zoom              # lado de la tesela (m)
  x0 <- -mundo / 2 + x * tam
  y1 <-  mundo / 2 - y * tam         # borde superior de la tesela
  terra::ext(elevacion) <- terra::ext(x0, x0 + tam, y1 - tam, y1)
  terra::crs(elevacion) <- "EPSG:3857"
  elevacion
}

# --- Geometría del lienzo ---------------------------------------------------

# Medidas del lienzo en coordenadas de datos (CRTM05). `px(n)` convierte
# píxeles a metros del lienzo, para posicionar texto de forma determinista.
layout_video <- function() {
  ancho_m <- XLIM_VIDEO[2] - XLIM_VIDEO[1]
  m_por_px <- ancho_m / ANCHO_PX
  alto_m <- m_por_px * ALTO_PX
  ymax <- YMIN_VIDEO + alto_m
  list(
    xlim = XLIM_VIDEO,
    ylim = c(YMIN_VIDEO, ymax),
    y_mapa = ymax - m_por_px * ENCABEZADO_PX,  # borde superior del mapa
    px = function(n) n * m_por_px
  )
}

# --- Fondo de relieve -------------------------------------------------------

# Compone el fondo del video: dentro del parque, el matiz viene de la clase
# de cobertura (WorldCover, PALETA_COBERTURA_VIDEO) y la luminancia del
# hillshade del DEM Terrarium; fuera, una rampa neutra atenuada (emula el
# "fuera de la región de interés" del estilo de referencia y deja que la
# cobertura del parque destaque). Retorna una imagen RGBA aplanada + su
# extensión, lista para annotation_raster (objeto plano, serializable como
# target rds).
fondo_relieve_video <- function(archivos_dem, parque, bbox_wgs84,
                                archivos_worldcover) {
  indices <- regmatches(basename(archivos_dem),
                        regexec("z(\\d+)_x(\\d+)_y(\\d+)\\.png", basename(archivos_dem)))
  teselas <- lapply(seq_along(archivos_dem), function(i) {
    z <- as.integer(indices[[i]][2])
    x <- as.integer(indices[[i]][3])
    y <- as.integer(indices[[i]][4])
    decodificar_terrarium(archivos_dem[i], x, y, z)
  })
  lay <- layout_video()
  dem <- terra::merge(terra::sprc(teselas)) |>
    terra::project(CRS_CRTM05) |>
    terra::crop(terra::ext(lay$xlim[1], lay$xlim[2], lay$ylim[1], lay$y_mapa)) |>
    # Las teselas Terrarium traen pequeñas discontinuidades en sus bordes que
    # el hillshade convierte en costuras horizontales; un promedio focal 3x3
    # las disimula sin borrar el relieve (~19 m/px).
    terra::focal(w = 3, fun = "mean", na.policy = "omit")

  pendiente   <- terra::terrain(dem, "slope", unit = "radians")
  orientacion <- terra::terrain(dem, "aspect", unit = "radians")
  sombra <- terra::shade(pendiente, orientacion, angle = 40, direction = 315)

  celdas <- terra::as.matrix(sombra, wide = TRUE)
  rango <- range(celdas, na.rm = TRUE)
  norma <- (celdas - rango[1]) / diff(rango)
  indice <- pmin(256L, 1L + floor(norma * 256))

  mascara <- terra::rasterize(terra::vect(a_crtm05(parque)), sombra)
  dentro <- !is.na(terra::as.matrix(mascara, wide = TRUE))

  # Cobertura alineada a la rejilla de la sombra (vecino más cercano en ambos
  # pasos: los códigos de clase no se interpolan).
  cobertura <- recortar_worldcover(archivos_worldcover, bbox_wgs84) |>
    terra::aggregate(fact = 2, fun = "modal", na.rm = TRUE) |>
    terra::project(CRS_CRTM05, method = "near") |>
    terra::resample(sombra, method = "near")
  clases <- terra::as.matrix(cobertura, wide = TRUE)

  # Dentro del parque: color base por clase, luminancia por la sombra
  # (0,6-1,4x, recortado al máximo del canal). Todos los vectores se aplanan
  # en el mismo orden (column-major), por lo que las posiciones coinciden.
  base_hex <- PALETA_COBERTURA_VIDEO[as.character(clases)]
  base_hex[is.na(base_hex)] <- COLOR_FONDO_VIDEO
  rgb_base <- grDevices::col2rgb(base_hex) / 255
  factor_luz <- 0.6 + 0.8 * as.vector(norma)
  factor_luz[is.na(factor_luz)] <- 1
  rgb_mod <- pmin(rgb_base * rep(factor_luz, each = 3), 1)
  hex_dentro <- grDevices::rgb(rgb_mod[1, ], rgb_mod[2, ], rgb_mod[3, ])

  rampa_fuera <- grDevices::colorRampPalette(c("#1a2430", "#2e3947"))(256)
  # El agua se tiñe también FUERA del parque: el Tempisque y el Bebedero lo
  # enmarcan (y dos etiquetas los nombran), pero con la rampa neutra sus
  # cauces desaparecían.
  es_agua <- !is.na(clases) & clases == 80
  colores <- ifelse(as.vector(dentro | es_agua), hex_dentro,
                    rampa_fuera[as.vector(indice)])
  colores[is.na(as.vector(celdas))] <- COLOR_FONDO_VIDEO

  extension <- terra::ext(sombra)
  list(
    imagen = grDevices::as.raster(matrix(colores, nrow = nrow(celdas))),
    xmin = extension$xmin, xmax = extension$xmax,
    ymin = extension$ymin, ymax = extension$ymax
  )
}


# --- Datos por cuadro -------------------------------------------------------

# Un renglón por cuadro (mes): etiqueta de fecha y contadores acumulados.
# La secuencia cubre la unión de ambas series mensuales: MCD64A1 se publica
# con rezago distinto al de FIRMS y sus últimos meses no coinciden; recortar
# a una sola serie dejaría píxeles quemados fuera del conteo final.
datos_cuadros_video <- function(firms_mensual, area_quemada_mensual) {
  meses <- seq(min(firms_mensual$aniomes, area_quemada_mensual$aniomes),
               max(firms_mensual$aniomes, area_quemada_mensual$aniomes),
               by = "month")
  tibble::tibble(aniomes = meses) |>
    dplyr::left_join(dplyr::select(firms_mensual, aniomes, detecciones),
                     by = "aniomes") |>
    dplyr::left_join(dplyr::select(area_quemada_mensual, aniomes, hectareas),
                     by = "aniomes") |>
    dplyr::mutate(
      detecciones = tidyr::replace_na(detecciones, 0L),
      hectareas   = tidyr::replace_na(hectareas, 0),
      detecciones_acum = cumsum(detecciones),
      hectareas_acum   = cumsum(hectareas),
      anio = as.integer(format(aniomes, "%Y")),
      etiqueta_fecha = paste(
        toupper(MESES_ES[as.integer(format(aniomes, "%m"))]), anio
      )
    )
}

# Textos del video y del cartel que dependen de la fuente de datos. El título
# deriva el rango de años de los datos (para MODIS reproduce el histórico
# "2001 - 2026"); el resto intercambia las siglas del sensor y del producto
# de área quemada en leyendas, créditos y rótulos de panel.
etiquetas_video <- function(anios, etiqueta_fuente = "MODIS",
                            id_fuente = "MODIS_SP", etiqueta_ba = "MCD64A1") {
  list(
    titulo = paste0("Anomalías Térmicas en el PN Palo Verde, ",
                    min(anios), " - ", max(anios)),
    deteccion = paste0("Detección de fuego activo (", etiqueta_fuente, ")"),
    quema = paste0("Píxel de área quemada (", etiqueta_ba, ")"),
    creditos = paste0("Datos: NASA FIRMS (", id_fuente, ") · NASA LP DAAC (",
                      etiqueta_ba, ") · SINAC"),
    panel_ha = paste0("hectáreas quemadas por mes (", etiqueta_ba, ")"),
    panel_detecciones = paste0("detecciones de fuego por mes (",
                               etiqueta_fuente, ")")
  )
}

# Contador estilo cartel: entero con separador de miles de espacio. Se aparta
# de num_es() a propósito: en un contador grande de cinco dígitos el bloque
# sin separador es ilegible, y la coma decimal española prohíbe usar el punto
# como separador de miles.
contador_es <- function(x) {
  format(round(x), big.mark = " ", trim = TRUE, scientific = FALSE)
}

# Mayúsculas espaciadas (sustituto tipográfico del letter-spacing, que el
# device png no ofrece).
esparcir <- function(x) gsub("(?<=.)(?=.)", " ", toupper(x), perl = TRUE)

# --- Composición del cuadro -------------------------------------------------

# Retícula manual de meridianos y paralelos recortada al área del mapa (una
# retícula de coord_sf invadiría la banda del encabezado). Retorna una lista
# con las líneas (sf) y los rótulos (data.frame en CRTM05).
reticula_video <- function(lay) {
  lons <- seq(-85.40, -85.20, by = 0.10)
  lats <- seq(10.25, 10.45, by = 0.10)
  marco <- sf::st_polygon(list(cbind(
    c(lay$xlim[1], lay$xlim[2], lay$xlim[2], lay$xlim[1], lay$xlim[1]),
    c(lay$ylim[1], lay$ylim[1], lay$y_mapa, lay$y_mapa, lay$ylim[1])
  ))) |> sf::st_sfc(crs = CRS_CRTM05)

  # Las líneas se densifican a mano (60 vértices) para que la curvatura de la
  # reproyección se conserve sin necesitar lwgeom (st_segmentize geográfico).
  linea <- function(coords) {
    sf::st_linestring(coords) |> sf::st_sfc(crs = CRS_WGS84) |>
      sf::st_transform(CRS_CRTM05)
  }
  lineas <- c(
    do.call(c, lapply(lons, function(l) {
      linea(cbind(l, seq(10.15, 10.55, length.out = 60)))
    })),
    do.call(c, lapply(lats, function(l) {
      linea(cbind(seq(-85.50, -85.10, length.out = 60), l))
    }))
  ) |> sf::st_intersection(marco)

  # Rótulos: meridianos abajo, paralelos a la izquierda (coma decimal).
  grados <- function(v) gsub("\\.", ",", sprintf("%.1f°", abs(v)))
  pos_lon <- sf::st_coordinates(sf::st_transform(
    sf::st_sfc(lapply(lons, function(l) sf::st_point(c(l, 10.25))), crs = CRS_WGS84),
    CRS_CRTM05))
  pos_lat <- sf::st_coordinates(sf::st_transform(
    sf::st_sfc(lapply(lats, function(l) sf::st_point(c(-85.40, l))), crs = CRS_WGS84),
    CRS_CRTM05))
  rotulos <- rbind(
    data.frame(x = pos_lon[, 1], y = lay$ylim[1] + lay$px(56),
               texto = paste0(grados(lons), " O"), angulo = 0),
    data.frame(x = lay$xlim[1] + lay$px(18), y = pos_lat[, 2],
               texto = paste0(grados(lats), " N"), angulo = 90)
  )
  list(lineas = lineas, rotulos = rotulos)
}

# Objeto ggplot con TODO lo estático (fondo, retícula, límite, etiquetas,
# encabezado fijo, leyenda, escala, norte y créditos). Cada cuadro se
# construye como `base + capas dinámicas`: sumar capas a un ggplot es una
# copia barata y evita reconstruir esto ~300 veces.
base_video <- function(relieve, parque, etiquetas) {
  lay <- layout_video()
  ret <- reticula_video(lay)

  etiquetas <- sf::st_as_sf(ETIQUETAS_VIDEO, coords = c("lon", "lat"),
                            crs = CRS_WGS84) |>
    sf::st_transform(CRS_CRTM05)
  pos_etiquetas <- cbind(sf::st_drop_geometry(etiquetas),
                         sf::st_coordinates(etiquetas))

  x0 <- lay$xlim[1] + lay$px(40)          # margen izquierdo del texto
  x1 <- lay$xlim[2] - lay$px(40)          # margen derecho
  y_desde_arriba <- function(n) lay$ylim[2] - lay$px(n)
  y_desde_abajo  <- function(n) lay$ylim[1] + lay$px(n)

  ggplot2::ggplot() +
    ggplot2::annotation_raster(relieve$imagen,
                               xmin = relieve$xmin, xmax = relieve$xmax,
                               ymin = relieve$ymin, ymax = relieve$ymax) +
    ggplot2::geom_sf(data = ret$lineas, color = COLOR_RETICULA,
                     linewidth = 0.25) +
    ggplot2::geom_sf(data = parque, fill = NA, color = COLOR_LIMITE_VIDEO,
                     linewidth = 0.45) +
    ggplot2::geom_text(data = ret$rotulos,
                       ggplot2::aes(x = x, y = y, label = texto, angle = angulo),
                       family = FUENTE_VIDEO, size = 2.4,
                       color = COLOR_TEXTO_SUAVE) +
    ggplot2::geom_point(data = pos_etiquetas[pos_etiquetas$punto, ],
                        ggplot2::aes(x = X, y = Y),
                        color = COLOR_TEXTO_SUAVE, size = 0.9) +
    # El texto de los hitos con marcador va desplazado a la derecha del punto.
    ggplot2::geom_text(data = pos_etiquetas,
                       ggplot2::aes(x = X + ifelse(hjust == 0, lay$px(10), 0),
                                    y = Y, label = nombre, angle = angulo,
                                    hjust = hjust),
                       family = FUENTE_VIDEO, fontface = "italic", size = 2.9,
                       color = COLOR_TEXTO_SUAVE) +
    # --- Encabezado (banda superior, coordenadas de datos) ---
    # Título fijo en una sola línea (el rango vive en el título); debajo va
    # lo que cambia por cuadro: acumulados grandes y, más abajo, el mes con
    # sus valores (ver cuadro_video()).
    ggplot2::annotate("text", x = x0, y = y_desde_arriba(70),
                      label = etiquetas$titulo,
                      family = FUENTE_VIDEO, fontface = "bold", size = 6.4,
                      hjust = 0, vjust = 0, color = COLOR_TEXTO_VIDEO) +
    ggplot2::annotate("text", x = x0, y = y_desde_arriba(176),
                      label = esparcir("hectáreas quemadas acumuladas"),
                      family = FUENTE_VIDEO, size = 2.7,
                      hjust = 0, vjust = 0.5, color = COLOR_TEXTO_SUAVE) +
    ggplot2::annotate("text", x = x0 + lay$px(560), y = y_desde_arriba(176),
                      label = esparcir("detecciones de fuego acumuladas"),
                      family = FUENTE_VIDEO, size = 2.7,
                      hjust = 0, vjust = 0.5, color = COLOR_TEXTO_SUAVE) +
    # --- Leyenda, escala, norte y créditos (dentro del mapa) ---
    ggplot2::annotate("point", x = x0, y = y_desde_abajo(106),
                      color = COLOR_FUEGO_NUCLEO, size = 1.8) +
    ggplot2::annotate("text", x = x0 + lay$px(16), y = y_desde_abajo(106),
                      label = etiquetas$deteccion,
                      family = FUENTE_VIDEO, size = 2.7,
                      hjust = 0, vjust = 0.5, color = COLOR_TEXTO_SUAVE) +
    ggplot2::annotate("tile", x = x0, y = y_desde_abajo(82),
                      width = lay$px(11), height = lay$px(11),
                      fill = COLOR_QUEMA_VIDEO, color = NA) +
    ggplot2::annotate("text", x = x0 + lay$px(16), y = y_desde_abajo(82),
                      label = etiquetas$quema,
                      family = FUENTE_VIDEO, size = 2.7,
                      hjust = 0, vjust = 0.5, color = COLOR_TEXTO_SUAVE) +
    # Mini-leyenda de las coberturas principales del fondo (los rótulos usan
    # los colores base de la paleta; en el mapa su luminancia varía con el
    # relieve). "Pastizal (marisma)" refleja el hallazgo del proyecto: lo que
    # WorldCover llama pastizal es en su mayoría el humedal estacional.
    ggplot2::annotate("tile", x = x0 + lay$px(295),
                      y = y_desde_abajo(c(106, 82)),
                      width = lay$px(11), height = lay$px(11),
                      fill = unname(PALETA_COBERTURA_VIDEO[c("10", "30")]),
                      color = NA) +
    ggplot2::annotate("text", x = x0 + lay$px(311),
                      y = y_desde_abajo(c(106, 82)),
                      label = c("Bosque", "Pastizal (marisma)"),
                      family = FUENTE_VIDEO, size = 2.7,
                      hjust = 0, vjust = 0.5, color = COLOR_TEXTO_SUAVE) +
    ggplot2::annotate("tile", x = x0 + lay$px(475),
                      y = y_desde_abajo(c(106, 82)),
                      width = lay$px(11), height = lay$px(11),
                      fill = unname(PALETA_COBERTURA_VIDEO[c("95", "80")]),
                      color = NA) +
    ggplot2::annotate("text", x = x0 + lay$px(491),
                      y = y_desde_abajo(c(106, 82)),
                      label = c("Manglar", "Agua"),
                      family = FUENTE_VIDEO, size = 2.7,
                      hjust = 0, vjust = 0.5, color = COLOR_TEXTO_SUAVE) +
    ggplot2::annotate("segment", x = x1 - lay$px(180) - 5000, xend = x1 - lay$px(180),
                      y = y_desde_abajo(82), yend = y_desde_abajo(82),
                      linewidth = 1.2, color = COLOR_TEXTO_SUAVE) +
    ggplot2::annotate("text", x = x1 - lay$px(180) - 2500, y = y_desde_abajo(98),
                      label = "5 km", family = FUENTE_VIDEO, size = 2.6,
                      hjust = 0.5, vjust = 0.5, color = COLOR_TEXTO_SUAVE) +
    ggplot2::annotate("polygon",
                      x = x1 - lay$px(40) + c(0, lay$px(9), -lay$px(9)),
                      y = lay$y_mapa - lay$px(52) + c(lay$px(22), 0, 0),
                      fill = COLOR_TEXTO_SUAVE) +
    ggplot2::annotate("text", x = x1 - lay$px(40), y = lay$y_mapa - lay$px(72),
                      label = "N", family = FUENTE_VIDEO, size = 3,
                      hjust = 0.5, vjust = 0.5, color = COLOR_TEXTO_SUAVE) +
    ggplot2::annotate("text", x = x0, y = y_desde_abajo(34),
                      label = etiquetas$creditos,
                      family = FUENTE_VIDEO, size = 2.3,
                      hjust = 0, vjust = 0.5, color = COLOR_TEXTO_SUAVE) +
    ggplot2::annotate("text", x = x0, y = y_desde_abajo(18),
                      label = "Fondo: ESA WorldCover 2021 · Terrain Tiles (Mapzen/AWS) · Estilo: Milos Popovic",
                      family = FUENTE_VIDEO, size = 2.3,
                      hjust = 0, vjust = 0.5, color = COLOR_TEXTO_SUAVE) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = COLOR_FONDO_VIDEO,
                                              color = NA),
      plot.margin = ggplot2::margin(0, 0, 0, 0)
    )
}

# Capas dinámicas de un mes: píxeles quemados (debajo) y detecciones (encima)
# del mes vigente y su estela. La estela son los meses anteriores con alfa
# decreciente (ALFAS_ESTELA); el mes vigente lleva además el resplandor
# multicapa (el mismo punto varias veces, de halo translúcido a núcleo
# brillante). Capa por antigüedad en vez de aes(alpha): evita escalas de
# identidad y deja cada alfa fijado explícitamente.
capas_mes_video <- function(puntos, quemas, mes_actual,
                            n_estela = length(ALFAS_ESTELA)) {
  edad <- function(aniomes) {
    (as.integer(format(mes_actual, "%Y")) - as.integer(format(aniomes, "%Y"))) * 12 +
    (as.integer(format(mes_actual, "%m")) - as.integer(format(aniomes, "%m")))
  }
  capas <- list()
  for (e in rev(seq_len(n_estela) - 1)) {          # de más viejo a más nuevo
    alfa <- ALFAS_ESTELA[e + 1]
    q <- quemas[edad(quemas$aniomes) == e, ]
    if (nrow(q) > 0) {
      capas <- c(capas, list(
        ggplot2::geom_sf(data = q, fill = grDevices::adjustcolor(
          COLOR_QUEMA_VIDEO, alpha.f = 0.85 * alfa), color = NA)
      ))
    }
  }
  for (e in rev(seq_len(n_estela) - 1)) {
    alfa <- ALFAS_ESTELA[e + 1]
    p <- puntos[edad(puntos$aniomes) == e, ]
    if (nrow(p) == 0) next
    if (e == 0) {
      capas <- c(capas, list(
        ggplot2::geom_sf(data = p, color = COLOR_FUEGO_HALO, size = 10, alpha = 0.08),
        ggplot2::geom_sf(data = p, color = COLOR_FUEGO_HALO, size = 6,   alpha = 0.22),
        ggplot2::geom_sf(data = p, color = COLOR_FUEGO_HALO, size = 3.2, alpha = 0.55),
        ggplot2::geom_sf(data = p, color = COLOR_FUEGO_NUCLEO, size = 1.5, alpha = 0.95)
      ))
    } else {
      capas <- c(capas, list(
        ggplot2::geom_sf(data = p, color = COLOR_FUEGO_HALO, size = 1.3,
                         alpha = alfa)
      ))
    }
  }
  capas
}

# Renderiza un cuadro: base + capas del mes + fecha y contadores, a PNG con
# el device png cairo (control exacto en píxeles; ggsave piensa en pulgadas).
#
# El coord_sf se aplica AQUÍ, al final: sumar una capa geom_sf a un ggplot ya
# armado hace que ggplot2 agregue automáticamente un coord_sf por defecto que
# reemplaza al configurado (y arruina el encuadre exacto del lienzo). Por eso
# base_video() no fija coordenadas y suppressMessages() silencia los avisos
# de reemplazo intermedios.
cuadro_video <- function(base, capas, info_mes, dest_png) {
  lay <- layout_video()
  x0 <- lay$xlim[1] + lay$px(40)
  x1 <- lay$xlim[2] - lay$px(40)
  # Jerarquía de los contadores: primero los acumulados desde 2001 en grande
  # (columnas rotuladas en base_video()) y debajo el renglón del mes: la
  # fecha y las hectáreas y detecciones de ese mes, con unidades explícitas.
  # En meses sin actividad los valores del mes van atenuados y sin "+".
  valor_mes <- function(x, unidad) {
    if (x > 0) paste0("+", contador_es(x), " ", unidad) else paste0("0 ", unidad)
  }
  color_mes <- function(x, color) if (x > 0) color else COLOR_TEXTO_SUAVE
  p <- suppressMessages(
    Reduce(`+`, capas, init = base) +
      ggplot2::annotate("text", x = x0, y = lay$ylim[2] - lay$px(140),
                        label = contador_es(info_mes$hectareas_acum),
                        family = FUENTE_VIDEO, fontface = "bold", size = 7.5,
                        hjust = 0, vjust = 0.5, color = COLOR_QUEMA_VIDEO) +
      ggplot2::annotate("text", x = x0 + lay$px(560), y = lay$ylim[2] - lay$px(140),
                        label = contador_es(info_mes$detecciones_acum),
                        family = FUENTE_VIDEO, fontface = "bold", size = 7.5,
                        hjust = 0, vjust = 0.5, color = COLOR_FUEGO_HALO) +
      ggplot2::annotate("text", x = x0, y = lay$ylim[2] - lay$px(240),
                        label = info_mes$etiqueta_fecha,
                        family = FUENTE_VIDEO, fontface = "bold", size = 5.5,
                        hjust = 0, vjust = 0.5, color = COLOR_TEXTO_VIDEO) +
      ggplot2::annotate("text", x = x0 + lay$px(260), y = lay$ylim[2] - lay$px(240),
                        label = valor_mes(info_mes$hectareas, "ha"),
                        family = FUENTE_VIDEO, fontface = "bold", size = 5.5,
                        hjust = 0, vjust = 0.5,
                        color = color_mes(info_mes$hectareas, COLOR_QUEMA_VIDEO)) +
      ggplot2::annotate("text", x = x0 + lay$px(560), y = lay$ylim[2] - lay$px(240),
                        label = valor_mes(info_mes$detecciones, "detecciones"),
                        family = FUENTE_VIDEO, fontface = "bold", size = 5.5,
                        hjust = 0, vjust = 0.5,
                        color = color_mes(info_mes$detecciones, COLOR_FUEGO_HALO)) +
      ggplot2::coord_sf(crs = sf::st_crs(CRS_CRTM05), datum = NA,
                        xlim = lay$xlim, ylim = lay$ylim,
                        expand = FALSE, clip = "off")
  )
  grDevices::png(dest_png, width = ANCHO_PX, height = ALTO_PX,
                 type = "cairo", res = 132)
  print(p)
  grDevices::dev.off()
  dest_png
}

# --- Cartel resumen (PNG) ---------------------------------------------------

# Cartel estático que resume las dos series del video con su mismo estilo
# (fondo, colores, tipografía, encabezado y créditos). Dos paneles apilados
# con eje x común — nunca un doble eje: hectáreas y detecciones son
# magnitudes no comparables — y un rótulo directo en el mes máximo de cada
# serie. Los colores de serie pasan la validación de daltonismo y contraste
# sobre el fondo oscuro (ΔE 27 protan, 3:1+); el naranja queda apenas sobre
# la banda de luminosidad recomendada, desviación aceptada por coherencia
# con el video y porque cada serie vive en su propio panel.
panel_cartel <- function(datos, columna, color, titulo) {
  imax <- which.max(datos[[columna]])
  temprano <- imax < nrow(datos) / 2
  etiqueta_max <- paste0(
    MESES_ES[as.integer(format(datos$aniomes[imax], "%m"))], " ",
    format(datos$aniomes[imax], "%Y"), ": ",
    contador_es(datos[[columna]][imax])
  )
  ggplot2::ggplot(datos, ggplot2::aes(x = aniomes, y = .data[[columna]])) +
    ggplot2::geom_col(fill = color, width = 25) +
    ggplot2::annotate("text",
                      x = datos$aniomes[imax] + if (temprano) 300 else -300,
                      y = datos[[columna]][imax],
                      label = etiqueta_max, hjust = if (temprano) 0 else 1,
                      vjust = 0.9, family = FUENTE_VIDEO, size = 2.9,
                      color = COLOR_TEXTO_VIDEO) +
    ggplot2::scale_x_date(date_breaks = "5 years", date_labels = "%Y",
                          expand = ggplot2::expansion(mult = 0.01)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.06))) +
    ggplot2::labs(title = esparcir(titulo), x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_family = FUENTE_VIDEO) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", color = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = COLOR_RETICULA,
                                                 linewidth = 0.3),
      axis.text  = ggplot2::element_text(color = COLOR_TEXTO_SUAVE, size = 7.5),
      plot.title = ggplot2::element_text(color = COLOR_TEXTO_SUAVE, size = 8.5),
      plot.margin = ggplot2::margin(6, 16, 4, 16)
    )
}

# Compone el cartel: encabezado y créditos del video (dibujados con grid) y
# los dos paneles ggplot apilados. Target con format = "file".
generar_cartel_resumen <- function(firms_mensual, area_quemada_mensual, dest,
                                   etiqueta_fuente = "MODIS",
                                   id_fuente = "MODIS_SP",
                                   etiqueta_ba = "MCD64A1") {
  datos <- datos_cuadros_video(firms_mensual, area_quemada_mensual)
  etiquetas <- etiquetas_video(datos$anio, etiqueta_fuente, id_fuente,
                               etiqueta_ba)
  total_ha  <- sum(datos$hectareas)
  total_det <- sum(datos$detecciones)

  p_ha  <- panel_cartel(datos, "hectareas", COLOR_QUEMA_VIDEO,
                        etiquetas$panel_ha)
  p_det <- panel_cartel(datos, "detecciones", COLOR_FUEGO_HALO,
                        etiquetas$panel_detecciones)

  # Coordenadas en píxeles: y positiva desde ARRIBA (encabezado) y negativa
  # desde ABAJO (créditos), misma convención visual que el video.
  x_px <- function(n) grid::unit(n / ANCHO_PX, "npc")
  y_px <- function(n) {
    grid::unit(if (n >= 0) 1 - n / ALTO_PX else -n / ALTO_PX, "npc")
  }
  texto <- function(etiqueta, x, y, pt, color, negrita = FALSE) {
    grid::grid.text(
      etiqueta, x = x_px(x), y = y_px(y), just = c("left", "bottom"),
      gp = grid::gpar(fontfamily = FUENTE_VIDEO, fontsize = pt, col = color,
                      fontface = if (negrita) "bold" else "plain")
    )
  }

  alto_encabezado <- 210
  alto_pie <- 70
  alto_panel <- (ALTO_PX - alto_encabezado - alto_pie) / 2

  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  grDevices::png(dest, width = ANCHO_PX, height = ALTO_PX,
                 type = "cairo", res = 132)
  grid::grid.newpage()
  grid::grid.rect(gp = grid::gpar(fill = COLOR_FONDO_VIDEO, col = NA))

  texto(etiquetas$titulo,
        40, 75, 18.2, COLOR_TEXTO_VIDEO, negrita = TRUE)
  texto(contador_es(total_ha), 40, 150, 21.3, COLOR_QUEMA_VIDEO, negrita = TRUE)
  texto(esparcir("hectáreas quemadas acumuladas"), 40, 180, 7.7,
        COLOR_TEXTO_SUAVE)
  texto(contador_es(total_det), 600, 150, 21.3, COLOR_FUEGO_HALO,
        negrita = TRUE)
  texto(esparcir("detecciones de fuego acumuladas"), 600, 180, 7.7,
        COLOR_TEXTO_SUAVE)

  imprimir <- function(p, y_centro_px) {
    print(p, vp = grid::viewport(
      x = 0.5, y = grid::unit(1 - y_centro_px / ALTO_PX, "npc"),
      width = 1, height = grid::unit(alto_panel / ALTO_PX, "npc")
    ))
  }
  imprimir(p_ha,  alto_encabezado + alto_panel / 2)
  imprimir(p_det, alto_encabezado + alto_panel * 1.5)

  texto(etiquetas$creditos,
        40, -40, 6.5, COLOR_TEXTO_SUAVE)
  texto("Estilo: Milos Popovic", 40, -22, 6.5, COLOR_TEXTO_SUAVE)
  grDevices::dev.off()
  dest
}

# --- Generación del video ---------------------------------------------------

# Renderiza los cuadros mensuales y ensambla el MP4 (target format = "file").
#   anios:       filtro opcional de años para pruebas (p. ej. 2008); los
#                contadores siguen siendo acumulados desde el inicio real.
#   dir_cuadros: si se indica, los PNG se conservan ahí para inspección;
#                por defecto van a un directorio temporal efímero.
# El último cuadro se congela `congelar_s` segundos repitiendo su ruta en la
# entrada de av (da tiempo de leer las cifras finales).
# Nota: MCD64A1 se publica con rezago mayor que FIRMS, por lo que el contador
# de hectáreas puede quedar plano en los meses finales.
generar_video_anomalias <- function(firms_parque, area_quemada_parque,
                                    firms_mensual, area_quemada_mensual,
                                    parque, relieve, dest,
                                    fps = 10, congelar_s = 2.5,
                                    anios = NULL, dir_cuadros = NULL,
                                    etiqueta_fuente = "MODIS",
                                    id_fuente = "MODIS_SP",
                                    etiqueta_ba = "MCD64A1") {
  datos <- datos_cuadros_video(firms_mensual, area_quemada_mensual)
  # El título conserva el rango completo del registro aunque `anios` filtre
  # los cuadros para pruebas.
  etiquetas <- etiquetas_video(datos$anio, etiqueta_fuente, id_fuente,
                               etiqueta_ba)
  if (!is.null(anios)) datos <- datos[datos$anio %in% anios, ]

  if (is.null(dir_cuadros)) dir_cuadros <- tempfile("cuadros_video_")
  dir.create(dir_cuadros, recursive = TRUE, showWarnings = FALSE)

  base <- base_video(relieve, parque, etiquetas)
  cuadros <- vapply(seq_len(nrow(datos)), function(i) {
    if (i %% 50 == 0) message(glue::glue("[video] cuadro {i}/{nrow(datos)}"))
    capas <- capas_mes_video(firms_parque, area_quemada_parque,
                             datos$aniomes[i])
    cuadro_video(base, capas, datos[i, ],
                 file.path(dir_cuadros, sprintf("cuadro_%04d.png", i)))
  }, character(1))

  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  av::av_encode_video(
    input = c(cuadros, rep(cuadros[length(cuadros)], round(fps * congelar_s))),
    output = dest, framerate = fps, verbose = FALSE
  )
  dest
}
