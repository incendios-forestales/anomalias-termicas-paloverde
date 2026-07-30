# Video animado tipo cartel (estilo Milos Popovic): mapa oscuro con relieve
# sombreado, detecciones de fuego con resplandor, píxeles de área quemada,
# fecha animada y contadores acumulados. Un cuadro por mes, 2001-2026.
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

# Etiquetas geográficas (WGS84); se proyectan a CRTM05 en base_video().
ETIQUETAS_VIDEO <- data.frame(
  nombre = c("Laguna Palo Verde", "Río Tempisque", "Lomas de Barbudal"),
  lon    = c(-85.345, -85.400, -85.365),
  lat    = c( 10.348,  10.305,  10.445),
  angulo = c(0, -55, 0)
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

# Compone el fondo del video: hillshade del DEM Terrarium coloreado con una
# rampa azul oscuro dentro del parque y una rampa atenuada más clara fuera
# (emula el "fuera de la región de interés" del estilo de referencia).
# Retorna una imagen RGBA aplanada + su extensión, lista para
# annotation_raster (objeto plano, serializable como target rds).
fondo_relieve_video <- function(archivos_dem, parque, bbox_wgs84) {
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
  indice <- pmin(256L, 1L + floor((celdas - rango[1]) / diff(rango) * 256))

  mascara <- terra::rasterize(terra::vect(a_crtm05(parque)), sombra)
  dentro <- !is.na(terra::as.matrix(mascara, wide = TRUE))

  rampa_dentro <- grDevices::colorRampPalette(c("#0d1520", "#3d4a5c"))(256)
  rampa_fuera  <- grDevices::colorRampPalette(c("#1a2430", "#2e3947"))(256)
  colores <- ifelse(dentro, rampa_dentro[indice], rampa_fuera[indice])
  colores[is.na(celdas)] <- COLOR_FONDO_VIDEO

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
base_video <- function(relieve, parque) {
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
    ggplot2::geom_text(data = pos_etiquetas,
                       ggplot2::aes(x = X, y = Y, label = nombre, angle = angulo),
                       family = FUENTE_VIDEO, fontface = "italic", size = 2.9,
                       color = COLOR_TEXTO_SUAVE) +
    # --- Encabezado (banda superior, coordenadas de datos) ---
    ggplot2::annotate("text", x = x0, y = y_desde_arriba(95),
                      label = "INCENDIOS EN PALO VERDE",
                      family = FUENTE_VIDEO, fontface = "bold", size = 8.2,
                      hjust = 0, vjust = 0, color = COLOR_TEXTO_VIDEO) +
    ggplot2::annotate("segment", x = x0, xend = x0 + lay$px(210),
                      y = y_desde_arriba(112), yend = y_desde_arriba(112),
                      linewidth = 1.6, color = COLOR_FUEGO_HALO) +
    ggplot2::annotate("text", x = x0, y = y_desde_arriba(160),
                      label = esparcir("2001 - 2026 · Parque Nacional, Costa Rica"),
                      family = FUENTE_VIDEO, size = 3.1,
                      hjust = 0, vjust = 0.5, color = COLOR_TEXTO_SUAVE) +
    ggplot2::annotate("text", x = x0, y = y_desde_arriba(262),
                      label = esparcir("hectáreas quemadas (MCD64A1)"),
                      family = FUENTE_VIDEO, size = 2.6,
                      hjust = 0, vjust = 0.5, color = COLOR_TEXTO_SUAVE) +
    ggplot2::annotate("text", x = x0 + lay$px(560), y = y_desde_arriba(262),
                      label = esparcir("detecciones MODIS"),
                      family = FUENTE_VIDEO, size = 2.6,
                      hjust = 0, vjust = 0.5, color = COLOR_TEXTO_SUAVE) +
    # --- Leyenda, escala, norte y créditos (dentro del mapa) ---
    ggplot2::annotate("point", x = x0, y = y_desde_abajo(106),
                      color = COLOR_FUEGO_NUCLEO, size = 1.8) +
    ggplot2::annotate("text", x = x0 + lay$px(16), y = y_desde_abajo(106),
                      label = "Detección de fuego activo (MODIS)",
                      family = FUENTE_VIDEO, size = 2.7,
                      hjust = 0, vjust = 0.5, color = COLOR_TEXTO_SUAVE) +
    ggplot2::annotate("tile", x = x0, y = y_desde_abajo(82),
                      width = lay$px(11), height = lay$px(11),
                      fill = COLOR_QUEMA_VIDEO, color = NA) +
    ggplot2::annotate("text", x = x0 + lay$px(16), y = y_desde_abajo(82),
                      label = "Píxel de área quemada (MCD64A1)",
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
    ggplot2::annotate("text", x = x0, y = y_desde_abajo(20),
                      label = paste("Datos: NASA FIRMS (MODIS_SP) · NASA LP DAAC (MCD64A1) · SINAC",
                                    "· Relieve: Terrain Tiles (Mapzen/AWS) · Estilo: Milos Popovic"),
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
  p <- suppressMessages(
    Reduce(`+`, capas, init = base) +
      ggplot2::annotate("text", x = x1, y = lay$ylim[2] - lay$px(95),
                        label = info_mes$etiqueta_fecha,
                        family = FUENTE_VIDEO, fontface = "bold", size = 6.8,
                        hjust = 1, vjust = 0, color = COLOR_TEXTO_VIDEO) +
      ggplot2::annotate("text", x = x0, y = lay$ylim[2] - lay$px(225),
                        label = contador_es(info_mes$hectareas_acum),
                        family = FUENTE_VIDEO, fontface = "bold", size = 7,
                        hjust = 0, vjust = 0.5, color = COLOR_QUEMA_VIDEO) +
      ggplot2::annotate("text", x = x0 + lay$px(560), y = lay$ylim[2] - lay$px(225),
                        label = contador_es(info_mes$detecciones_acum),
                        family = FUENTE_VIDEO, fontface = "bold", size = 7,
                        hjust = 0, vjust = 0.5, color = COLOR_FUEGO_HALO) +
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
generar_video_incendios <- function(firms_parque, area_quemada_parque,
                                    firms_mensual, area_quemada_mensual,
                                    parque, relieve, dest,
                                    fps = 10, congelar_s = 2.5,
                                    anios = NULL, dir_cuadros = NULL) {
  datos <- datos_cuadros_video(firms_mensual, area_quemada_mensual)
  if (!is.null(anios)) datos <- datos[datos$anio %in% anios, ]

  if (is.null(dir_cuadros)) dir_cuadros <- tempfile("cuadros_video_")
  dir.create(dir_cuadros, recursive = TRUE, showWarnings = FALSE)

  base <- base_video(relieve, parque)
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
