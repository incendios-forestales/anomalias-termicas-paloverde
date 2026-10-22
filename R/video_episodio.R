# Video de un episodio (estilo cartel, como R/video.R): el avance del fuego
# activo pase a pase sobre el sector del evento, con reloj local, noche y día,
# y al cierre las otras tres mediciones del episodio: el área quemada de
# 500 m, la cicatriz Sentinel-2 y la cifra oficial. Es la versión animada de
# la sección "Un episodio con cifra oficial" de la portada: las isócronas, la
# cicatriz y las cuatro cifras en ~30 segundos.
#
# Decisiones metodológicas:
#
# - Un cuadro cada PASO_EPISODIO_MIN minutos de tiempo real, no un cuadro por
#   pase. Los pases están repartidos de forma desigual (tres en 50 minutos al
#   mediodía, ninguno en diez horas de noche) y el ritmo del video debe ser el
#   del reloj: las noches se sienten largas y los mediodías, rápidos. Un pase
#   aparece en el primer cuadro posterior a su instante.
#
# - Los anillos se colorean por su EDAD en el instante del cuadro, de amarillo
#   casi blanco a brasa apagada (ESCALA_BRASA): fuego que se enfría. La figura
#   estática (grafico_isocronas_evento) usa inferno por hora de llegada; aquí
#   esa escala no sirve porque su extremo oscuro desaparece sobre el fondo
#   oscuro y porque el tiempo ya lo dan el reloj y la línea de tiempo.
#
# - La noche oscurece el FONDO (un rectángulo translúcido sobre el relieve,
#   con crepúsculos de una hora), no las capas de fuego: el fuego brilla igual
#   de noche. Los pases nocturnos se distinguen además en la línea de tiempo.
#
# - El cierre superpone, en orden, el área quemada de 500 m (púrpura, el color
#   de esa magnitud en toda la serie), el perímetro de la cicatriz (cian,
#   pariente claro de COLOR_CICATRIZ) y la cifra oficial como texto: el mismo
#   mensaje de "cuatro cifras que no se contradicen" de la portada, en un solo
#   cuadro final que se congela unos segundos.
#
# - Mismo lienzo, encabezado, tipografía y créditos que el video de 25 años
#   (layout_video, fondo_relieve_video y reticula_video con otro encuadre):
#   los dos videos se leen como una serie.

PASO_EPISODIO_MIN <- 30

# Cian claro para el perímetro de la cicatriz: COLOR_CICATRIZ (#0072B2) es
# demasiado oscuro sobre el fondo del video.
COLOR_CICATRIZ_VIDEO <- "#5fc9f5"
# Azul grisáceo para lo nocturno (pases de noche en la línea de tiempo,
# rótulo "noche").
COLOR_NOCHE_VIDEO <- "#8fa8c8"

# Color de un anillo según las horas transcurridas desde su pase: de amarillo
# casi blanco (recién detectado) a brasa apagada (cuatro días). Interpolación
# lineal por canal entre estos puntos.
ESCALA_BRASA <- data.frame(
  horas = c(0, 2, 8, 24, 60, 96),
  color = c("#fff4bf", "#ffe066", "#ff8c1a", "#c9471c", "#7f3024", "#4e2724")
)

# Nombres cortos de los días, indexados por lubridate::wday (1 = domingo).
DIAS_ES <- c("dom", "lun", "mar", "mié", "jue", "vie", "sáb")

color_brasa <- function(horas) {
  horas <- pmin(pmax(horas, 0), max(ESCALA_BRASA$horas))
  rgb <- grDevices::col2rgb(ESCALA_BRASA$color) / 255
  canal <- function(i) stats::approx(ESCALA_BRASA$horas, rgb[i, ], xout = horas)$y
  grDevices::rgb(canal(1), canal(2), canal(3))
}

# Polígono del sector del evento (Registro Nacional de Humedales), unido y en
# CRTM05. Lo comparten el encuadre, el fondo de relieve y las capas.
sector_evento <- function(clave_evento, humedales) {
  e <- evento(clave_evento)
  humedales[grepl(e$sector, humedales$nom_hum, fixed = TRUE), ] |>
    sf::st_transform(CRS_CRTM05) |>
    sf::st_union()
}

# Encuadre del episodio: el sector centrado en el área del mapa (debajo del
# encabezado) con un margen relativo, respetando la proporción del lienzo.
layout_episodio <- function(sector, margen = 0.12) {
  caja <- sf::st_bbox(sector)
  ancho <- unname(caja["xmax"] - caja["xmin"])
  alto <- unname(caja["ymax"] - caja["ymin"])
  aspecto <- ANCHO_PX / (ALTO_PX - ENCABEZADO_PX)
  ancho_m <- max(ancho, alto * aspecto) * (1 + 2 * margen)
  alto_mapa_m <- ancho_m / aspecto
  cx <- unname(mean(caja[c("xmin", "xmax")]))
  cy <- unname(mean(caja[c("ymin", "ymax")]))
  layout_video(xlim = c(cx - ancho_m / 2, cx + ancho_m / 2),
               ymin = cy - alto_mapa_m / 2)
}

# Meridianos y paralelos (múltiplos de `paso` grados) que cruzan el área del
# mapa de un encuadre.
pasos_reticula <- function(lay, paso = 0.05) {
  marco <- sf::st_bbox(c(xmin = lay$xlim[1], xmax = lay$xlim[2],
                         ymin = lay$ylim[1], ymax = lay$y_mapa),
                       crs = sf::st_crs(CRS_CRTM05)) |>
    sf::st_as_sfc() |>
    sf::st_transform(CRS_WGS84) |>
    sf::st_bbox()
  multiplos <- function(a, b) {
    seq(ceiling(a / paso) * paso, floor(b / paso) * paso, by = paso) |>
      round(6)
  }
  list(lons = multiplos(marco["xmin"], marco["xmax"]),
       lats = multiplos(marco["ymin"], marco["ymax"]))
}

# Opacidad del velo nocturno sobre el fondo según la hora local: plena de
# 18:30 a 05:00, con crepúsculos lineales de una hora (Guanacaste, fines de
# mayo: el sol sale ~05:15 y se pone ~17:55).
alfa_noche <- function(instante_local, plena = 0.45) {
  h <- lubridate::hour(instante_local) + lubridate::minute(instante_local) / 60
  fraccion <- ifelse(h < 5, 1,
              ifelse(h < 6, 6 - h,
              ifelse(h < 17.5, 0,
              ifelse(h < 18.5, h - 17.5, 1))))
  plena * fraccion
}

# --- Cuadros ------------------------------------------------------------------

# Un renglón por cuadro. La fase "episodio" recorre el tiempo real desde
# `previo_h` horas antes del primer pase (el sector intacto, para situar al
# espectador) hasta el último pase, de `paso_min` en `paso_min`. El cierre
# congela el reloj en el último pase y revela, con fundidos de `fundido_s`
# segundos separados por esperas de `espera_s`, el área quemada, la cicatriz y
# la cifra oficial; el cuadro final dura `final_s` segundos.
datos_cuadros_episodio <- function(pases, fps, paso_min = PASO_EPISODIO_MIN,
                                   previo_h = 1, espera_s = 1, fundido_s = 1,
                                   final_s = 4) {
  t0 <- min(pases$instante) - previo_h * 3600
  t1 <- max(pases$instante)
  instantes <- seq(t0, t1, by = paso_min * 60)
  if (max(instantes) < t1) instantes <- c(instantes, t1)
  n_espera <- round(fps * espera_s)
  n_fundido <- round(fps * fundido_s)
  n_final <- round(fps * final_s)
  rampa <- seq(1 / n_fundido, 1, length.out = n_fundido)
  cero <- function(n) rep(0, n)
  uno <- function(n) rep(1, n)

  episodio <- tibble::tibble(instante = instantes, alfa_quemas = 0,
                             alfa_cicatriz = 0, oficial = FALSE)
  cierre <- tibble::tibble(
    instante = t1,
    #                espera     quemas   espera     cicatriz  espera    final
    alfa_quemas   = c(cero(n_espera), rampa, uno(n_espera), uno(n_fundido),
                      uno(n_espera), uno(n_final)),
    alfa_cicatriz = c(cero(n_espera), cero(n_fundido), cero(n_espera), rampa,
                      uno(n_espera), uno(n_final)),
    oficial       = c(rep(FALSE, 3 * n_espera + 2 * n_fundido),
                      rep(TRUE, n_final))
  )
  dplyr::bind_rows(episodio, cierre) |>
    dplyr::mutate(local = lubridate::with_tz(instante, TZ_LOCAL),
                  cuadro = dplyr::row_number())
}

# Lo acumulado hasta el instante t: pases vistos, detecciones y hectáreas de
# la envolvente.
estado_episodio <- function(pases, t) {
  vistos <- pases[pases$instante <= t, ]
  list(
    pases = nrow(vistos),
    detecciones = sum(vistos$detecciones),
    hectareas = if (nrow(vistos) > 0) max(vistos$ha_acumuladas) else 0
  )
}

# Fecha compacta del reloj: "VIE 29 MAY 2026".
fecha_reloj <- function(local) {
  toupper(paste(DIAS_ES[lubridate::wday(local)], lubridate::day(local),
                MESES_ES[lubridate::month(local)], lubridate::year(local)))
}

# --- Composición ------------------------------------------------------------

# Textos fijos del video del episodio.
etiquetas_episodio <- function(isocronas, etiqueta_ba) {
  e <- isocronas$evento
  local <- isocronas$pases$instante_local
  list(
    titulo = paste0(toupper(substring(e$nombre, 1, 1)), substring(e$nombre, 2),
                    ", PN Palo Verde"),
    subtitulo = paste0("Fuego activo pase a pase · VIIRS ",
                       nombres_plataformas(isocronas), " · ",
                       fecha_es(min(local), con_anio = FALSE), " al ",
                       fecha_es(max(local)), " · hora local"),
    creditos = paste0("Datos: NASA FIRMS (", paste(isocronas$plataformas,
                                                   collapse = ", "),
                      ") · NASA LP DAAC (", etiqueta_ba,
                      ") · ESA Copernicus Sentinel-2 · SINAC"),
    sector = paste0(e$sector, " (Registro Nacional de Humedales)")
  )
}

# Posición horizontal de un instante en la línea de tiempo del encabezado.
x_linea_tiempo <- function(t, t_ini, t_fin, x0, x1) {
  x0 + (x1 - x0) * as.numeric(difftime(t, t_ini, units = "secs")) /
    as.numeric(difftime(t_fin, t_ini, units = "secs"))
}

# Objeto ggplot con todo lo estático: fondo, retícula, encabezado fijo (título,
# rótulos de los contadores, línea de tiempo con sus pases y medianoches),
# leyenda de las capas siempre presentes, escala, norte y créditos. Cada cuadro
# suma sus capas dinámicas encima (ver cuadro_episodio()).
base_episodio <- function(relieve, lay, etiquetas, pases, t_ini, t_fin) {
  pasos <- pasos_reticula(lay)
  ret <- reticula_video(lay, lons = pasos$lons, lats = pasos$lats)
  x0 <- lay$xlim[1] + lay$px(40)
  x1 <- lay$xlim[2] - lay$px(40)
  y_desde_arriba <- function(n) lay$ylim[2] - lay$px(n)
  y_desde_abajo  <- function(n) lay$ylim[1] + lay$px(n)
  y_linea <- y_desde_arriba(268)

  cortes <- cortes_dias(pases)
  medianoches <- data.frame(
    x = x_linea_tiempo(cortes$instantes, t_ini, t_fin, x0, x1),
    etiqueta = cortes$etiquetas
  )
  puntos_pases <- data.frame(
    x = x_linea_tiempo(pases$instante, t_ini, t_fin, x0, x1),
    color = ifelse(pases$nocturno, COLOR_NOCHE_VIDEO, COLOR_FUEGO_NUCLEO)
  )

  ggplot2::ggplot() +
    ggplot2::annotation_raster(relieve$imagen,
                               xmin = relieve$xmin, xmax = relieve$xmax,
                               ymin = relieve$ymin, ymax = relieve$ymax) +
    ggplot2::geom_sf(data = ret$lineas, color = COLOR_RETICULA,
                     linewidth = 0.25) +
    ggplot2::geom_text(data = ret$rotulos,
                       ggplot2::aes(x = x, y = y, label = texto, angle = angulo),
                       family = FUENTE_VIDEO, size = 2.4,
                       color = COLOR_TEXTO_SUAVE) +
    # --- Encabezado fijo ---
    ggplot2::annotate("text", x = x0, y = y_desde_arriba(70),
                      label = etiquetas$titulo,
                      family = FUENTE_VIDEO, fontface = "bold", size = 6.4,
                      hjust = 0, vjust = 0, color = COLOR_TEXTO_VIDEO) +
    ggplot2::annotate("text", x = x0, y = y_desde_arriba(97),
                      label = etiquetas$subtitulo,
                      family = FUENTE_VIDEO, size = 3.4,
                      hjust = 0, vjust = 0.5, color = COLOR_TEXTO_SUAVE) +
    ggplot2::annotate("text", x = x0 + lay$px(c(0, 360, 720)),
                      y = y_desde_arriba(176),
                      label = esparcir(c("hectáreas alcanzadas",
                                         "detecciones de fuego",
                                         "pases de satélite")),
                      family = FUENTE_VIDEO, size = 2.7,
                      hjust = 0, vjust = 0.5, color = COLOR_TEXTO_SUAVE) +
    # Línea de tiempo: el episodio completo, con una marca por pase (azul los
    # nocturnos) y las medianoches locales rotuladas.
    ggplot2::annotate("segment", x = x0, xend = x1, y = y_linea, yend = y_linea,
                      linewidth = 1.4, color = COLOR_RETICULA) +
    ggplot2::annotate("segment", x = medianoches$x, xend = medianoches$x,
                      y = y_linea - lay$px(5), yend = y_linea + lay$px(5),
                      linewidth = 0.5, color = COLOR_TEXTO_SUAVE) +
    ggplot2::annotate("text", x = medianoches$x, y = y_linea - lay$px(17),
                      label = medianoches$etiqueta,
                      family = FUENTE_VIDEO, size = 2.3,
                      hjust = 0.5, vjust = 0.5, color = COLOR_TEXTO_SUAVE) +
    ggplot2::annotate("point", x = puntos_pases$x, y = y_linea,
                      color = puntos_pases$color, size = 1.3) +
    # --- Leyenda de las capas siempre presentes (izquierda) ---
    ggplot2::annotate("point", x = x0, y = y_desde_abajo(130),
                      color = COLOR_FUEGO_NUCLEO, size = 1.8) +
    ggplot2::annotate("tile", x = x0, y = y_desde_abajo(106),
                      width = lay$px(11), height = lay$px(11),
                      fill = color_brasa(12), color = NA) +
    ggplot2::annotate("segment", x = x0 - lay$px(6), xend = x0 + lay$px(6),
                      y = y_desde_abajo(82), yend = y_desde_abajo(82),
                      color = COLOR_TEXTO_VIDEO, linewidth = 0.7,
                      linetype = "22") +
    ggplot2::annotate("text", x = x0 + lay$px(16),
                      y = y_desde_abajo(c(130, 106, 82)),
                      label = c("Detección de fuego activo (VIIRS)",
                                "Alcance del fuego activo: envolvente de los footprints",
                                etiquetas$sector),
                      family = FUENTE_VIDEO, size = 2.7,
                      hjust = 0, vjust = 0.5, color = COLOR_TEXTO_SUAVE) +
    # --- Escala, norte y créditos ---
    # La escala va arriba a la derecha, bajo el norte: la banda inferior del
    # mapa la ocupan las dos columnas de la leyenda.
    ggplot2::annotate("segment", x = x1 - 2000, xend = x1,
                      y = lay$y_mapa - lay$px(110), yend = lay$y_mapa - lay$px(110),
                      linewidth = 1.2, color = COLOR_TEXTO_SUAVE) +
    ggplot2::annotate("text", x = x1 - 1000, y = lay$y_mapa - lay$px(126),
                      label = "2 km", family = FUENTE_VIDEO, size = 2.6,
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

# Capas del mapa en el instante t: velo nocturno, límites, anillos coloreados
# por edad (el pase más reciente con borde claro), detecciones con resplandor
# durante `ventana_h` horas tras su pase, y las capas del cierre con su
# opacidad.
capas_episodio <- function(capas_fijas, isocronas, cuadro, lay,
                           ventana_h = 1.5) {
  t <- cuadro$instante
  alfa <- alfa_noche(cuadro$local)
  capas <- list(
    ggplot2::annotate("rect", xmin = lay$xlim[1], xmax = lay$xlim[2],
                      ymin = lay$ylim[1], ymax = lay$y_mapa,
                      fill = COLOR_FONDO_VIDEO, alpha = alfa),
    ggplot2::geom_sf(data = capas_fijas$parque,
                     color = COLOR_LIMITE_VIDEO, linewidth = 0.45),
    ggplot2::geom_sf(data = capas_fijas$sector, fill = NA,
                     color = COLOR_TEXTO_VIDEO, linewidth = 0.6,
                     linetype = "22")
  )

  anillos <- isocronas$anillos[isocronas$anillos$instante <= t, ]
  if (nrow(anillos) > 0) {
    edad <- as.numeric(difftime(t, anillos$instante, units = "hours"))
    capas <- c(capas, list(
      ggplot2::geom_sf(data = anillos, fill = color_brasa(edad), color = NA,
                       alpha = 0.9)
    ))
    recientes <- anillos[edad < PASO_EPISODIO_MIN / 60, ]
    if (nrow(recientes) > 0) {
      capas <- c(capas, list(
        ggplot2::geom_sf(data = recientes, fill = NA, color = "#fff4bf",
                         linewidth = 0.5)
      ))
    }
  }

  pases_activos <- isocronas$pases$pase[
    isocronas$pases$instante <= t &
      isocronas$pases$instante > t - ventana_h * 3600]
  puntos <- capas_fijas$detecciones[capas_fijas$detecciones$pase %in% pases_activos, ]
  if (nrow(puntos) > 0) {
    capas <- c(capas, list(
      ggplot2::geom_sf(data = puntos, color = COLOR_FUEGO_HALO, size = 10, alpha = 0.08),
      ggplot2::geom_sf(data = puntos, color = COLOR_FUEGO_HALO, size = 6,   alpha = 0.22),
      ggplot2::geom_sf(data = puntos, color = COLOR_FUEGO_HALO, size = 3.2, alpha = 0.55),
      ggplot2::geom_sf(data = puntos, color = COLOR_FUEGO_NUCLEO, size = 1.5, alpha = 0.95)
    ))
  }

  if (cuadro$alfa_quemas > 0) {
    capas <- c(capas, list(
      ggplot2::geom_sf(data = capas_fijas$quemas,
                       fill = grDevices::adjustcolor(COLOR_QUEMA_VIDEO,
                                                     alpha.f = 0.6 * cuadro$alfa_quemas),
                       color = NA)
    ))
  }
  if (cuadro$alfa_cicatriz > 0) {
    capas <- c(capas, list(
      ggplot2::geom_sf(data = capas_fijas$perimetro, fill = NA,
                       color = grDevices::adjustcolor(COLOR_CICATRIZ_VIDEO,
                                                      alpha.f = cuadro$alfa_cicatriz),
                       linewidth = 0.9)
    ))
  }
  capas
}

# Renderiza un cuadro: base + capas del instante + contadores, reloj, marca en
# la línea de tiempo y leyenda del cierre. Mismas convenciones que
# cuadro_video() (coord_sf al final, device png cairo).
cuadro_episodio <- function(base, capas, cuadro, estado, cifras, lay,
                            t_ini, t_fin, dest_png) {
  x0 <- lay$xlim[1] + lay$px(40)
  x1 <- lay$xlim[2] - lay$px(40)
  y_desde_arriba <- function(n) lay$ylim[2] - lay$px(n)
  y_desde_abajo  <- function(n) lay$ylim[1] + lay$px(n)
  y_linea <- y_desde_arriba(268)
  x_ahora <- x_linea_tiempo(cuadro$instante, t_ini, t_fin, x0, x1)
  de_noche <- alfa_noche(cuadro$local) > 0.5 * 0.45
  x_derecha <- x0 + lay$px(540)
  con_alfa <- function(color, alfa) grDevices::adjustcolor(color, alpha.f = alfa)

  leyenda_cierre <- list()
  if (cuadro$alfa_quemas > 0) {
    leyenda_cierre <- c(leyenda_cierre, list(
      # Un fondo translúcido detrás de la columna derecha: ahí cruzan el
      # límite del parque y el del sector, y el texto se perdería.
      ggplot2::annotate("rect", xmin = x_derecha - lay$px(14),
                        xmax = x_derecha + lay$px(275),
                        ymin = y_desde_abajo(68), ymax = y_desde_abajo(144),
                        fill = COLOR_FONDO_VIDEO,
                        alpha = 0.75 * cuadro$alfa_quemas),
      ggplot2::annotate("tile", x = x_derecha, y = y_desde_abajo(106),
                        width = lay$px(11), height = lay$px(11),
                        fill = con_alfa(COLOR_QUEMA_VIDEO, cuadro$alfa_quemas),
                        color = NA),
      ggplot2::annotate("text", x = x_derecha + lay$px(16), y = y_desde_abajo(106),
                        label = cifras$quemas,
                        family = FUENTE_VIDEO, size = 2.7, hjust = 0, vjust = 0.5,
                        color = con_alfa(COLOR_TEXTO_SUAVE, cuadro$alfa_quemas))
    ))
  }
  if (cuadro$alfa_cicatriz > 0) {
    leyenda_cierre <- c(leyenda_cierre, list(
      ggplot2::annotate("segment", x = x_derecha - lay$px(6),
                        xend = x_derecha + lay$px(6),
                        y = y_desde_abajo(82), yend = y_desde_abajo(82),
                        color = con_alfa(COLOR_CICATRIZ_VIDEO, cuadro$alfa_cicatriz),
                        linewidth = 0.9),
      ggplot2::annotate("text", x = x_derecha + lay$px(16), y = y_desde_abajo(82),
                        label = cifras$cicatriz,
                        family = FUENTE_VIDEO, size = 2.7, hjust = 0, vjust = 0.5,
                        color = con_alfa(COLOR_TEXTO_SUAVE, cuadro$alfa_cicatriz))
    ))
  }
  if (cuadro$oficial) {
    leyenda_cierre <- c(leyenda_cierre, list(
      ggplot2::annotate("text", x = x_derecha + lay$px(16), y = y_desde_abajo(130),
                        label = cifras$oficial,
                        family = FUENTE_VIDEO, fontface = "bold", size = 2.7,
                        hjust = 0, vjust = 0.5, color = COLOR_TEXTO_VIDEO)
    ))
  }

  p <- suppressMessages(
    Reduce(`+`, c(capas, leyenda_cierre), init = base) +
      # Contadores
      ggplot2::annotate("text", x = x0, y = y_desde_arriba(140),
                        label = contador_es(estado$hectareas),
                        family = FUENTE_VIDEO, fontface = "bold", size = 7.5,
                        hjust = 0, vjust = 0.5, color = COLOR_FUEGO_HALO) +
      ggplot2::annotate("text", x = x0 + lay$px(360), y = y_desde_arriba(140),
                        label = contador_es(estado$detecciones),
                        family = FUENTE_VIDEO, fontface = "bold", size = 7.5,
                        hjust = 0, vjust = 0.5, color = COLOR_FUEGO_NUCLEO) +
      ggplot2::annotate("text", x = x0 + lay$px(720), y = y_desde_arriba(140),
                        label = contador_es(estado$pases),
                        family = FUENTE_VIDEO, fontface = "bold", size = 7.5,
                        hjust = 0, vjust = 0.5, color = COLOR_TEXTO_VIDEO) +
      # Reloj
      ggplot2::annotate("text", x = x0, y = y_desde_arriba(222),
                        label = fecha_reloj(cuadro$local),
                        family = FUENTE_VIDEO, fontface = "bold", size = 5.5,
                        hjust = 0, vjust = 0.5, color = COLOR_TEXTO_VIDEO) +
      ggplot2::annotate("text", x = x0 + lay$px(360), y = y_desde_arriba(222),
                        label = format(cuadro$local, "%H:%M"),
                        family = FUENTE_VIDEO, fontface = "bold", size = 5.5,
                        hjust = 0, vjust = 0.5, color = COLOR_TEXTO_VIDEO) +
      ggplot2::annotate("text", x = x0 + lay$px(720), y = y_desde_arriba(222),
                        label = esparcir(if (de_noche) "noche" else "día"),
                        family = FUENTE_VIDEO, size = 3.4,
                        hjust = 0, vjust = 0.5,
                        color = if (de_noche) COLOR_NOCHE_VIDEO else COLOR_FUEGO_NUCLEO) +
      # Línea de tiempo: lo transcurrido y la marca del instante
      ggplot2::annotate("segment", x = x0, xend = x_ahora,
                        y = y_linea, yend = y_linea,
                        linewidth = 1.4, color = COLOR_FUEGO_HALO) +
      ggplot2::annotate("point", x = x_ahora, y = y_linea,
                        color = COLOR_TEXTO_VIDEO, size = 2.6) +
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

# --- Generación -------------------------------------------------------------

# Renderiza los cuadros y ensambla el MP4 (target format = "file").
#   isocronas:   salida de isocronas_evento() (pases, anillos, detecciones).
#   cicatriz:    salida de cicatriz_evento() (perímetro y cifras).
#   quemas:      píxeles de área quemada del evento (quemas_evento()) de la
#                plataforma cuyo producto se cita (etiqueta_ba).
#   relieve:     fondo de fondo_relieve_video() con el encuadre del episodio.
#   dir_cuadros: si se indica, los PNG se conservan ahí para inspección.
generar_video_episodio <- function(isocronas, cicatriz, quemas, parque,
                                   humedales, relieve, dest, etiqueta_ba,
                                   fps = 10, dir_cuadros = NULL) {
  e <- isocronas$evento
  pases <- isocronas$pases
  sector <- sector_evento(e$clave, humedales)
  lay <- layout_episodio(sector)
  etiquetas <- etiquetas_episodio(isocronas, etiqueta_ba)
  cuadros <- datos_cuadros_episodio(pases, fps)
  t_ini <- min(cuadros$instante)
  t_fin <- max(pases$instante)

  # El límite del parque se recorta al área del mapa como LÍNEA: con
  # clip = "off" un polígono se dibujaría sobre el encabezado, y recortar el
  # polígono añadiría un borde horizontal falso bajo el encabezado.
  marco_mapa <- sf::st_bbox(c(xmin = lay$xlim[1], xmax = lay$xlim[2],
                              ymin = lay$ylim[1], ymax = lay$y_mapa),
                            crs = sf::st_crs(CRS_CRTM05))
  capas_fijas <- list(
    parque = sf::st_transform(parque, CRS_CRTM05) |>
      sf::st_geometry() |>
      sf::st_boundary() |>
      sf::st_crop(marco_mapa),
    sector = sector,
    detecciones = sf::st_transform(isocronas$detecciones, CRS_CRTM05),
    quemas = sf::st_transform(quemas, CRS_CRTM05),
    perimetro = sf::st_transform(cicatriz$perimetro, CRS_CRTM05)
  )
  ha_quemas <- sum(sf::st_drop_geometry(quemas)$area_ha)
  cifras <- list(
    quemas = paste0("Área quemada (", etiqueta_ba, "): ",
                    contador_es(ha_quemas), " ha"),
    cicatriz = paste0("Cicatriz (Sentinel-2, dNBR): ",
                      contador_es(cicatriz$cifras$perimetro_ha), " ha"),
    oficial = paste0("Cifra oficial (", e$fuente_cifra, "): ",
                     contador_es(e$ha_oficial), " ha")
  )

  if (is.null(dir_cuadros)) dir_cuadros <- tempfile("cuadros_episodio_")
  dir.create(dir_cuadros, recursive = TRUE, showWarnings = FALSE)

  base <- base_episodio(relieve, lay, etiquetas, pases, t_ini, t_fin)
  rutas <- vapply(seq_len(nrow(cuadros)), function(i) {
    if (i %% 50 == 0) message(glue::glue("[video] cuadro {i}/{nrow(cuadros)}"))
    cuadro <- cuadros[i, ]
    cuadro_episodio(
      base, capas_episodio(capas_fijas, isocronas, cuadro, lay), cuadro,
      estado_episodio(pases, cuadro$instante), cifras, lay, t_ini, t_fin,
      file.path(dir_cuadros, sprintf("cuadro_%04d.png", i))
    )
  }, character(1))

  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  av::av_encode_video(input = rutas, output = dest, framerate = fps,
                      verbose = FALSE)
  dest
}
