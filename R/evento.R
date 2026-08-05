# Eventos documentados: incendios con un registro externo (periodístico o
# institucional) contra el cual contrastar lo que ven los satélites.
#
# El valor de un evento así no es narrar la noticia sino calibrar los
# productos: se conoce la fecha de ignición, la causa y una cifra oficial de
# superficie, de modo que la brecha entre esa cifra y las hectáreas del
# producto de área quemada se vuelve medible en lugar de conjetural.
#
# El catálogo es una tabla porque los eventos se acumulan: agregar una fila
# basta para que las cuatro plataformas puedan reportarlo, igual que PLATAFORMAS
# (R/plataformas.R) mantiene a los sensores como hermanos.
#
# Columnas:
#   clave        identificador corto
#   nombre       cómo se rotula el evento en títulos y encabezados. La
#                terminología del proyecto es "anomalías térmicas", "área
#                quemada" y "episodio": la palabra "incendio" solo aparece en
#                la prosa cuando se atribuye a la fuente externa, que sí
#                dispone de una confirmación en tierra
#   inicio/fin   ventana de detecciones que se le atribuye (fechas de paso del
#                satélite, no de la ignición: `ignicion` va aparte)
#   ignicion     fecha de inicio según la fuente externa
#   sector       patrón que identifica al polígono del Registro Nacional de
#                Humedales donde ocurrió (se compara con nom_hum)
#   ha_oficial   superficie afectada según la fuente externa
#   causa        causa reportada, en una frase
#   combustible  vegetación que ardió según la fuente externa
#   fuente_*     medio, enlace y fecha de publicación
EVENTOS <- tibble::tribble(
  ~clave,           ~nombre,                          ~inicio,                 ~fin,                    ~ignicion,               ~sector,           ~ha_oficial, ~causa,                 ~combustible,
  "catalina_2026",  "episodio del humedal Catalina",  as.Date("2026-05-29"),   as.Date("2026-06-02"),   as.Date("2026-05-28"),   "Sector Catalina", 4000,        "la caída de un rayo",  "*Typha*"
) |>
  dplyr::mutate(
    fuente_medio  = "Delfino.cr",
    fuente_url    = paste0("https://delfino.cr/2026/06/incendio-en-parque-",
                           "nacional-palo-verde-afecto-cerca-de-4000-hectareas"),
    fuente_fecha  = as.Date("2026-06-03"),
    fuente_cifra  = "Minae-Sinac"
  )

# Fila de EVENTOS, con error claro si la clave no existe (mismo criterio que
# plataforma()).
evento <- function(clave) {
  fila <- EVENTOS[EVENTOS$clave == clave, ]
  if (nrow(fila) != 1) {
    stop("Evento desconocido: '", clave, "'. Definidos: ",
         paste(EVENTOS$clave, collapse = ", "), call. = FALSE)
  }
  fila
}

# Detecciones de la plataforma atribuidas al evento: las que caen dentro del
# parque en la ventana de fechas. El recorte espacial ya lo hizo
# recortar_al_parque(); no se restringe además al polígono del sector, porque
# saber qué proporción cayó fuera de él es justamente uno de los resultados.
detecciones_evento <- function(clave_evento, firms_parque) {
  e <- evento(clave_evento)
  firms_parque[firms_parque$acq_date >= e$inicio &
                 firms_parque$acq_date <= e$fin, ]
}

# Píxeles de área quemada atribuidos al evento. A diferencia de las
# detecciones, el filtro es por MES y no por día: la fecha de un píxel de
# MCD64A1/VNP64A1 es la estimación algorítmica del día de la quema a partir del
# cambio de reflectancia, con incertidumbre de varios días —en Palo Verde 2026
# hay píxeles fechados antes de la ignición reportada—, así que recortar al día
# exacto descartaría parte de la misma cicatriz.
quemas_evento <- function(clave_evento, area_quemada_parque) {
  e <- evento(clave_evento)
  meses <- seq(lubridate::floor_date(e$inicio, "month"),
               lubridate::floor_date(e$fin, "month"), by = "month")
  area_quemada_parque[lubridate::floor_date(area_quemada_parque$fecha,
                                            "month") %in% meses, ]
}

# Cifras del evento para la prosa del reporte, calculadas desde los targets de
# UNA plataforma. Se calculan en lugar de escribirse porque cada plataforma da
# las suyas y porque cambian al reprocesarse los datos provisionales a
# estándar. Los porcentajes vienen formateados en español (num_es); los
# conteos, las hectáreas y las fechas van crudos para que la prosa los componga.
#
# `cobertura` y `humedales_detecciones` se indexan por id_deteccion, que es la
# posición de la fila en firms_parque: de ahí el which() sobre la ventana en
# lugar de un join por fecha.
resumen_evento <- function(clave_evento, firms_parque, firms_mensual,
                           area_quemada_parque, cobertura,
                           humedales_detecciones, clase_objetivo = "Pastizal") {
  e <- evento(clave_evento)
  fechas <- sf::st_drop_geometry(firms_parque)$acq_date
  ids <- which(fechas >= e$inicio & fechas <= e$fin)
  detecciones <- sf::st_drop_geometry(firms_parque)[ids, ]
  quemas <- sf::st_drop_geometry(quemas_evento(clave_evento,
                                               area_quemada_parque))

  # Sector del evento según el Registro Nacional de Humedales. nom_hum es el
  # polígono con mayor traslape del footprint (ver cruce_humedales()), así que
  # esto cuenta detecciones cuyo footprint está DOMINADO por el sector.
  en_sector <- humedales_detecciones$nom_hum[ids]
  en_sector <- !is.na(en_sector) & grepl(e$sector, en_sector, fixed = TRUE)

  # Clase dominante de WorldCover bajo los footprints del evento.
  clases <- clase_dominante(cobertura)
  clases <- clases[match(ids, clases$id_deteccion), ]

  # Posición del mes pico del evento en toda la serie mensual de la plataforma:
  # el dato que dice si el evento es excepcional o rutinario para ese sensor.
  mensual <- firms_mensual[!is.na(firms_mensual$detecciones), ]
  mes_evento <- lubridate::floor_date(e$inicio, "month")
  n_mes <- sum(mensual$detecciones[mensual$aniomes == mes_evento])
  rango_meses <- seq(lubridate::floor_date(e$inicio, "month"),
                     lubridate::floor_date(e$fin, "month"), by = "month")

  ha <- sum(quemas$area_ha)
  # Días entre la ignición reportada y el primer paso del satélite que la
  # registró: la latencia real del monitoreo satelital para este evento.
  latencia <- if (nrow(detecciones)) {
    as.integer(min(detecciones$acq_date) - e$ignicion)
  } else NA_integer_

  list(
    evento = e,
    n = nrow(detecciones),
    n_dias = dplyr::n_distinct(detecciones$acq_date),
    primera = if (nrow(detecciones)) min(detecciones$acq_date) else NA,
    ultima = if (nrow(detecciones)) max(detecciones$acq_date) else NA,
    latencia = latencia,
    latencia_prosa = if (is.na(latencia)) NA_character_ else {
      paste(latencia, if (latencia == 1) "día" else "días")
    },
    frp_total = round(sum(detecciones$frp, na.rm = TRUE)),
    frp_maximo = if (nrow(detecciones)) max(detecciones$frp, na.rm = TRUE)
                 else NA_real_,
    n_nocturnas = sum(detecciones$daynight == "N"),
    por_dia = dplyr::count(detecciones, acq_date, name = "detecciones"),
    n_sector = sum(en_sector),
    pct_sector = num_es(100 * mean(en_sector)),
    pct_clase = num_es(100 * mean(clases$clase == clase_objetivo)),
    clase_objetivo = clase_objetivo,
    hectareas = round(ha),
    meses_quemas = rango_meses,
    # Cuánto se queda corto el producto de área quemada frente a la cifra
    # oficial. Con 0 ha (producto aún no publicado para esos meses) el cociente
    # sería Inf: se devuelve NA para que la prosa lo trate como "sin dato".
    veces_menos = if (ha > 0) num_es(e$ha_oficial / ha, 1) else NA_character_,
    n_mes = n_mes,
    ranking_mes = sum(mensual$detecciones > n_mes) + 1L,
    n_meses_serie = nrow(mensual)
  )
}

# El evento visto por TODAS las plataformas, una fila por cada una. Un reporte
# lo usa para hablar de sus hermanas sin escribir sus cifras a mano: cuando un
# episodio es el mismo para las cuatro, la comparación entre ellas es parte del
# resultado (mismo fuego, mismos días, conteos que difieren en un orden de
# magnitud). Sigue el patrón de resumen_multiplataforma() en
# R/ayudantes_reporte.R, incluido leer los targets con tar_read_raw.
resumen_evento_plataformas <- function(clave_evento, claves, store) {
  purrr::map(claves, function(k) {
    fp <- targets::tar_read_raw(paste0("firms_parque_", k), store = store)
    aq <- targets::tar_read_raw(paste0("area_quemada_parque_", k),
                                store = store)
    e <- evento(clave_evento)
    d <- sf::st_drop_geometry(fp)
    d <- d[d$acq_date >= e$inicio & d$acq_date <= e$fin, ]
    ha <- sum(sf::st_drop_geometry(quemas_evento(clave_evento, aq))$area_ha)
    tibble::tibble(
      clave = k,
      plataforma = etiquetas_plataforma(k)$corta,
      detecciones = nrow(d),
      dias = dplyr::n_distinct(d$acq_date),
      frp_maximo = if (nrow(d)) max(d$frp, na.rm = TRUE) else NA_real_,
      pct_nocturnas = 100 * mean(d$daynight == "N"),
      hectareas = round(ha),
      veces_menos = if (ha > 0) e$ha_oficial / ha else NA_real_
    )
  }) |>
    purrr::list_rbind()
}

# Niveles de procesamiento de FIRMS que cubren la ventana del evento, en
# prosa ("estándar", "tiempo casi real (provisional)" o ambos). Importa porque
# un evento reciente cae casi siempre en la cola en tiempo casi real, cuyas
# cifras pueden cambiar cuando la NASA la reprocesa a estándar, y porque el
# tramo que le toca a cada plataforma es distinto.
nivel_evento <- function(clave_evento, rangos) {
  e <- evento(clave_evento)
  cubre <- rangos$inicio <= e$fin & rangos$fin >= e$inicio
  niveles <- unique(etiqueta_nivel(rangos$nivel[cubre]))
  list(
    niveles = niveles,
    prosa = paste(niveles, collapse = " y "),
    provisional = any(rangos$nivel[cubre] == "NRT"),
    solo_provisional = all(rangos$nivel[cubre] == "NRT")
  )
}

# ¿Cubre el producto de área quemada todos los meses del evento? Los productos
# se publican con uno o dos meses de rezago, así que un reporte generado poco
# después del evento puede mostrar solo la primera parte de la cicatriz. La
# prosa debe advertirlo en vez de presentar una cifra parcial como final.
cobertura_ba_evento <- function(clave_evento, area_quemada_mensual) {
  e <- evento(clave_evento)
  meses <- seq(lubridate::floor_date(e$inicio, "month"),
               lubridate::floor_date(e$fin, "month"), by = "month")
  publicados <- area_quemada_mensual$aniomes[!is.na(area_quemada_mensual$hectareas)]
  list(
    completa = all(meses %in% publicados),
    faltantes = meses[!meses %in% publicados]
  )
}

# Rango de meses en prosa ("mayo y junio de 2026", "mayo de 2026").
meses_en_prosa <- function(fechas) {
  nombres <- tolower(MESES_ES_LARGO[lubridate::month(fechas)])
  anios <- unique(lubridate::year(fechas))
  if (length(nombres) == 1) {
    paste(nombres, "de", anios[1])
  } else {
    paste(paste(utils::head(nombres, -1), collapse = ", "), "y",
          utils::tail(nombres, 1), "de", paste(anios, collapse = " y "))
  }
}

# Mapa del evento: detecciones por fecha y píxeles de área quemada sobre el
# sector, con el resto del parque como contexto. Encuadra el sector y no el
# parque completo —el evento ocupa una fracción del área protegida y a escala
# de parque los puntos se apelotonan— pero dibuja el límite completo para que
# se lea dónde ocurrió.
grafico_evento <- function(clave_evento, firms_parque, area_quemada_parque,
                           parque, humedales, archivos_worldcover, bbox, dest,
                           etiqueta_fuente, etiqueta_ba, fuente_datos) {
  e <- evento(clave_evento)
  detecciones <- detecciones_evento(clave_evento, firms_parque)
  quemas <- quemas_evento(clave_evento, area_quemada_parque)
  sector <- humedales[grepl(e$sector, humedales$nom_hum, fixed = TRUE), ]

  # Encuadre: el sector con un margen del 12 % de su lado mayor.
  caja <- sf::st_bbox(sector)
  margen <- 0.12 * max(caja["xmax"] - caja["xmin"], caja["ymax"] - caja["ymin"])
  fondo <- fondo_cobertura_animacion(archivos_worldcover, bbox)

  p <- ggplot2::ggplot() +
    ggplot2::annotation_raster(fondo$imagen, xmin = fondo$xmin,
                               xmax = fondo$xmax, ymin = fondo$ymin,
                               ymax = fondo$ymax) +
    ggplot2::geom_sf(data = parque, fill = NA, color = "grey30",
                     linewidth = 0.5) +
    ggplot2::geom_sf(data = sf::st_union(sector), fill = NA, color = "grey15",
                     linewidth = 0.7, linetype = "22") +
    ggplot2::geom_sf(data = quemas, fill = COLOR_AREA_QUEMADA, alpha = 0.5,
                     color = COLOR_AREA_QUEMADA, linewidth = 0.3) +
    # El color codifica el DÍA y no el FRP: en una ventana de pocos días lo
    # informativo es cómo avanzó el fuego, y una escala discreta por fecha se
    # lee sin consultar una barra continua. El factor se ordena por fecha y no
    # por la etiqueta "%d/%m", que pondría el 01/06 antes del 29/05.
    ggplot2::geom_sf(
      data = dplyr::mutate(
        detecciones,
        dia = factor(format(acq_date, "%d/%m"),
                     levels = format(sort(unique(acq_date)), "%d/%m"))
      ),
      ggplot2::aes(fill = dia),
      shape = 21, size = 2.4, stroke = 0.3, color = "grey15", alpha = 0.9
    ) +
    ggplot2::scale_fill_viridis_d(option = "inferno", direction = -1,
                                  begin = 0.15, end = 0.85, name = NULL) +
    ggplot2::coord_sf(
      xlim = c(caja["xmin"] - margen, caja["xmax"] + margen),
      ylim = c(caja["ymin"] - margen, caja["ymax"] + margen)
    ) +
    ggplot2::labs(
      title = paste0("El ", e$nombre),
      subtitle = paste0("Detecciones ", etiqueta_fuente, " del ",
                        format(e$inicio, "%d/%m/%Y"), " al ",
                        format(e$fin, "%d/%m/%Y"),
                        " y área quemada (", etiqueta_ba, ")\n",
                        "El trazo discontinuo delimita el ", e$sector,
                        " del Registro Nacional de Humedales"),
      caption = fuente_datos
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_line(color = "grey92", linewidth = 0.3),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 9, lineheight = 1.2),
      legend.position = "bottom",
      plot.caption = ggplot2::element_text(size = 7, color = "grey40")
    )

  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(dest, p, width = 7, height = 6.4, dpi = 150)
  dest
}
