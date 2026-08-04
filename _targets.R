# Pipeline de anomalías térmicas y área quemada en el PN Palo Verde.
#
# Cuatro plataformas satelitales HERMANAS —MODIS, VIIRS S-NPP, VIIRS NOAA-20 y
# VIIRS NOAA-21— con juegos de productos equivalentes e independientes. Sus
# detecciones NUNCA se suman: cada sensor y cada plataforma observa con
# resolución y hora de paso distintas, así que una serie combinada mostraría
# saltos que reflejan el instrumental disponible y no el régimen de fuego.
#
# Ejecución:      targets::tar_make()
# Grafo:          targets::tar_visnetwork()
# Estado:         targets::tar_outdated()
# Targets:        targets::tar_manifest(fields = "name")
#
# CÓMO LEER ESTE ARCHIVO
# Las cuatro cadenas se generan con tarchetypes::tar_map a partir de
# PLATAFORMAS (R/plataformas.R): cada target del bloque `tar_map` existe
# cuatro veces, con el sufijo de la clave de plataforma (_modis, _viirs,
# _noaa20, _noaa21). Es decir, `firms_parque` no existe como tal; existen
# `firms_parque_modis`, `firms_parque_viirs`, etc.
#
# La descarga es reanudable: cada fragmento de fechas es una rama dinámica
# respaldada por un CSV en data/raw/firms/; si la ejecución se interrumpe,
# volver a correr tar_make() continúa donde quedó.

library(targets)
library(tarchetypes)

tar_source("R")

tar_option_set(
  packages = c(
    "sf", "dplyr", "tidyr", "purrr", "readr", "lubridate", "glue", "httr2",
    "ggplot2", "gganimate", "gifski", "av", "plotly", "leaflet", "leaflet.extras2",
    "yyjsonr", "htmlwidgets", "DT", "here", "quarto", "terra", "exactextractr",
    "tibble"
  ),
  format = "rds"
)

# Valores del map. `quemas_origen` apunta al target del PRODUCTO de área
# quemada que le corresponde a cada plataforma: MODIS y S-NPP tienen el suyo;
# NOAA-20 y NOAA-21 toman el de S-NPP porque no existe uno propio.
plataformas_pipeline <- dplyr::mutate(
  PLATAFORMAS[, c("clave", "etiqueta", "ba_producto")],
  quemas_origen = rlang::syms(paste0("quemas_", tolower(ba_producto))),
  ultimo_ba     = rlang::syms(paste0("ultimo_mes_", tolower(ba_producto)))
)
plataformas_pipeline$ba_producto <- NULL

list(
  # --- Parámetros del pipeline (editar aquí) -------------------------------
  # El rango se recorta automáticamente al disponible en FIRMS, por lo que
  # una fecha_fin lejana significa "hasta lo más reciente disponible".
  tar_target(fecha_inicio, as.Date("2001-01-01")),
  tar_target(fecha_fin,    as.Date("2100-01-01")),

  # --- Contexto compartido por las cuatro plataformas ----------------------
  # Nada de esto depende del sensor: el polígono del parque, las capas
  # nacionales, la cobertura de la tierra y el relieve del video.
  tar_target(archivo_parque, descargar_parque_wfs("data/raw/wfs/palo_verde.gpkg"),
             format = "file"),
  tar_target(parque, procesar_parque(archivo_parque)),
  tar_target(bbox_descarga, bbox_con_buffer(parque)),
  tar_target(bbox_parque, sf::st_bbox(parque)),
  tar_target(archivos_worldcover, descargar_worldcover(bbox_descarga),
             format = "file"),
  tar_target(archivo_humedales,
             descargar_capa_wfs(WFS_CAPA_HUMEDALES, bbox_parque,
                                "data/raw/wfs/humedales.gpkg"),
             format = "file"),
  tar_target(archivo_bosque,
             descargar_capa_wfs(WFS_CAPA_BOSQUE, bbox_parque,
                                "data/raw/wfs/cobertura_forestal.gpkg"),
             format = "file"),
  tar_target(humedales, sf::st_read(archivo_humedales, quiet = TRUE)),
  tar_target(bosque, sf::st_read(archivo_bosque, quiet = TRUE)),
  tar_target(paisaje_parque,
             composicion_paisaje(parque, archivos_worldcover, bbox_descarga)),
  tar_target(archivos_dem, descargar_dem_terrarium(bbox_descarga),
             format = "file"),
  tar_target(relieve_video, fondo_relieve_video(archivos_dem, parque,
                                                bbox_descarga,
                                                archivos_worldcover)),

  # --- Área quemada, por PRODUCTO y no por plataforma ----------------------
  # Hay exactamente dos productos y son un recurso compartido: VNP64A1
  # alimenta a tres plataformas. Nombrarlos por producto evita un target
  # llamado "granulos_ba_noaa21" que contendría granulos de Suomi-NPP, y evita
  # repetir la consulta al catálogo CMR (que corre en cada ejecución) una vez
  # por plataforma.
  tar_target(granulos_mcd64a1,
             cmr_granulos_ba(fecha_inicio, fecha_fin,
                             MCD64A1_SHORT_NAME, MCD64A1_VERSION),
             cue = tar_cue(mode = "always"), iteration = "group"),
  tar_target(hdf_mcd64a1,
             descargar_granulo_ba(granulos_mcd64a1, "data/raw/mcd64a1"),
             pattern = map(granulos_mcd64a1), format = "file"),
  tar_target(quemas_mcd64a1, extraer_quemas(hdf_mcd64a1, parque)),
  # Frontera de PUBLICACIÓN del producto, tomada de la lista de granulos y no
  # de los píxeles: un mes publicado sin fuego en el parque no aporta píxeles
  # y correría la frontera hacia atrás.
  tar_target(ultimo_mes_mcd64a1, max(granulos_mcd64a1$aniomes)),
  tar_target(granulos_vnp64a1,
             cmr_granulos_ba(fecha_inicio, fecha_fin,
                             VNP64A1_SHORT_NAME, VNP64A1_VERSION),
             cue = tar_cue(mode = "always"), iteration = "group"),
  tar_target(hdf_vnp64a1,
             descargar_granulo_ba(granulos_vnp64a1, "data/raw/vnp64a1"),
             pattern = map(granulos_vnp64a1), format = "file"),
  tar_target(quemas_vnp64a1, extraer_quemas(hdf_vnp64a1, parque)),
  tar_target(ultimo_mes_vnp64a1, max(granulos_vnp64a1$aniomes)),

  # --- Una cadena por plataforma -------------------------------------------
  # Todo lo que sigue existe cuatro veces, con el sufijo de la clave. El área
  # quemada se recorta al período observado por la plataforma: sin ese
  # recorte, una plataforma que empieza en 2018 mostraría superficie quemada
  # de 2012, años sin detecciones, sugiriendo un vacío de detección
  # inexistente.
  tar_map(
    values = plataformas_pipeline,
    names = clave,
    descriptions = etiqueta,

    tar_target(etiquetas, etiquetas_plataforma(clave)),
    tar_target(rangos, rangos_plataforma(clave, fecha_inicio, fecha_fin),
               cue = tar_cue(mode = "always")),
    tar_target(fragmentos, construir_fragmentos_multi(rangos),
               iteration = "group"),
    tar_target(csv_fragmentos,
               descargar_firms_fragmento(fragmentos, bbox_descarga),
               pattern = map(fragmentos), format = "file"),
    tar_target(firms_crudo,   leer_y_unir_csv(csv_fragmentos, rangos)),
    tar_target(firms_puntos,  a_sf_puntos(firms_crudo)),
    tar_target(firms_parque,  recortar_al_parque(firms_puntos, parque)),
    tar_target(firms_mensual, agregar_mensual(firms_parque, rangos)),
    tar_target(cobertura, extraer_cobertura(firms_parque, archivos_worldcover)),
    tar_target(humedales_detecciones,
               cruzar_con_humedales(firms_parque, humedales)),
    tar_target(area_quemada_parque,
               recortar_quemas_al_periodo(quemas_origen, firms_mensual)),
    tar_target(area_quemada_mensual,
               agregar_mensual_quemas(area_quemada_parque,
                                      rango_meses = range(firms_mensual$aniomes),
                                      hasta = ultimo_ba)),
    tar_target(cobertura_quemas,
               extraer_cobertura_quemas(area_quemada_parque,
                                        archivos_worldcover)),
    tar_target(humedales_quemas,
               cruzar_quemas_con_humedales(area_quemada_parque, humedales)),
    tar_target(borde_bosque,
               contexto_borde(firms_parque, parque, archivos_worldcover,
                              bbox_descarga, quemas = area_quemada_parque)),

    tar_target(cartel_resumen,
               generar_cartel_resumen(firms_mensual, area_quemada_mensual,
                                      file.path("outputs/figs", clave,
                                                "cartel_resumen.png"),
                                      etiquetas$corta, etiquetas$ids_fuente,
                                      etiquetas$etiqueta_ba),
               format = "file"),
    tar_target(video_anomalias,
               generar_video_anomalias(firms_parque, area_quemada_parque,
                                       firms_mensual, area_quemada_mensual,
                                       parque, relieve_video,
                                       file.path("outputs/figs", clave,
                                                 "video_anomalias_termicas.mp4"),
                                       etiquetas$corta, etiquetas$ids_fuente,
                                       etiquetas$etiqueta_ba, fps = 5),
               format = "file"),
    tar_target(anim_gif,
               animar_detecciones(firms_parque, parque, firms_mensual,
                                  archivos_worldcover, bbox_descarga,
                                  file.path("outputs/figs", clave,
                                            "animacion_mensual.gif"),
                                  etiquetas$fuente_fig, etiquetas$pie_animacion),
               format = "file"),
    tar_target(anim_mp4,
               animar_detecciones(firms_parque, parque, firms_mensual,
                                  archivos_worldcover, bbox_descarga,
                                  file.path("outputs/figs", clave,
                                            "animacion_mensual.mp4"),
                                  etiquetas$fuente_fig, etiquetas$pie_animacion),
               format = "file"),
    tar_target(mapa_html,
               mapa_leaflet_temporal(firms_parque, parque, cobertura,
                                     archivos_worldcover, bbox_descarga,
                                     humedales, bosque,
                                     file.path("outputs/maps", clave,
                                               "mapa_temporal.html"),
                                     etiquetas$etiqueta_quemas,
                                     quemas = area_quemada_parque),
               format = "file"),
    tar_target(fig_serie,
               grafico_serie_temporal(firms_mensual,
                                      file.path("outputs/figs", clave,
                                                "serie_mensual.png"),
                                      etiquetas$fuente_fig, etiquetas$pie_firms),
               format = "file"),
    tar_target(fig_climatologia,
               grafico_climatologia(firms_mensual,
                                    file.path("outputs/figs", clave,
                                              "climatologia_mensual.png"),
                                    etiquetas$fuente_fig, etiquetas$pie_firms),
               format = "file"),
    tar_target(fig_serie_html,
               grafico_serie_html(firms_mensual,
                                  file.path("outputs/figs", clave,
                                            "serie_mensual.html"),
                                  etiquetas$fuente_fig, etiquetas$pie_firms),
               format = "file"),
    tar_target(fig_climatologia_html,
               grafico_climatologia_html(firms_mensual,
                                         file.path("outputs/figs", clave,
                                                   "climatologia_mensual.html"),
                                         etiquetas$fuente_fig,
                                         etiquetas$pie_firms),
               format = "file"),
    tar_target(fig_area_quemada,
               grafico_area_quemada(area_quemada_mensual,
                                    file.path("outputs/figs", clave,
                                              "area_quemada_mensual.png"),
                                    etiquetas$etiqueta_ba, etiquetas$pie_ba),
               format = "file"),
    tar_target(fig_comparacion,
               grafico_comparacion(firms_mensual, area_quemada_mensual,
                                   file.path("outputs/figs", clave,
                                             "detecciones_vs_area_quemada.png"),
                                   etiquetas$etiqueta_ba, etiquetas$pie_ambos),
               format = "file"),
    tar_target(fig_cobertura_quemas,
               grafico_cobertura_quemas(cobertura_quemas, humedales_quemas,
                                        file.path("outputs/figs", clave,
                                                  "cobertura_area_quemada.png"),
                                        etiquetas$pie_cobertura_ba),
               format = "file"),
    tar_target(fig_climatologia_comparada,
               grafico_climatologia_comparada(firms_mensual,
                                              area_quemada_mensual,
                                              file.path("outputs/figs", clave,
                                                        "climatologia_comparada.png"),
                                              etiquetas$etiqueta_ba,
                                              etiquetas$pie_ambos),
               format = "file"),
    tar_target(fig_cobertura,
               grafico_cobertura(cobertura, humedales_detecciones,
                                 file.path("outputs/figs", clave,
                                           "cobertura_detecciones.png"),
                                 etiquetas$pie_cobertura),
               format = "file"),
    tar_target(tabla_area_quemada,
               tabla_area_quemada_csv(area_quemada_parque,
                                      file.path("outputs/tables", clave,
                                                "area_quemada_anual.csv")),
               format = "file"),
    tar_target(tabla_cobertura,
               tabla_cobertura_csv(cobertura,
                                   file.path("outputs/tables", clave,
                                             "cobertura_detecciones.csv")),
               format = "file"),
    tar_target(tabla_contraste,
               tabla_contraste_csv(cobertura, humedales_detecciones,
                                   cobertura_quemas, humedales_quemas,
                                   file.path("outputs/tables", clave,
                                             "contraste_humedales.csv")),
               format = "file"),
    tar_target(tabla_borde,
               tabla_borde_csv(borde_bosque,
                               file.path("outputs/tables", clave,
                                         "contexto_borde.csv")),
               format = "file"),
    tar_target(tabla_csv,
               tabla_resumen_csv(firms_parque, area_quemada_parque,
                                 file.path("outputs/tables", clave,
                                           "resumen_anual.csv")),
               format = "file"),
    tar_target(tabla_html,
               tabla_resumen_html(firms_parque, area_quemada_parque,
                                  file.path("outputs/tables", clave,
                                            "resumen_anual.html"),
                                  etiquetas$fuente_fig, etiquetas$etiqueta_ba),
               format = "file")
  ),

  # --- Reportes Quarto ------------------------------------------------------
  # Se escriben explícitamente: son el elemento menos uniforme del proyecto
  # (su prosa es distinta por definición) y son pocos. tar_quarto solo detecta
  # como dependencias los targets leídos con tar_read() dentro del qmd, no las
  # funciones de R/ que el qmd llama vía tar_source(); de ahí `extra_files`.
  tar_quarto(reporte_modis, "analysis/index.qmd",
             extra_files = list.files("R", pattern = "[.][Rr]$",
                                      full.names = TRUE)),
  tar_target(pagina_modis, {
    reporte_modis
    file.copy("analysis/index.html", "index.html", overwrite = TRUE)
    "index.html"
  }, format = "file"),
  tar_quarto(reporte_viirs, "analysis/viirs.qmd",
             extra_files = list.files("R", pattern = "[.][Rr]$",
                                      full.names = TRUE)),
  tar_target(pagina_viirs, {
    reporte_viirs
    dir.create("viirs", showWarnings = FALSE)
    file.copy("analysis/viirs.html", "viirs/index.html", overwrite = TRUE)
    "viirs/index.html"
  }, format = "file"),
  tar_quarto(reporte_noaa20, "analysis/noaa20.qmd",
             extra_files = list.files("R", pattern = "[.][Rr]$",
                                      full.names = TRUE)),
  tar_target(pagina_noaa20, {
    reporte_noaa20
    dir.create("noaa20", showWarnings = FALSE)
    file.copy("analysis/noaa20.html", "noaa20/index.html", overwrite = TRUE)
    "noaa20/index.html"
  }, format = "file")
)
