# Pipeline de anomalías térmicas (FIRMS MODIS_SP) y área quemada (MCD64A1)
# en el PN Palo Verde.
#
# Ejecución:      targets::tar_make()
# Grafo:          targets::tar_visnetwork()
# Estado:         targets::tar_outdated()
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

list(
  # --- Parámetros del pipeline (editar aquí) -------------------------------
  # El rango se recorta automáticamente al disponible en FIRMS, por lo que
  # una fecha_fin lejana significa "hasta lo más reciente disponible".
  tar_target(fecha_inicio, as.Date("2001-01-01")),
  tar_target(fecha_fin,    as.Date("2100-01-01")),
  tar_target(fuente_firms, "MODIS_SP"),

  # --- Polígono del parque (WFS del SINAC) ---------------------------------
  tar_target(archivo_parque, descargar_parque_wfs("data/raw/wfs/palo_verde.gpkg"),
             format = "file"),
  tar_target(parque, procesar_parque(archivo_parque)),
  tar_target(bbox_descarga, bbox_con_buffer(parque)),

  # --- Disponibilidad y fragmentos de descarga -----------------------------
  tar_target(rango_disponible, firms_disponibilidad(fuente_firms),
             cue = tar_cue(mode = "always")),
  tar_target(rango_efectivo, clamp_rango(fecha_inicio, fecha_fin, rango_disponible)),
  tar_target(fragmentos, construir_fragmentos(rango_efectivo),
             iteration = "group"),

  # --- Descarga reanudable: una rama dinámica por fragmento ----------------
  tar_target(
    csv_fragmentos,
    descargar_firms_fragmento(fragmentos, fuente_firms, bbox_descarga),
    pattern = map(fragmentos),
    format = "file"
  ),

  # --- Procesamiento --------------------------------------------------------
  tar_target(firms_crudo,   leer_y_unir_csv(csv_fragmentos)),
  tar_target(firms_puntos,  a_sf_puntos(firms_crudo)),
  tar_target(firms_parque,  recortar_al_parque(firms_puntos, parque)),
  tar_target(firms_mensual, agregar_mensual(firms_parque)),

  # --- Cobertura de la tierra (ESA WorldCover 2021) ------------------------
  tar_target(archivos_worldcover, descargar_worldcover(bbox_descarga),
             format = "file"),
  tar_target(cobertura, extraer_cobertura(firms_parque, archivos_worldcover)),

  # --- Capas nacionales del SINAC: humedales y cobertura forestal ----------
  tar_target(bbox_parque, sf::st_bbox(parque)),
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
  tar_target(humedales_detecciones, cruzar_con_humedales(firms_parque, humedales)),

  # --- Fuente VIIRS S-NPP: cadena paralela a la de MODIS -------------------
  # Genera productos independientes: las detecciones de sensores distintos
  # NUNCA se suman (375 m de VIIRS detecta muchos más fuegos que 1 km de
  # MODIS). La rejilla de fragmentos es común (ORIGEN_GRILLA) y clamp_rango()
  # recorta al registro VIIRS (desde 2012-01-20). Si se agrega una tercera
  # fuente, migrar estas cadenas a tarchetypes::tar_map.
  tar_target(fuente_firms_viirs, "VIIRS_SNPP_SP"),
  tar_target(rango_disponible_viirs, firms_disponibilidad(fuente_firms_viirs),
             cue = tar_cue(mode = "always")),
  tar_target(rango_efectivo_viirs,
             clamp_rango(fecha_inicio, fecha_fin, rango_disponible_viirs)),
  tar_target(fragmentos_viirs, construir_fragmentos(rango_efectivo_viirs),
             iteration = "group"),
  tar_target(
    csv_fragmentos_viirs,
    descargar_firms_fragmento(fragmentos_viirs, fuente_firms_viirs,
                              bbox_descarga),
    pattern = map(fragmentos_viirs),
    format = "file"
  ),
  tar_target(firms_crudo_viirs,   leer_y_unir_csv(csv_fragmentos_viirs)),
  tar_target(firms_puntos_viirs,  a_sf_puntos(firms_crudo_viirs)),
  tar_target(firms_parque_viirs,  recortar_al_parque(firms_puntos_viirs, parque)),
  tar_target(firms_mensual_viirs, agregar_mensual(firms_parque_viirs)),
  tar_target(cobertura_viirs,
             extraer_cobertura(firms_parque_viirs, archivos_worldcover)),
  tar_target(humedales_detecciones_viirs,
             cruzar_con_humedales(firms_parque_viirs, humedales)),

  # Área quemada VIIRS (VNP64A1 v002): heredero de MCD64A1 derivado de
  # S-NPP; misma tesela, formato y lectura, registro desde 2012-03. Se
  # empareja con las detecciones VIIRS (nunca se mezclan productos de
  # sensores distintos en un mismo juego de salidas).
  tar_target(granulos_ba_viirs,
             cmr_granulos_mcd64a1(fecha_inicio, fecha_fin,
                                  short_name = VNP64A1_SHORT_NAME,
                                  version = VNP64A1_VERSION),
             cue = tar_cue(mode = "always"), iteration = "group"),
  tar_target(hdf_ba_viirs,
             descargar_granulo_mcd64a1(granulos_ba_viirs,
                                       dir = "data/raw/vnp64a1"),
             pattern = map(granulos_ba_viirs), format = "file"),
  tar_target(area_quemada_parque_viirs, extraer_quemas(hdf_ba_viirs, parque)),
  tar_target(area_quemada_mensual_viirs,
             agregar_mensual_quemas(area_quemada_parque_viirs,
                                    rango_meses = range(firms_mensual_viirs$aniomes))),
  tar_target(cobertura_quemas_viirs,
             extraer_cobertura_quemas(area_quemada_parque_viirs,
                                      archivos_worldcover)),
  tar_target(humedales_quemas_viirs,
             cruzar_quemas_con_humedales(area_quemada_parque_viirs, humedales)),
  tar_target(borde_bosque_viirs,
             contexto_borde(firms_parque_viirs, parque, archivos_worldcover,
                            bbox_descarga, quemas = area_quemada_parque_viirs)),

  # --- Fuente VIIRS NOAA-20: tercera cadena, también independiente ---------
  # Mismo instrumento que S-NPP pero otra plataforma, con media órbita de
  # separación: sus detecciones son observaciones adicionales, no las mismas,
  # y por eso tampoco se suman a las de S-NPP (lo harían aparecer como un
  # salto en 2018). Registro desde 2018-04.
  #
  # No existe producto de área quemada de NOAA-20 (VJ164A1 no está publicado;
  # verificado en CMR el 2026-08-04), así que estos productos se acompañan del
  # ÚNICO producto de área quemada VIIRS, VNP64A1 de S-NPP, recortado a la
  # ventana de NOAA-20 y rotulado como tal en todas las salidas. Es una
  # medición distinta (superficie marcada a 500 m), no detecciones de otro
  # sensor mezcladas.
  tar_target(fuente_firms_noaa20, "VIIRS_NOAA20_SP"),
  tar_target(rango_disponible_noaa20, firms_disponibilidad(fuente_firms_noaa20),
             cue = tar_cue(mode = "always")),
  tar_target(rango_efectivo_noaa20,
             clamp_rango(fecha_inicio, fecha_fin, rango_disponible_noaa20)),
  tar_target(fragmentos_noaa20, construir_fragmentos(rango_efectivo_noaa20),
             iteration = "group"),
  tar_target(
    csv_fragmentos_noaa20,
    descargar_firms_fragmento(fragmentos_noaa20, fuente_firms_noaa20,
                              bbox_descarga),
    pattern = map(fragmentos_noaa20),
    format = "file"
  ),
  tar_target(firms_crudo_noaa20,   leer_y_unir_csv(csv_fragmentos_noaa20)),
  tar_target(firms_puntos_noaa20,  a_sf_puntos(firms_crudo_noaa20)),
  tar_target(firms_parque_noaa20,  recortar_al_parque(firms_puntos_noaa20, parque)),
  tar_target(firms_mensual_noaa20, agregar_mensual(firms_parque_noaa20)),
  tar_target(cobertura_noaa20,
             extraer_cobertura(firms_parque_noaa20, archivos_worldcover)),
  tar_target(humedales_detecciones_noaa20,
             cruzar_con_humedales(firms_parque_noaa20, humedales)),
  # Área quemada de S-NPP recortada a la ventana de NOAA-20: reutiliza los
  # granulos ya descargados (no hay descarga nueva).
  tar_target(area_quemada_parque_noaa20,
             recortar_quemas_al_periodo(area_quemada_parque_viirs,
                                        firms_mensual_noaa20)),
  tar_target(area_quemada_mensual_noaa20,
             agregar_mensual_quemas(area_quemada_parque_noaa20,
                                    rango_meses = range(firms_mensual_noaa20$aniomes))),
  tar_target(cobertura_quemas_noaa20,
             extraer_cobertura_quemas(area_quemada_parque_noaa20,
                                      archivos_worldcover)),
  tar_target(humedales_quemas_noaa20,
             cruzar_quemas_con_humedales(area_quemada_parque_noaa20, humedales)),
  tar_target(borde_bosque_noaa20,
             contexto_borde(firms_parque_noaa20, parque, archivos_worldcover,
                            bbox_descarga, quemas = area_quemada_parque_noaa20)),

  # --- Área quemada (MCD64A1 v6.1, LP DAAC vía Earthdata) ------------------
  # FIRMS no entrega BA_MODIS como datos (su API de área responde vacío);
  # se usa el producto original. Ver cabecera de R/descarga_mcd64a1.R.
  # La lista de granulos se refresca en cada corrida (cue always, como
  # rango_disponible); la clave de rama es el mes (aniomes), estable ante
  # meses nuevos y sensible a granulos reprocesados.
  tar_target(granulos_ba, cmr_granulos_mcd64a1(fecha_inicio, fecha_fin),
             cue = tar_cue(mode = "always"), iteration = "group"),
  tar_target(hdf_ba, descargar_granulo_mcd64a1(granulos_ba),
             pattern = map(granulos_ba), format = "file"),
  tar_target(area_quemada_parque, extraer_quemas(hdf_ba, parque)),
  tar_target(area_quemada_mensual,
             agregar_mensual_quemas(area_quemada_parque,
                                    rango_meses = range(firms_mensual$aniomes))),
  # Cobertura y humedales sobre los píxeles quemados (análogos de `cobertura`
  # y `humedales_detecciones`; el píxel de 500 m ya es el footprint).
  tar_target(cobertura_quemas,
             extraer_cobertura_quemas(area_quemada_parque, archivos_worldcover)),
  tar_target(humedales_quemas,
             cruzar_quemas_con_humedales(area_quemada_parque, humedales)),

  # --- Contexto paisajístico: ¿borde del bosque o terreno abierto? ---------
  tar_target(paisaje_parque,
             composicion_paisaje(parque, archivos_worldcover, bbox_descarga)),
  tar_target(borde_bosque,
             contexto_borde(firms_parque, parque, archivos_worldcover,
                            bbox_descarga, quemas = area_quemada_parque)),

  # --- Video estilo cartel (relieve Terrarium + render cuadro a cuadro) ----
  # relieve_video es un target propio para que ajustar colores o layout del
  # video no re-descargue ni re-proyecte el DEM.
  tar_target(archivos_dem, descargar_dem_terrarium(bbox_descarga),
             format = "file"),
  tar_target(relieve_video, fondo_relieve_video(archivos_dem, parque,
                                                bbox_descarga,
                                                archivos_worldcover)),
  tar_target(cartel_resumen,
             generar_cartel_resumen(firms_mensual, area_quemada_mensual,
                                    "outputs/figs/cartel_resumen.png"),
             format = "file"),
  tar_target(video_anomalias,
             generar_video_anomalias(firms_parque, area_quemada_parque,
                                     firms_mensual, area_quemada_mensual,
                                     parque, relieve_video,
                                     "outputs/figs/video_anomalias_termicas.mp4",
                                     fps = 5),
             format = "file"),

  # --- Salidas: animaciones -------------------------------------------------
  tar_target(anim_gif,
             animar_detecciones(firms_parque, parque, firms_mensual,
                                archivos_worldcover, bbox_descarga,
                                "outputs/figs/animacion_mensual.gif"),
             format = "file"),
  tar_target(anim_mp4,
             animar_detecciones(firms_parque, parque, firms_mensual,
                                archivos_worldcover, bbox_descarga,
                                "outputs/figs/animacion_mensual.mp4"),
             format = "file"),

  # --- Salidas: mapa interactivo -------------------------------------------
  tar_target(mapa_html,
             mapa_leaflet_temporal(firms_parque, parque, cobertura,
                                   archivos_worldcover, bbox_descarga,
                                   humedales, bosque,
                                   "outputs/maps/mapa_temporal.html",
                                   quemas = area_quemada_parque),
             format = "file"),

  # --- Salidas: gráficos y tablas ------------------------------------------
  tar_target(fig_serie,
             grafico_serie_temporal(firms_mensual, "outputs/figs/serie_mensual.png"),
             format = "file"),
  tar_target(fig_climatologia,
             grafico_climatologia(firms_mensual, "outputs/figs/climatologia_mensual.png"),
             format = "file"),
  tar_target(fig_serie_html,
             grafico_serie_html(firms_mensual, "outputs/figs/serie_mensual.html"),
             format = "file"),
  tar_target(fig_climatologia_html,
             grafico_climatologia_html(firms_mensual, "outputs/figs/climatologia_mensual.html"),
             format = "file"),
  tar_target(fig_area_quemada,
             grafico_area_quemada(area_quemada_mensual,
                                  "outputs/figs/area_quemada_mensual.png"),
             format = "file"),
  tar_target(fig_comparacion,
             grafico_comparacion(firms_mensual, area_quemada_mensual,
                                 "outputs/figs/detecciones_vs_area_quemada.png"),
             format = "file"),
  tar_target(tabla_area_quemada,
             tabla_area_quemada_csv(area_quemada_parque,
                                    "outputs/tables/area_quemada_anual.csv"),
             format = "file"),
  tar_target(fig_cobertura_quemas,
             grafico_cobertura_quemas(cobertura_quemas, humedales_quemas,
                                      "outputs/figs/cobertura_area_quemada.png"),
             format = "file"),
  tar_target(fig_climatologia_comparada,
             grafico_climatologia_comparada(firms_mensual, area_quemada_mensual,
                                            "outputs/figs/climatologia_comparada.png"),
             format = "file"),
  tar_target(fig_cobertura,
             grafico_cobertura(cobertura, humedales_detecciones,
                               "outputs/figs/cobertura_detecciones.png"),
             format = "file"),
  tar_target(tabla_cobertura,
             tabla_cobertura_csv(cobertura, "outputs/tables/cobertura_detecciones.csv"),
             format = "file"),
  tar_target(tabla_contraste,
             tabla_contraste_csv(cobertura, humedales_detecciones,
                                 cobertura_quemas, humedales_quemas,
                                 "outputs/tables/contraste_humedales.csv"),
             format = "file"),
  tar_target(tabla_borde,
             tabla_borde_csv(borde_bosque, "outputs/tables/contexto_borde.csv"),
             format = "file"),
  tar_target(tabla_csv,
             tabla_resumen_csv(firms_parque, area_quemada_parque,
                               "outputs/tables/resumen_anual.csv"),
             format = "file"),
  tar_target(tabla_html,
             tabla_resumen_html(firms_parque, area_quemada_parque,
                                "outputs/tables/resumen_anual.html"),
             format = "file"),

  # --- Salidas VIIRS: espejo de las salidas MODIS ---------------------------
  # Mismos nombres de archivo bajo outputs/*/viirs/ (los productos MODIS
  # conservan sus rutas publicadas); las etiquetas intercambian las siglas
  # VIIRS_SNPP_SP y VNP64A1. El relieve del video es compartido.
  tar_target(cartel_resumen_viirs,
             generar_cartel_resumen(firms_mensual_viirs,
                                    area_quemada_mensual_viirs,
                                    "outputs/figs/viirs/cartel_resumen.png",
                                    etiqueta_fuente = "VIIRS",
                                    id_fuente = "VIIRS_SNPP_SP",
                                    etiqueta_ba = "VNP64A1"),
             format = "file"),
  tar_target(video_anomalias_viirs,
             generar_video_anomalias(firms_parque_viirs,
                                     area_quemada_parque_viirs,
                                     firms_mensual_viirs,
                                     area_quemada_mensual_viirs,
                                     parque, relieve_video,
                                     "outputs/figs/viirs/video_anomalias_termicas.mp4",
                                     fps = 5,
                                     etiqueta_fuente = "VIIRS",
                                     id_fuente = "VIIRS_SNPP_SP",
                                     etiqueta_ba = "VNP64A1"),
             format = "file"),
  tar_target(anim_gif_viirs,
             animar_detecciones(firms_parque_viirs, parque, firms_mensual_viirs,
                                archivos_worldcover, bbox_descarga,
                                "outputs/figs/viirs/animacion_mensual.gif",
                                etiqueta_fuente = "VIIRS (FIRMS)",
                                fuente_datos = "Datos: NASA FIRMS (VIIRS_SNPP_SP) y SINAC. Fondo: ESA WorldCover 2021"),
             format = "file"),
  tar_target(anim_mp4_viirs,
             animar_detecciones(firms_parque_viirs, parque, firms_mensual_viirs,
                                archivos_worldcover, bbox_descarga,
                                "outputs/figs/viirs/animacion_mensual.mp4",
                                etiqueta_fuente = "VIIRS (FIRMS)",
                                fuente_datos = "Datos: NASA FIRMS (VIIRS_SNPP_SP) y SINAC. Fondo: ESA WorldCover 2021"),
             format = "file"),
  tar_target(mapa_html_viirs,
             mapa_leaflet_temporal(firms_parque_viirs, parque, cobertura_viirs,
                                   archivos_worldcover, bbox_descarga,
                                   humedales, bosque,
                                   "outputs/maps/viirs/mapa_temporal.html",
                                   quemas = area_quemada_parque_viirs,
                                   etiqueta_quemas = "Área quemada (VNP64A1)"),
             format = "file"),
  tar_target(fig_serie_viirs,
             grafico_serie_temporal(firms_mensual_viirs,
                                    "outputs/figs/viirs/serie_mensual.png",
                                    etiqueta_fuente = "VIIRS (FIRMS)",
                                    fuente = "Datos: NASA FIRMS (VIIRS_SNPP_SP)"),
             format = "file"),
  tar_target(fig_climatologia_viirs,
             grafico_climatologia(firms_mensual_viirs,
                                  "outputs/figs/viirs/climatologia_mensual.png",
                                  etiqueta_fuente = "VIIRS (FIRMS)",
                                  fuente = "Datos: NASA FIRMS (VIIRS_SNPP_SP)"),
             format = "file"),
  tar_target(fig_serie_html_viirs,
             grafico_serie_html(firms_mensual_viirs,
                                "outputs/figs/viirs/serie_mensual.html",
                                etiqueta_fuente = "VIIRS (FIRMS)",
                                fuente = "Datos: NASA FIRMS (VIIRS_SNPP_SP)"),
             format = "file"),
  tar_target(fig_climatologia_html_viirs,
             grafico_climatologia_html(firms_mensual_viirs,
                                       "outputs/figs/viirs/climatologia_mensual.html",
                                       etiqueta_fuente = "VIIRS (FIRMS)",
                                       fuente = "Datos: NASA FIRMS (VIIRS_SNPP_SP)"),
             format = "file"),
  tar_target(fig_area_quemada_viirs,
             grafico_area_quemada(area_quemada_mensual_viirs,
                                  "outputs/figs/viirs/area_quemada_mensual.png",
                                  etiqueta_ba = "VNP64A1",
                                  fuente = "Datos: NASA LP DAAC (VNP64A1 v2)"),
             format = "file"),
  tar_target(fig_comparacion_viirs,
             grafico_comparacion(firms_mensual_viirs, area_quemada_mensual_viirs,
                                 "outputs/figs/viirs/detecciones_vs_area_quemada.png",
                                 etiqueta_ba = "VNP64A1",
                                 fuente = "Datos: NASA FIRMS (VIIRS_SNPP_SP) y LP DAAC (VNP64A1)"),
             format = "file"),
  tar_target(tabla_area_quemada_viirs,
             tabla_area_quemada_csv(area_quemada_parque_viirs,
                                    "outputs/tables/viirs/area_quemada_anual.csv"),
             format = "file"),
  tar_target(fig_cobertura_quemas_viirs,
             grafico_cobertura_quemas(cobertura_quemas_viirs,
                                      humedales_quemas_viirs,
                                      "outputs/figs/viirs/cobertura_area_quemada.png",
                                      fuente = "Datos: NASA LP DAAC (VNP64A1), ESA WorldCover 2021 y SINAC"),
             format = "file"),
  tar_target(fig_climatologia_comparada_viirs,
             grafico_climatologia_comparada(firms_mensual_viirs,
                                            area_quemada_mensual_viirs,
                                            "outputs/figs/viirs/climatologia_comparada.png",
                                            etiqueta_ba = "VNP64A1",
                                            fuente = "Datos: NASA FIRMS (VIIRS_SNPP_SP) y LP DAAC (VNP64A1)"),
             format = "file"),
  tar_target(fig_cobertura_viirs,
             grafico_cobertura(cobertura_viirs, humedales_detecciones_viirs,
                               "outputs/figs/viirs/cobertura_detecciones.png",
                               fuente = "Datos: NASA FIRMS (VIIRS_SNPP_SP), ESA WorldCover 2021 y SINAC"),
             format = "file"),
  tar_target(tabla_cobertura_viirs,
             tabla_cobertura_csv(cobertura_viirs,
                                 "outputs/tables/viirs/cobertura_detecciones.csv"),
             format = "file"),
  tar_target(tabla_contraste_viirs,
             tabla_contraste_csv(cobertura_viirs, humedales_detecciones_viirs,
                                 cobertura_quemas_viirs, humedales_quemas_viirs,
                                 "outputs/tables/viirs/contraste_humedales.csv"),
             format = "file"),
  tar_target(tabla_borde_viirs,
             tabla_borde_csv(borde_bosque_viirs,
                             "outputs/tables/viirs/contexto_borde.csv"),
             format = "file"),
  tar_target(tabla_csv_viirs,
             tabla_resumen_csv(firms_parque_viirs, area_quemada_parque_viirs,
                               "outputs/tables/viirs/resumen_anual.csv"),
             format = "file"),
  tar_target(tabla_html_viirs,
             tabla_resumen_html(firms_parque_viirs, area_quemada_parque_viirs,
                                "outputs/tables/viirs/resumen_anual.html",
                                etiqueta_fuente = "VIIRS (FIRMS)",
                                etiqueta_ba = "VNP64A1"),
             format = "file"),

  # --- Salidas NOAA-20: espejo de las anteriores ----------------------------
  # El área quemada se rotula "VNP64A1, S-NPP" en todas partes: no existe
  # producto de NOAA-20 y el lector debe poder saberlo sin abrir el reporte.
  tar_target(cartel_resumen_noaa20,
             generar_cartel_resumen(firms_mensual_noaa20,
                                    area_quemada_mensual_noaa20,
                                    "outputs/figs/noaa20/cartel_resumen.png",
                                    etiqueta_fuente = "VIIRS NOAA-20",
                                    id_fuente = "VIIRS_NOAA20_SP",
                                    etiqueta_ba = "VNP64A1, S-NPP"),
             format = "file"),
  tar_target(video_anomalias_noaa20,
             generar_video_anomalias(firms_parque_noaa20,
                                     area_quemada_parque_noaa20,
                                     firms_mensual_noaa20,
                                     area_quemada_mensual_noaa20,
                                     parque, relieve_video,
                                     "outputs/figs/noaa20/video_anomalias_termicas.mp4",
                                     fps = 5,
                                     etiqueta_fuente = "VIIRS NOAA-20",
                                     id_fuente = "VIIRS_NOAA20_SP",
                                     etiqueta_ba = "VNP64A1, S-NPP"),
             format = "file"),
  tar_target(anim_gif_noaa20,
             animar_detecciones(firms_parque_noaa20, parque, firms_mensual_noaa20,
                                archivos_worldcover, bbox_descarga,
                                "outputs/figs/noaa20/animacion_mensual.gif",
                                etiqueta_fuente = "VIIRS NOAA-20 (FIRMS)",
                                fuente_datos = "Datos: NASA FIRMS (VIIRS_NOAA20_SP) y SINAC. Fondo: ESA WorldCover 2021"),
             format = "file"),
  tar_target(anim_mp4_noaa20,
             animar_detecciones(firms_parque_noaa20, parque, firms_mensual_noaa20,
                                archivos_worldcover, bbox_descarga,
                                "outputs/figs/noaa20/animacion_mensual.mp4",
                                etiqueta_fuente = "VIIRS NOAA-20 (FIRMS)",
                                fuente_datos = "Datos: NASA FIRMS (VIIRS_NOAA20_SP) y SINAC. Fondo: ESA WorldCover 2021"),
             format = "file"),
  tar_target(mapa_html_noaa20,
             mapa_leaflet_temporal(firms_parque_noaa20, parque, cobertura_noaa20,
                                   archivos_worldcover, bbox_descarga,
                                   humedales, bosque,
                                   "outputs/maps/noaa20/mapa_temporal.html",
                                   quemas = area_quemada_parque_noaa20,
                                   etiqueta_quemas = "Área quemada (VNP64A1, S-NPP)"),
             format = "file"),
  tar_target(fig_serie_noaa20,
             grafico_serie_temporal(firms_mensual_noaa20,
                                    "outputs/figs/noaa20/serie_mensual.png",
                                    etiqueta_fuente = "VIIRS NOAA-20 (FIRMS)",
                                    fuente = "Datos: NASA FIRMS (VIIRS_NOAA20_SP)"),
             format = "file"),
  tar_target(fig_climatologia_noaa20,
             grafico_climatologia(firms_mensual_noaa20,
                                  "outputs/figs/noaa20/climatologia_mensual.png",
                                  etiqueta_fuente = "VIIRS NOAA-20 (FIRMS)",
                                  fuente = "Datos: NASA FIRMS (VIIRS_NOAA20_SP)"),
             format = "file"),
  tar_target(fig_serie_html_noaa20,
             grafico_serie_html(firms_mensual_noaa20,
                                "outputs/figs/noaa20/serie_mensual.html",
                                etiqueta_fuente = "VIIRS NOAA-20 (FIRMS)",
                                fuente = "Datos: NASA FIRMS (VIIRS_NOAA20_SP)"),
             format = "file"),
  tar_target(fig_climatologia_html_noaa20,
             grafico_climatologia_html(firms_mensual_noaa20,
                                       "outputs/figs/noaa20/climatologia_mensual.html",
                                       etiqueta_fuente = "VIIRS NOAA-20 (FIRMS)",
                                       fuente = "Datos: NASA FIRMS (VIIRS_NOAA20_SP)"),
             format = "file"),
  tar_target(fig_area_quemada_noaa20,
             grafico_area_quemada(area_quemada_mensual_noaa20,
                                  "outputs/figs/noaa20/area_quemada_mensual.png",
                                  etiqueta_ba = "VNP64A1, S-NPP",
                                  fuente = "Datos: NASA LP DAAC (VNP64A1 v2, Suomi-NPP)"),
             format = "file"),
  tar_target(fig_comparacion_noaa20,
             grafico_comparacion(firms_mensual_noaa20, area_quemada_mensual_noaa20,
                                 "outputs/figs/noaa20/detecciones_vs_area_quemada.png",
                                 etiqueta_ba = "VNP64A1, S-NPP",
                                 fuente = "Datos: NASA FIRMS (VIIRS_NOAA20_SP) y LP DAAC (VNP64A1, Suomi-NPP)"),
             format = "file"),
  tar_target(tabla_area_quemada_noaa20,
             tabla_area_quemada_csv(area_quemada_parque_noaa20,
                                    "outputs/tables/noaa20/area_quemada_anual.csv"),
             format = "file"),
  tar_target(fig_cobertura_quemas_noaa20,
             grafico_cobertura_quemas(cobertura_quemas_noaa20,
                                      humedales_quemas_noaa20,
                                      "outputs/figs/noaa20/cobertura_area_quemada.png",
                                      fuente = "Datos: NASA LP DAAC (VNP64A1, Suomi-NPP), ESA WorldCover 2021 y SINAC"),
             format = "file"),
  tar_target(fig_climatologia_comparada_noaa20,
             grafico_climatologia_comparada(firms_mensual_noaa20,
                                            area_quemada_mensual_noaa20,
                                            "outputs/figs/noaa20/climatologia_comparada.png",
                                            etiqueta_ba = "VNP64A1, S-NPP",
                                            fuente = "Datos: NASA FIRMS (VIIRS_NOAA20_SP) y LP DAAC (VNP64A1, Suomi-NPP)"),
             format = "file"),
  tar_target(fig_cobertura_noaa20,
             grafico_cobertura(cobertura_noaa20, humedales_detecciones_noaa20,
                               "outputs/figs/noaa20/cobertura_detecciones.png",
                               fuente = "Datos: NASA FIRMS (VIIRS_NOAA20_SP), ESA WorldCover 2021 y SINAC"),
             format = "file"),
  tar_target(tabla_cobertura_noaa20,
             tabla_cobertura_csv(cobertura_noaa20,
                                 "outputs/tables/noaa20/cobertura_detecciones.csv"),
             format = "file"),
  tar_target(tabla_contraste_noaa20,
             tabla_contraste_csv(cobertura_noaa20, humedales_detecciones_noaa20,
                                 cobertura_quemas_noaa20, humedales_quemas_noaa20,
                                 "outputs/tables/noaa20/contraste_humedales.csv"),
             format = "file"),
  tar_target(tabla_borde_noaa20,
             tabla_borde_csv(borde_bosque_noaa20,
                             "outputs/tables/noaa20/contexto_borde.csv"),
             format = "file"),
  tar_target(tabla_csv_noaa20,
             tabla_resumen_csv(firms_parque_noaa20, area_quemada_parque_noaa20,
                               "outputs/tables/noaa20/resumen_anual.csv"),
             format = "file"),
  tar_target(tabla_html_noaa20,
             tabla_resumen_html(firms_parque_noaa20, area_quemada_parque_noaa20,
                                "outputs/tables/noaa20/resumen_anual.html",
                                etiqueta_fuente = "VIIRS NOAA-20 (FIRMS)",
                                etiqueta_ba = "VNP64A1, S-NPP"),
             format = "file"),

  # --- Reporte Quarto → index.html en la raíz (GitHub Pages) ---------------
  # El .qmd llama funciones de R/ directamente (crear_mapa_temporal,
  # crear_grafico_cobertura, ...) que carga con tar_source(). tar_quarto solo
  # detecta como dependencias los targets leídos con tar_read()/tar_load(), no
  # esas funciones, así que sin `extra_files` un cambio en R/ dejaba el reporte
  # sin regenerar (y publicaba una versión vieja). Cualquier cambio en R/
  # invalida ahora el render; el costo es re-renderizar (~20 s) también cuando
  # el cambio no afecta al reporte.
  tar_quarto(reporte, "analysis/index.qmd",
             extra_files = list.files("R", pattern = "[.][Rr]$",
                                      full.names = TRUE)),
  tar_target(pagina_principal, {
    reporte  # dependencia explícita del render
    file.copy("analysis/index.html", "index.html", overwrite = TRUE)
    "index.html"
  }, format = "file"),

  # --- Reporte VIIRS → viirs/index.html (misma publicación, otra ruta) -----
  # GitHub Pages sirve la raíz de main, así que viirs/index.html queda en
  # <sitio>/viirs/ sin tocar la URL del reporte MODIS.
  tar_quarto(reporte_viirs, "analysis/viirs.qmd",
             extra_files = list.files("R", pattern = "[.][Rr]$",
                                      full.names = TRUE)),
  tar_target(pagina_viirs, {
    reporte_viirs  # dependencia explícita del render
    dir.create("viirs", showWarnings = FALSE)
    file.copy("analysis/viirs.html", "viirs/index.html", overwrite = TRUE)
    "viirs/index.html"
  }, format = "file")
)
