# Pipeline de anomalías térmicas (FIRMS MODIS_SP) en el PN Palo Verde.
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
    "ggplot2", "gganimate", "leaflet", "leaflet.extras2", "htmlwidgets", "DT"
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

  # --- Salidas: animaciones -------------------------------------------------
  tar_target(anim_gif,
             animar_detecciones(firms_parque, parque, firms_mensual,
                                "outputs/figs/animacion_mensual.gif"),
             format = "file"),
  tar_target(anim_mp4,
             animar_detecciones(firms_parque, parque, firms_mensual,
                                "outputs/figs/animacion_mensual.mp4"),
             format = "file"),

  # --- Salidas: mapa interactivo -------------------------------------------
  tar_target(mapa_html,
             mapa_leaflet_temporal(firms_parque, parque,
                                   "outputs/maps/mapa_temporal.html"),
             format = "file"),

  # --- Salidas: gráficos y tablas ------------------------------------------
  tar_target(fig_serie,
             grafico_serie_temporal(firms_mensual, "outputs/figs/serie_mensual.png"),
             format = "file"),
  tar_target(fig_climatologia,
             grafico_climatologia(firms_mensual, "outputs/figs/climatologia_mensual.png"),
             format = "file"),
  tar_target(tabla_csv,
             tabla_resumen_csv(firms_parque, "outputs/tables/resumen_anual.csv"),
             format = "file"),
  tar_target(tabla_html,
             tabla_resumen_html(firms_parque, "outputs/tables/resumen_anual.html"),
             format = "file"),

  # --- Reporte Quarto → index.html en la raíz (GitHub Pages) ---------------
  tar_quarto(reporte, "analysis/index.qmd"),
  tar_target(pagina_principal, {
    reporte  # dependencia explícita del render
    file.copy("analysis/index.html", "index.html", overwrite = TRUE)
    "index.html"
  }, format = "file")
)
