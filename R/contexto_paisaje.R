# Contexto paisajístico de las detecciones: ¿el fuego ocurre en el borde del
# bosque o en las áreas abiertas?
#
# En el mapa interactivo las detecciones parecen dibujar el contorno del bloque
# forestal, lo que sugiere un "efecto de borde". Para distinguir un patrón real
# de una ilusión geométrica (en un parque cuyo bosque es un bloque compacto,
# cualquier concentración en terreno abierto traza su silueta) se contrastan
# las detecciones contra un modelo nulo: puntos aleatorios dentro del parque,
# es decir, la hipótesis de que el fuego se distribuye como el paisaje
# disponible.
#
# Métrica de borde: fracción de bosque en un radio de RADIO_VECINDARIO_M
# alrededor del punto. Tiende a 1 en el interior del bosque y a 0 en terreno
# abierto; los valores intermedios son justamente los bordes. Se prefiere a la
# distancia al borde más cercano porque un `distance()` sobre el raster de 10 m
# del parque no termina en tiempo razonable.

VALOR_BOSQUE_WORLDCOVER <- 10
RADIO_VECINDARIO_M <- 500
N_PUNTOS_ALEATORIOS <- 3000
SEMILLA_ALEATORIOS <- 42

# Umbrales de la clasificación abierto / borde / interior, como fracción de
# bosque en el vecindario.
CORTES_BORDE <- c(0.2, 0.8)
ETIQUETAS_BORDE <- c(
  "Abierto (<20 % de bosque alrededor)",
  "Borde (20–80 %)",
  "Interior de bosque (>80 %)"
)

# Composición de clases WorldCover dentro del parque: el paisaje disponible
# contra el cual se comparan las detecciones.
composicion_paisaje <- function(parque, archivos_worldcover, bbox) {
  recorte <- recortar_worldcover(archivos_worldcover, bbox)
  poligono <- terra::vect(sf::st_transform(parque, terra::crs(recorte)))
  conteo <- terra::freq(terra::mask(terra::crop(recorte, poligono), poligono))
  tibble::tibble(
    clase = unname(CLASES_WORLDCOVER[as.character(conteo$value)]),
    pct_paisaje = 100 * conteo$count / sum(conteo$count)
  ) |>
    dplyr::arrange(dplyr::desc(pct_paisaje))
}

# Fracción de bosque en un radio dado alrededor de cada geometría. El buffer se
# construye en CRTM05 (metros) y se reproyecta al raster: terra::extract no
# acepta el argumento `buffer` de raster::extract.
fraccion_bosque_vecindario <- function(geometrias, bosque, radio_m) {
  vecindarios <- sf::st_transform(geometrias, CRS_CRTM05) |>
    sf::st_buffer(radio_m) |>
    sf::st_transform(terra::crs(bosque))
  terra::extract(bosque, terra::vect(vecindarios), fun = mean, na.rm = TRUE)[[2]]
}

# Compara la situación paisajística de las detecciones contra puntos aleatorios
# dentro del parque. Retorna la tabla de porcentajes por situación y las
# medidas de apoyo que cita el reporte.
contexto_borde <- function(puntos, parque, archivos_worldcover, bbox,
                           radio_m = RADIO_VECINDARIO_M,
                           n_aleatorios = N_PUNTOS_ALEATORIOS,
                           semilla = SEMILLA_ALEATORIOS) {
  recorte <- recortar_worldcover(archivos_worldcover, bbox)
  poligono <- sf::st_transform(parque, terra::crs(recorte))
  # Recorte al parque antes de reclasificar: el bbox de descarga lleva buffer
  # y multiplica el costo de las extracciones.
  bosque <- terra::classify(
    terra::crop(recorte, terra::vect(poligono)),
    cbind(VALOR_BOSQUE_WORLDCOVER, 1), others = 0
  )

  set.seed(semilla)
  aleatorios <- sf::st_sample(sf::st_union(poligono), n_aleatorios)

  f_detecciones <- fraccion_bosque_vecindario(sf::st_geometry(puntos), bosque,
                                              radio_m)
  f_aleatorios  <- fraccion_bosque_vecindario(aleatorios, bosque, radio_m)
  f_detecciones <- f_detecciones[!is.na(f_detecciones)]
  f_aleatorios  <- f_aleatorios[!is.na(f_aleatorios)]

  situacion <- function(x) {
    cut(x, breaks = c(-Inf, CORTES_BORDE, Inf), labels = ETIQUETAS_BORDE)
  }
  reparto <- function(x) {
    as.numeric(100 * table(situacion(x)) / length(x))
  }

  es_borde <- function(x) x > CORTES_BORDE[1] & x < CORTES_BORDE[2]
  prueba <- stats::prop.test(
    c(sum(es_borde(f_detecciones)), sum(es_borde(f_aleatorios))),
    c(length(f_detecciones), length(f_aleatorios))
  )

  list(
    tabla = tibble::tibble(
      situacion = ETIQUETAS_BORDE,
      pct_detecciones = reparto(f_detecciones),
      pct_aleatorio = reparto(f_aleatorios)
    ),
    n_detecciones = length(f_detecciones),
    n_aleatorios = length(f_aleatorios),
    radio_m = radio_m,
    mediana_detecciones = stats::median(f_detecciones),
    mediana_aleatorios = stats::median(f_aleatorios),
    p_borde = prueba$p.value
  )
}

# Widget DT con el contraste de situación paisajística (para el reporte).
crear_tabla_borde <- function(contexto) {
  DT::datatable(
    contexto$tabla |>
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) round(x, 1))),
    colnames = c("Situación paisajística", "% de las detecciones",
                 "% esperado por azar"),
    caption = paste0(
      "Situación paisajística de las detecciones frente a ",
      contexto$n_aleatorios, " puntos aleatorios dentro del parque ",
      "(fracción de bosque en ", contexto$radio_m, " m a la redonda)"
    ),
    options = list(dom = "t"),
    rownames = FALSE
  )
}

tabla_borde_csv <- function(contexto, dest) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(contexto$tabla, dest)
  dest
}
