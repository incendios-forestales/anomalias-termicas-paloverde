# Ayudantes de prosa para los reportes Quarto.
#
# Encapsulan los cálculos que la prosa interpola con inline R, de modo que
# cualquier reporte (MODIS, VIIRS) los construya desde sus propios targets en
# lugar de duplicar el código en cada qmd. Los ayudantes devuelven el número
# YA formateado en español (coma decimal, num_es); para operar con los
# valores hay que leerlos del objeto de origen (p. ej. borde$tabla), no del
# texto formateado.

# Ayudantes de la sección "¿El fuego bordea el bosque o lo rodea?".
# Retorna una lista con:
#   pct_paisaje(clase)      % del paisaje del parque en esa clase WorldCover
#   pct_dominante(clase)    % de detecciones con esa clase dominante
#   pct_ha_dominante(clase) % de hectáreas quemadas con esa clase dominante
#   pct_situacion(i, col)   celda [i, col] de la tabla de situación de borde
#   veces_menos_bosque      cociente azar/área quemada en interior de bosque
ayudantes_borde <- function(borde, paisaje, cobertura, cobertura_quemas) {
  dominantes <- clase_dominante(cobertura) |>
    dplyr::count(clase) |>
    dplyr::mutate(pct = 100 * n / sum(n))
  ha_dominante <- clase_dominante(cobertura_quemas) |>
    dplyr::summarise(ha = sum(area_ha), .by = clase) |>
    dplyr::mutate(pct = 100 * ha / sum(ha))
  list(
    pct_paisaje = function(x) {
      num_es(paisaje$pct_paisaje[match(x, paisaje$clase)])
    },
    pct_dominante = function(x) {
      num_es(dominantes$pct[match(x, dominantes$clase)])
    },
    pct_ha_dominante = function(x) {
      num_es(ha_dominante$pct[match(x, ha_dominante$clase)])
    },
    pct_situacion = function(i, col) num_es(borde$tabla[[col]][i]),
    veces_menos_bosque = num_es(
      borde$tabla$pct_aleatorio[3] / borde$tabla$pct_area_quemada[3], 0
    )
  )
}

# Cifras del hallazgo "las quemas del pastizal ocurren en la marisma": cuántas
# detecciones tienen `clase` como cobertura dominante, qué porcentaje cae en
# humedal registrado, cuánto del footprint cubre el humedal en promedio y
# cuántas detecciones concentra el polígono más frecuente del registro (el
# sector estuarino rotulado "Manglar" en el caso de Palo Verde). Se calculan
# en vez de escribirse en la prosa: cambian con la fuente y al ampliar el
# registro. Devuelve números formateados en español, salvo los conteos.
resumen_hallazgo_humedales <- function(cobertura, humedales_detecciones,
                                       clase_objetivo = "Pastizal") {
  cruce <- clase_dominante(cobertura) |>
    dplyr::left_join(humedales_detecciones, by = "id_deteccion")
  objetivo <- cruce[cruce$clase == clase_objetivo, ]
  poligono <- sort(table(cruce$nom_hum[cruce$en_humedal]), decreasing = TRUE)
  list(
    n = nrow(objetivo),
    pct_en_humedal = num_es(100 * mean(objetivo$en_humedal)),
    pct_footprint = num_es(100 * mean(objetivo$fraccion_humedal), 0),
    poligono_nombre = names(poligono)[1],
    poligono_n = unname(poligono[1])
  )
}

# Resumen comparativo de las plataformas, para la portada. Lee los targets de
# cada una y devuelve una fila por plataforma. Las cifras NUNCA se escriben a
# mano en la prosa: se interpolan desde aquí, de modo que no puedan quedar
# desfasadas respecto del pipeline.
resumen_multiplataforma <- function(claves, store) {
  purrr::map(claves, function(k) {
    p <- targets::tar_read_raw(paste0("firms_parque_", k), store = store)
    q <- targets::tar_read_raw(paste0("area_quemada_parque_", k), store = store)
    r <- targets::tar_read_raw(paste0("rangos_", k), store = store)
    e <- etiquetas_plataforma(k)
    fechas <- sf::st_drop_geometry(p)$acq_date
    tibble::tibble(
      clave = k,
      plataforma = e$plataforma,
      fuentes = e$ids_fuente,
      desde = min(fechas),
      hasta = max(fechas),
      detecciones = nrow(p),
      hectareas = round(sum(sf::st_drop_geometry(q)$area_ha)),
      area_quemada = e$etiqueta_ba,
      solo_provisional = all(r$nivel == "NRT")
    )
  }) |>
    purrr::list_rbind() |>
    dplyr::arrange(desde)
}

# Rango entre plataformas de una cifra, ya formateado ("58,2 % y 79,9 %").
# La portada enuncia los hallazgos comunes como rangos y no repitiendo una
# cifra por plataforma: cada reporte da la suya.
rango_entre_plataformas <- function(valores, decimales = 1) {
  paste(num_es(min(valores), decimales), "y", num_es(max(valores), decimales))
}
