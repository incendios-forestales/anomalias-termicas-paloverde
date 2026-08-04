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
