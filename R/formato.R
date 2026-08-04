# Formato de números para textos en español.
#
# R escribe los decimales con punto y el reporte es en español, donde el
# separador decimal es la coma. Sin esto la prosa mezcla ambas convenciones:
# las cifras interpoladas con inline R salen "74.5" junto a las escritas a
# mano como "95,5". Los CSV NO se tocan: ahí el punto es lo esperable para
# lectura por máquina (readr::write_csv).
#
# `decimales` fija la precisión Y los ceros a la derecha, de modo que una
# serie de porcentajes se lea pareja ("54,0 %" junto a "58,2 %") en lugar de
# alternar precisiones. Sin separador de miles: los valores del reporte son
# de cuatro o cinco dígitos y la prosa ya los escribe sin él.
num_es <- function(x, decimales = 1) {
  format(round(x, decimales), decimal.mark = ",", big.mark = "",
         nsmall = decimales, trim = TRUE, scientific = FALSE)
}

# Envoltorio para las tablas DT: aplica la coma decimal a las columnas
# indicadas conservando el dato numérico subyacente (el ordenamiento de la
# tabla sigue siendo numérico, no alfabético).
formato_dt_es <- function(tabla, columnas, decimales = 1) {
  DT::formatRound(tabla, columns = columnas, digits = decimales,
                  mark = "", dec.mark = ",")
}

# Etiqueta legible para la confianza de una detección de FIRMS, que cambia de
# dominio según el sensor: MODIS la reporta como porcentaje 0-100 y VIIRS como
# categoría l/n/h. `leer_y_unir_csv()` deja la columna como carácter, así que
# aquí se opera sobre texto y se cubren ambas representaciones.
# Nivel de procesamiento en palabras. El dato provisional se marca como tal
# en todo producto donde aparece: proviene del procesamiento en tiempo casi
# real, cuya geolocalización y filtrado de falsas alarmas se refinan cuando
# NASA lo reprocesa a estándar, de modo que puede cambiar retroactivamente.
etiqueta_nivel <- function(x) {
  ifelse(x == "SP", "estándar", "tiempo casi real (provisional)")
}

etiqueta_confianza <- function(x) {
  categorias <- c(l = "baja", n = "nominal", h = "alta")
  ifelse(x %in% names(categorias), categorias[x], paste0(x, " %"))
}
