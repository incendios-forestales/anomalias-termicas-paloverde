# Anomalías térmicas en el Parque Nacional Palo Verde

Pipeline reproducible en R que descarga las anomalías térmicas (detecciones de
fuego activo) de [NASA FIRMS](https://firms.modaps.eosdis.nasa.gov/) y el
**área quemada** mensual de NASA LP DAAC para el **Parque Nacional Palo
Verde** (Costa Rica), caracteriza la **cobertura de la tierra** en la que
ocurren y genera un **mapa animado a través del tiempo**, un mapa interactivo
con deslizador temporal, gráficos estadísticos interactivos y tablas.

Cada sensor se procesa por separado y tiene su propio juego de productos:

| Plataforma | Fuego activo (FIRMS) | Área quemada | Registro | Reporte |
|---|---|---|---|---|
| MODIS (Terra/Aqua, ~1 km) | `MODIS_SP` + `MODIS_NRT` | MCD64A1 v6.1 | desde 2001 | [modis/](https://incendios-forestales.github.io/anomalias-termicas-paloverde/modis/) |
| VIIRS (Suomi-NPP, 375 m) | `VIIRS_SNPP_SP` + `_NRT` | VNP64A1 v002 | desde 2012 | [snpp/](https://incendios-forestales.github.io/anomalias-termicas-paloverde/snpp/) |
| VIIRS (NOAA-20, 375 m) | `VIIRS_NOAA20_SP` + `_NRT` | VNP64A1 (de S-NPP)¹ | desde 2018 | [noaa20/](https://incendios-forestales.github.io/anomalias-termicas-paloverde/noaa20/) |
| VIIRS (NOAA-21, 375 m) | `VIIRS_NOAA21_NRT`² | VNP64A1 (de S-NPP)¹ | desde 2024 | [noaa21/](https://incendios-forestales.github.io/anomalias-termicas-paloverde/noaa21/) |

¹ NOAA-20 y NOAA-21 no tienen producto de área quemada propio (`VJ164A1` no
está publicado en CMR), así que toman el de Suomi-NPP, recortado a su ventana
temporal y **rotulado como tal** en toda figura, tabla y leyenda.
² NOAA-21 es la única plataforma **sin procesamiento estándar**: todos sus
datos son en tiempo casi real y por tanto provisionales.

Cada serie se sirve de su procesamiento estándar y, a continuación, de su cola
en tiempo casi real, de modo que llega hasta hace pocos días; ese tramo va
marcado como provisional en todos los productos. Las series **no se suman ni
se empalman** entre plataformas: más resolución detecta más fuegos y más
satélites observan más veces, así que una serie combinada mostraría saltos
—en 2012, 2018 y 2024— que reflejarían el instrumental disponible y no el
régimen de fuego.

**La portada del sitio** presenta las cuatro y reúne los hallazgos comunes:
<https://incendios-forestales.github.io/anomalias-termicas-paloverde/>

Tener cuatro series paralelas sirve para tres cosas: **continuidad** (Terra y
Aqua terminan en 2027 y las plataformas VIIRS son su relevo), **contraste**
—cuando cuatro instrumentos independientes coinciden, el hallazgo no es
artefacto de ninguno— y **cobertura reciente** gracias a las colas en tiempo
casi real.

## Arquitectura

El flujo de trabajo está implementado con [{targets}](https://books.ropensci.org/targets/):

1. **Obtención de datos**
   - Detecciones: API de área de FIRMS, descargada en fragmentos de 5 días.
   - Área quemada: granulos mensuales de MCD64A1 v6.1 y VNP64A1 v002 (500 m)
     desde LP DAAC, descubiertos vía el API CMR de Earthdata. FIRMS lista
     estos productos (`BA_MODIS`, `BA_VIIRS`) pero su API no los entrega como
     datos (responde vacío), por lo que se usan los originales. No existe
     versión en tiempo casi real del área quemada: el método necesita observar
     la cicatriz durante semanas, así que el tramo más reciente de cada serie
     tiene detecciones pero todavía no superficie.
   - Polígono del parque y capas nacionales (humedales, cobertura forestal):
     WFS del SINAC.
   - Cobertura de la tierra: teselas de ESA WorldCover 2021 (10 m) desde S3.
2. **Procesamiento**: conversión a puntos `sf`, recorte estricto al polígono
   del parque y reproyección a CRTM05 (EPSG:5367); agregación mensual.
3. **Análisis de cobertura**: composición de clases en el *footprint* de cada
   detección y contraste con el Registro Nacional de Humedales (ver más abajo).
4. **Contexto paisajístico**: contraste de las detecciones contra un modelo
   nulo de puntos aleatorios en el parque, para distinguir un efecto de borde
   real de una ilusión geométrica (ver más abajo).
5. **Salidas**: animación GIF/MP4 (gganimate) con fondo de cobertura, video
   estilo cartel con relieve y contadores acumulados (ver más abajo), mapa
   leaflet con deslizador temporal, control de pantalla completa y capas
   conmutables (detecciones, límite del parque, cobertura y capas del
   SINAC), serie temporal
   mensual y climatología (PNG + plotly interactivo), cobertura por clase,
   tablas (CSV y HTML) y reporte Quarto (`index.html`).

### Descarga idempotente y reanudable

Cada fragmento de fechas es una rama dinámica de targets respaldada por un CSV
en `data/raw/firms/<data_id>/<inicio>_<fin>.csv`:

- Si la ejecución se interrumpe, volver a correr `targets::tar_make()` continúa
  exactamente donde quedó (los CSV existentes no se vuelven a descargar).
- Los límites de los fragmentos están anclados a una rejilla fija
  (2000-11-01 + k·5 días), por lo que **ampliar el rango de fechas solo
  descarga los fragmentos nuevos** sin invalidar los existentes.
- La escritura es atómica (`.part` → renombrar): nunca queda un CSV truncado.

Las capas de contexto (WFS del SINAC, teselas de WorldCover) también se
cachean en `data/raw/` y solo se descargan la primera vez.

### Cobertura de la tierra por *footprint*

Una detección MODIS no es un punto: es un píxel de ~1 km, mayor fuera del
nadir. Asignarle la clase del punto exacto sobre un mapa de 10 m sería
precisión espuria, así que la cobertura se caracteriza sobre el **footprint
completo** —una elipse con las dimensiones reales del píxel, columnas `scan` ×
`track`— y se reporta la fracción por clase y la clase dominante.

El resultado se contrasta con el Registro Nacional de Humedales del SINAC
(actualización 2016–2018 del Inventario Nacional de Humedales).
Dos advertencias que el reporte documenta:

- WorldCover es una foto fija de 2021 frente a un registro de 2001–2026.
- El registro de humedales asigna **una clase por polígono**, no por píxel:
  sirve para determinar si un sitio es humedal, no qué vegetación ardió. Las
  capas del SINAC son además inventarios de rasgos específicos (bosque,
  humedales) y no cubren todo el territorio, por lo que complementan a
  WorldCover pero no lo sustituyen como clasificación exhaustiva.

### Contexto paisajístico: ¿bordean el bosque o lo rodean?

En el mapa las detecciones parecen dibujar el contorno del bloque forestal.
Para distinguir un efecto de borde real de una ilusión geométrica —en un parque
cuyo bosque es un bloque compacto, cualquier concentración en terreno abierto
traza su silueta— se compara cada detección contra un modelo nulo de puntos
aleatorios dentro del parque, midiendo la fracción de bosque en 500 m a la
redonda.

No hay efecto de borde: la categoría intermedia (20–80 % de bosque alrededor)
no está sobrerrepresentada (25,4 % contra 28,2 % esperado por azar, *p* = 0,38).
Lo que sí está desplazado son los extremos: las detecciones ocurren en terreno
abierto casi al doble de lo esperado y en interior de bosque a menos de la
mitad. **El fuego no bordea el bosque, lo rodea.**

El área quemada (MCD64A1) agudiza el patrón: el 74,5 % de las hectáreas está
en terreno abierto y solo el 3,4 % en interior de bosque, contra un 38 %
esperado por azar. La diferencia entre ambos instrumentos apunta al tamaño del
píxel — el footprint MODIS de ~1 km invade el bosque cuando el fuego arde en
la marisma contigua, mientras que el píxel de 500 m de MCD64A1 sitúa mejor la
cicatriz—, de modo que buena parte de las detecciones en interior de bosque es
un artefacto de resolución y no fuego de dosel.

La métrica es la fracción de bosque en un vecindario y no la distancia al borde
más cercano porque un `distance()` sobre el raster de 10 m del parque no
termina en tiempo razonable. Implementación en
[`R/contexto_paisaje.R`](R/contexto_paisaje.R).

### Video estilo cartel

`outputs/figs/<plataforma>/video_anomalias_termicas.mp4` resume los 25 años en ~1 minuto,
un cuadro por mes: detecciones de fuego con resplandor y estela de los meses
recientes, píxeles de área quemada y, por cada serie, el acumulado desde
2001 con el valor del mes en curso debajo (por separado: son magnitudes
complementarias y no sumables), en un estilo inspirado en los videos de
[Milos Popovic](https://milospopovic.net/).

- **Render cuadro a cuadro** con ggplot2 (PNG numerados ensamblados con el
  paquete `av`), no con gganimate: los contadores, la fecha y el resplandor
  multicapa cambian texto y número de capas en cada cuadro.
- **Relieve**: teselas [Terrain Tiles](https://registry.opendata.aws/terrain-tiles/)
  (Mapzen/AWS, formato Terrarium, públicas y sin autenticación), que
  codifican la elevación en los canales RGB del PNG:
  `elevación (m) = R·256 + G + B/256 − 32768`. Se cachean en `data/raw/dem/`
  y el hillshade se calcula con terra.
- **Fondo por cobertura**: dentro del parque el matiz del fondo viene de la
  clase de WorldCover (paleta oscura propia, no la oficial: sobre fondo
  oscuro competiría con las detecciones y quemas) y la luminancia del
  hillshade; fuera del parque, una rampa neutra atenuada.

Implementación en [`R/video.R`](R/video.R). El cartel estático
`outputs/figs/<plataforma>/cartel_resumen.png` (target `cartel_resumen`) resume las dos
series mensuales con el mismo estilo visual, en dos paneles apilados —
nunca un doble eje: son magnitudes no comparables — con el mes máximo de
cada serie rotulado.

## Requisitos

- Una clave (MAP_KEY) gratuita del API de FIRMS:
  <https://firms.modaps.eosdis.nasa.gov/api/map_key/>
- Un token de Earthdata Login (cuenta gratuita) para descargar MCD64A1:
  <https://urs.earthdata.nasa.gov/> → *Generate Token*. Los tokens expiran a
  los ~60 días; si la descarga devuelve HTTP 401, hay que regenerarlo.
- Docker (recomendado) o R ≥ 4.5 con renv (alternativa, p. ej. en Windows).

## Uso con Docker (recomendado)

```bash
cp .env.example .env            # defina RSTUDIO_PASSWORD
cp .Renviron.example .Renviron  # ingrese FIRMS_MAP_KEY y EARTHDATA_TOKEN
docker compose up -d --build
```

Abra RStudio Server en <http://localhost:8787> (usuario `rstudio`, la contraseña
de `.env`), abra el proyecto `anomalias-termicas-paloverde.Rproj` y ejecute:

```r
renv::restore()      # instala las versiones fijadas de los paquetes
targets::tar_make()  # ejecuta el pipeline completo
```

También puede ejecutarse sin RStudio:

```bash
docker compose run --rm rstudio Rscript -e "renv::restore(); targets::tar_make()"
```

## Uso con renv (sin Docker, p. ej. Windows)

1. Instale [R ≥ 4.5](https://cran.r-project.org/),
   [RTools](https://cran.r-project.org/bin/windows/Rtools/) (Windows) y
   [Quarto](https://quarto.org/).
2. Clone el repositorio, copie `.Renviron.example` a `.Renviron` e ingrese sus
   credenciales (`FIRMS_MAP_KEY` y `EARTHDATA_TOKEN`).
3. En R, dentro del proyecto:

```r
renv::restore()
targets::tar_make()
```

## Configuración del pipeline

Los parámetros se editan al inicio de [`_targets.R`](_targets.R):

| Parámetro | Descripción | Valor por defecto |
|---|---|---|
| `fecha_inicio` | Inicio del período | `2001-01-01` |
| `fecha_fin` | Fin del período (se recorta a lo disponible) | `2100-01-01` (= todo lo disponible) |

Las plataformas se definen en la tabla `PLATAFORMAS` de
[`R/plataformas.R`](R/plataformas.R): una fila por plataforma, de la que se
derivan las colecciones de FIRMS, el producto de área quemada, los
directorios de salida y **todos los rótulos visibles**. Agregar una plataforma
es agregar una fila.

### Cómo leer el pipeline

Las cuatro cadenas se generan con `tarchetypes::tar_map`, así que los nombres
de target **no aparecen literalmente** en `_targets.R`: cada target del bloque
`tar_map` existe cuatro veces con el sufijo de la clave de plataforma. No
existe `firms_parque`; existen `firms_parque_modis`, `firms_parque_snpp`,
`firms_parque_noaa20` y `firms_parque_noaa21`.

```r
targets::tar_manifest(fields = "name")   # lista los targets generados
targets::tar_visnetwork()                # grafo, con el nombre de cada plataforma
```

El área quemada se organiza por **producto** y no por plataforma
(`quemas_mcd64a1`, `quemas_vnp64a1`): hay exactamente dos y VNP64A1 alimenta a
tres plataformas.

Constantes adicionales (buffer de descarga, tamaño de fragmento, productos de
área quemada) en [`R/constantes.R`](R/constantes.R).

## Estructura del repositorio

```
├── _targets.R          # definición del pipeline
├── R/                  # funciones: descarga (FIRMS, WFS), procesamiento,
│                       #   cobertura de la tierra, contexto paisajístico,
│                       #   visualización y tablas
├── analysis/portada.qmd # portada         → index.html (GitHub Pages)
├── analysis/modis.qmd   # reporte MODIS   → modis/index.html
├── analysis/snpp.qmd    # reporte S-NPP   → snpp/index.html
├── analysis/noaa20.qmd  # reporte NOAA-20 → noaa20/index.html
├── analysis/noaa21.qmd  # reporte NOAA-21 → noaa21/index.html
├── data/raw/           # caché de datos crudos (no versionada)
├── outputs/            # figuras, mapas y tablas, en una carpeta por
│                       #   plataforma: figs/modis/, figs/snpp/, ...
├── Dockerfile          # rocker/geospatial + paquetes del proyecto
├── docker-compose.yml  # RStudio Server (puerto 8787)
└── renv.lock           # versiones fijadas de paquetes
```

## Fuentes de datos

| Fuente | Datos | Licencia/atribución |
|---|---|---|
| [NASA FIRMS](https://firms.modaps.eosdis.nasa.gov/) | Anomalías térmicas MODIS Collection 6.1 (MODIS_SP), DOI: 10.5067/FIRMS/MODIS/MCD14ML, y VIIRS 375 m de Suomi-NPP (VIIRS_SNPP_SP), DOI: 10.5067/VIIRS/VNP14IMG.002 | Acceso abierto; se agradece atribución a NASA FIRMS |
| [NASA LP DAAC](https://lpdaac.usgs.gov/) | Área quemada mensual MCD64A1 v6.1 (500 m), DOI: 10.5067/MODIS/MCD64A1.061, y VNP64A1 v002 (VIIRS/NPP, 500 m), DOI: 10.5067/VIIRS/VNP64A1.002 | Acceso abierto con Earthdata Login; se agradece atribución a NASA LP DAAC |
| [SINAC](https://geos1pne.sirefor.go.cr/wfs) | Polígono del PN Palo Verde, capa oficial 1:5000 publicada en 2019 (`PNE:areas_silvestres_protegidas`), Registro Nacional de Humedales, actualización 2016–2018 (`PNE:registro_nacional_humedales`), y Cobertura Forestal 2023 (`PNE:cobertura_forestal_2023`) | Datos públicos del Estado costarricense |
| [ESA WorldCover](https://esa-worldcover.org/) | Cobertura de la tierra 2021 a 10 m (v200), DOI: 10.5281/zenodo.7254221 | CC BY 4.0; atribución a ESA WorldCover |
| [Terrain Tiles](https://registry.opendata.aws/terrain-tiles/) | Modelo de elevación (formato Terrarium, zoom 13) para el relieve del video | Datos abiertos en AWS; atribución a Mapzen y las fuentes del DEM (SRTM, NASA) |

## Trabajo futuro

- Incorporar las fuentes de FIRMS que faltan: los productos en tiempo casi
  real (`MODIS_NRT`, `VIIRS_*_NRT`), empalmando la cola NRT de cada plataforma
  a su serie estándar y deduplicando el traslape, y NOAA-21, que hoy **solo**
  existe en NRT (desde 2024-01) y por eso no tiene aún una cadena propia.
- Comparar formalmente las tres plataformas en sus traslapes, en vez de solo
  publicarlas lado a lado: cuantificar cuánto de la diferencia entre series es
  resolución, cuánto hora de paso y cuánto episodios puntuales. El reporte de
  NOAA-20 muestra por qué hace falta: casi toda su ventaja aparente sobre
  Suomi-NPP proviene de que su registro llega un mes más lejos y ese mes
  (mayo de 2026) concentró un episodio grande.
- Vigilar el fin de las misiones Terra y Aqua en 2027 y decidir cuál serie
  pasa a ser la de referencia del proyecto.
- Aprovechar las bandas `Burn Date Uncertainty` y `QA` de los productos de
  área quemada.
- Revisar el hueco de 2024 en VIIRS_SNPP_SP (cero detecciones ese año frente a
  siete de MODIS, con área quemada VNP64A1 no nula): confirmar si es un
  artefacto del producto o del envejecimiento de Suomi-NPP.
- Desagregar la clase «Bosque» de WorldCover en los tipos de la Cobertura
  Forestal del SINAC (deciduo, maduro, secundario), que aportan vocabulario
  ecológico local — el bosque deciduo es el bosque seco característico del
  parque.
- Cobertura con resolución temporal: WorldCover es una clasificación del año
  2021 completo, sin fecha por píxel, así que no distingue la marisma
  inundada de la marisma seca en el mes de cada detección. Alternativas:
  [Google Dynamic World](https://dynamicworld.app/) (cobertura a 10 m por
  escena de Sentinel-2, con fecha, desde mediados de 2015 — cubriría solo el
  tramo final del registro) o compuestos estacionales propios de Sentinel-2
  (2015+) o Landsat (todo el registro, a 30 m).
- Modelar los factores de ignición con covariables de accesibilidad (cercanía
  a caminos, linderos y zonas de cultivo). El análisis de contexto paisajístico
  descarta que el patrón visual sea un efecto de borde, pero su modelo nulo
  supone que cualquier punto del parque tiene la misma probabilidad de arder;
  identificar la causa de las igniciones exige covariables que hoy no se
  descargan.
- Reestructurar el reporte para que solo lea targets (`tar_read()`) en lugar
  de llamar funciones de `R/`; hoy esa dependencia se cubre con `extra_files`
  en `tar_quarto()`.

## Licencia

El código se distribuye bajo la [licencia MIT](LICENSE). Los datos conservan
las condiciones de sus fuentes originales.
