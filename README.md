# Anomalías térmicas en el Parque Nacional Palo Verde

Pipeline reproducible en R que descarga las anomalías térmicas (detecciones de
fuego activo) del producto **MODIS_SP** de [NASA FIRMS](https://firms.modaps.eosdis.nasa.gov/)
y el **área quemada** mensual del producto **MCD64A1** (NASA LP DAAC)
para el **Parque Nacional Palo Verde** (Costa Rica), caracteriza la **cobertura
de la tierra** en la que ocurren y genera un **mapa animado a través del
tiempo**, un mapa interactivo con deslizador temporal, gráficos estadísticos
interactivos y tablas.

**Productos en línea**: <https://incendios-forestales.github.io/anomalias-termicas-paloverde/>

## Arquitectura

El flujo de trabajo está implementado con [{targets}](https://books.ropensci.org/targets/):

1. **Obtención de datos**
   - Detecciones: API de área de FIRMS, descargada en fragmentos de 5 días.
   - Área quemada: granulos mensuales de MCD64A1 v6.1 (500 m) desde LP DAAC,
     descubiertos vía el API CMR de Earthdata. FIRMS lista este producto como
     `BA_MODIS` pero su API no lo entrega como datos (responde vacío), por lo
     que se usa el producto original.
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
5. **Salidas**: animación GIF/MP4 (gganimate) con fondo de cobertura, mapa
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
mitad. **Las detecciones no bordean el bosque, lo rodean.**

La métrica es la fracción de bosque en un vecindario y no la distancia al borde
más cercano porque un `distance()` sobre el raster de 10 m del parque no
termina en tiempo razonable. Implementación en
[`R/contexto_paisaje.R`](R/contexto_paisaje.R).

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
| `fuente_firms` | Producto de FIRMS | `MODIS_SP` |

Constantes adicionales (buffer de descarga, tamaño de fragmento, fuentes
futuras) en [`R/constantes.R`](R/constantes.R).

## Estructura del repositorio

```
├── _targets.R          # definición del pipeline
├── R/                  # funciones: descarga (FIRMS, WFS), procesamiento,
│                       #   cobertura de la tierra, contexto paisajístico,
│                       #   visualización y tablas
├── analysis/index.qmd  # reporte Quarto → index.html (GitHub Pages)
├── data/raw/           # caché de datos crudos (no versionada)
├── outputs/            # figuras, mapas y tablas generados
├── Dockerfile          # rocker/geospatial + paquetes del proyecto
├── docker-compose.yml  # RStudio Server (puerto 8787)
└── renv.lock           # versiones fijadas de paquetes
```

## Fuentes de datos

| Fuente | Datos | Licencia/atribución |
|---|---|---|
| [NASA FIRMS](https://firms.modaps.eosdis.nasa.gov/) | Anomalías térmicas MODIS Collection 6.1 (MODIS_SP), DOI: 10.5067/FIRMS/MODIS/MCD14ML | Acceso abierto; se agradece atribución a NASA FIRMS |
| [NASA LP DAAC](https://lpdaac.usgs.gov/) | Área quemada mensual MCD64A1 v6.1 (500 m), DOI: 10.5067/MODIS/MCD64A1.061 | Acceso abierto con Earthdata Login; se agradece atribución a NASA LP DAAC |
| [SINAC](https://geos1pne.sirefor.go.cr/wfs) | Polígono del PN Palo Verde, capa oficial 1:5000 publicada en 2019 (`PNE:areas_silvestres_protegidas`), Registro Nacional de Humedales, actualización 2016–2018 (`PNE:registro_nacional_humedales`), y Cobertura Forestal 2023 (`PNE:cobertura_forestal_2023`) | Datos públicos del Estado costarricense |
| [ESA WorldCover](https://esa-worldcover.org/) | Cobertura de la tierra 2021 a 10 m (v200), DOI: 10.5281/zenodo.7254221 | CC BY 4.0; atribución a ESA WorldCover |

## Trabajo futuro

- Incorporar otras fuentes de FIRMS: MODIS_NRT y VIIRS (SNPP/NOAA-20/NOAA-21).
  La capa de descarga ya está parametrizada por `data_id` (ver `FUENTES_FIRMS`
  en `R/constantes.R`). Cobra urgencia porque las misiones Terra y Aqua
  terminan en 2027 y VIIRS es su continuidad: las detecciones de distintos
  sensores **no deben sumarse** entre sí. (El área quemada `BA_MODIS` se
  descartó como fuente FIRMS: su API la acepta pero responde vacío; ya se
  integra desde el producto original MCD64A1.)
- Aprovechar las bandas `Burn Date Uncertainty` y `QA` de MCD64A1, e integrar
  el producto VIIRS de área quemada (VNP64A1) como continuidad post-2027.
- Desagregar la clase «Bosque» de WorldCover en los tipos de la Cobertura
  Forestal del SINAC (deciduo, maduro, secundario), que aportan vocabulario
  ecológico local — el bosque deciduo es el bosque seco característico del
  parque.
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
