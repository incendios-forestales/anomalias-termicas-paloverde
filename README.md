# Anomalías térmicas en el Parque Nacional Palo Verde

Pipeline reproducible en R que descarga las anomalías térmicas (detecciones de
fuego activo) del producto **MODIS_SP** de [NASA FIRMS](https://firms.modaps.eosdis.nasa.gov/)
para el **Parque Nacional Palo Verde** (Costa Rica) y genera un **mapa animado a
través del tiempo**, un mapa interactivo con deslizador temporal, gráficos
estadísticos y tablas.

**Productos en línea**: <https://incendios-forestales.github.io/anomalias-termicas-paloverde/>

## Arquitectura

El flujo de trabajo está implementado con [{targets}](https://books.ropensci.org/targets/):

1. **Obtención de datos**
   - Polígono del parque: WFS del SINAC (`PNE:areas_silvestres_protegidas`).
   - Detecciones: API de área de FIRMS, descargada en fragmentos de 5 días.
2. **Procesamiento**: conversión a puntos `sf`, recorte estricto al polígono
   del parque y reproyección a CRTM05 (EPSG:5367); agregación mensual.
3. **Salidas**: animación GIF/MP4 (gganimate), mapa leaflet con deslizador
   temporal, serie temporal mensual, climatología (estacionalidad), tabla
   resumen anual (CSV y HTML) y reporte Quarto (`index.html`).

### Descarga idempotente y reanudable

Cada fragmento de fechas es una rama dinámica de targets respaldada por un CSV
en `data/raw/firms/<data_id>/<inicio>_<fin>.csv`:

- Si la ejecución se interrumpe, volver a correr `targets::tar_make()` continúa
  exactamente donde quedó (los CSV existentes no se vuelven a descargar).
- Los límites de los fragmentos están anclados a una rejilla fija
  (2000-11-01 + k·5 días), por lo que **ampliar el rango de fechas solo
  descarga los fragmentos nuevos** sin invalidar los existentes.
- La escritura es atómica (`.part` → renombrar): nunca queda un CSV truncado.

## Requisitos

- Una clave (MAP_KEY) gratuita del API de FIRMS:
  <https://firms.modaps.eosdis.nasa.gov/api/map_key/>
- Docker (recomendado) o R ≥ 4.5 con renv (alternativa, p. ej. en Windows).

## Uso con Docker (recomendado)

```bash
cp .env.example .env            # defina RSTUDIO_PASSWORD
cp .Renviron.example .Renviron  # ingrese su FIRMS_MAP_KEY
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
2. Clone el repositorio, copie `.Renviron.example` a `.Renviron` e ingrese su
   clave.
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
├── R/                  # funciones (descarga WFS/FIRMS, procesamiento, visualización)
├── analysis/index.qmd  # reporte Quarto → index.html (GitHub Pages)
├── data/               # datos crudos y procesados (no versionados)
├── outputs/            # figuras, mapas y tablas generados
├── Dockerfile          # rocker/geospatial + paquetes del proyecto
├── docker-compose.yml  # RStudio Server (puerto 8787)
└── renv.lock           # versiones fijadas de paquetes
```

## Fuentes de datos

| Fuente | Datos | Licencia/atribución |
|---|---|---|
| [NASA FIRMS](https://firms.modaps.eosdis.nasa.gov/) | Anomalías térmicas MODIS Collection 6.1 (MODIS_SP), DOI: 10.5067/FIRMS/MODIS/MCD14ML | Acceso abierto; se agradece atribución a NASA FIRMS |
| [SINAC](https://geos1pne.sirefor.go.cr/wfs) | Polígono del PN Palo Verde (capa `PNE:areas_silvestres_protegidas`) | Datos públicos del Estado costarricense |

## Trabajo futuro

- Incorporar otras fuentes de FIRMS: MODIS_NRT, VIIRS (SNPP/NOAA-20/NOAA-21) y
  área quemada (BA_MODIS, BA_VIIRS). La capa de descarga ya está parametrizada
  por `data_id` (ver `FUENTES_FIRMS` en `R/constantes.R`).

## Licencia

El código se distribuye bajo la [licencia MIT](LICENSE). Los datos conservan
las condiciones de sus fuentes originales.
