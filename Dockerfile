FROM rocker/geospatial:4.5.3

ENV DEBIAN_FRONTEND=noninteractive

# libavfilter para el paquete av (render MP4); libsecret para credenciales
# RStudio; libglpk40 requerido por igraph (dependencia de targets)
RUN apt-get update && apt-get install -y --no-install-recommends \
      curl \
      libsecret-1-0 \
      libavfilter-dev \
      libglpk40 \
  && rm -rf /var/lib/apt/lists/*

# Se conserva el repositorio binario por defecto de rocker (P3M) para evitar
# compilar desde fuente; solo se ajusta el paralelismo de instalación.
RUN bash -lc "echo \"options(Ncpus = max(1L, parallel::detectCores()-1L))\" \
    >> /usr/local/lib/R/etc/Rprofile.site"

RUN R -q -e "install.packages(c('renv','here','httr2','glue','scales','DT', \
    'targets','tarchetypes','visNetwork','gganimate','gifski','transformr','av', \
    'leaflet','leaflet.extras2','yyjsonr','htmlwidgets','knitr','kableExtra','quarto'))"

# Pre-crear el punto de montaje de la caché de renv con el dueño correcto,
# para que las ejecuciones sin RStudio (docker compose run --user 1000) puedan
# escribir en ~/.cache (renv y quarto).
RUN mkdir -p /home/rstudio/.cache/R/renv && chown -R rstudio:rstudio /home/rstudio/.cache

EXPOSE 8787
