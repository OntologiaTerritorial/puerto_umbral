# ==============================================================================
# Dockerfile: Puerto Umbral en Shiny Server
# Plataforma de Ontologia Territorial y Geotensores (Tomo II)
# Autor: John Treimun Rios (Centro de Inteligencia Territorial, UAI)
# ==============================================================================

FROM rocker/shiny:latest

LABEL maintainer="John Treimun Rios (john.treimun.r@uai.cl)"
LABEL description="Servidor Shiny institucional de Puerto Umbral para modelamiento geotensorial y trabajo de campo"

# 1. Instalar dependencias geoespaciales y del sistema operativo Linux
RUN apt-get update && apt-get install -y --no-install-recommends \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libsqlite3-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    python3 \
    python3-pip \
    && rm -rf /var/lib/apt/lists/*

# 2. Instalar paquetes R del ecosistema Shiny y Geoespacial
RUN R -e "install.packages(c( \
    'shiny', \
    'shinyjs', \
    'leaflet', \
    'plotly', \
    'deSolve', \
    'RSQLite', \
    'DBI', \
    'ggplot2', \
    'htmltools', \
    'shinythemes', \
    'jsonlite', \
    'dplyr', \
    'sf', \
    'RColorBrewer', \
    'scales', \
    'fields', \
    'duckdb', \
    'rsconnect' \
), repos='https://cloud.r-project.org/')"

# 3. Copiar la configuracion personalizada de Shiny Server
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# 4. Copiar la aplicacion Puerto Umbral al directorio de servicio
RUN rm -rf /srv/shiny-server/*
COPY app /srv/shiny-server/app

# 5. Configurar permisos para el usuario 'shiny'
RUN chown -R shiny:shiny /srv/shiny-server/app \
    && chmod -R 755 /srv/shiny-server/app

# 6. Variables de entorno
ENV PUERTO_DEBUG="FALSE"
ENV SHINY_LOG_LEVEL="INFO"

# Exponer el puerto oficial de Shiny Server
EXPOSE 3838

# Comando por defecto para arrancar Shiny Server
CMD ["/usr/bin/shiny-server"]
