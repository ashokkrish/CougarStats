# Base image https://hub.docker.com/u/rocker/
# Pinned to the R version the app is tested with. ':latest' could move to a new
# R release (and package snapshot) on any rebuild.
FROM rocker/shiny:4.6.1

# system libraries of general use
## install debian packages
RUN apt-get update && \
    apt-get install -y \
            sudo \
            libcurl4-gnutls-dev \
            libcairo2-dev \
            libxt-dev \
            libssl-dev \
            libtcl \
            libtk   \
            libfftw3-dev \
            libnode-dev \
            nodejs \
            libwebp-dev \
            curl

## Install R packages
RUN R -e \
"install.packages(c('aplpack',          \
                    'broom',            \
                    'broom.helpers',    \
                    'bslib',            \
                    'car',              \
                    'caret',            \
                    'class',            \
                    'colourpicker',     \
                    'conflicted',       \
                    'datamods',         \
                    'DescTools',        \
                    'dplyr',            \
                    'DT',               \
                    'e1071',            \
                    'factoextra',       \
                    'forecast',         \
                    'generics',         \
                    'GGally',           \
                    'ggfortify',        \
                    'ggplot2',          \
                    'ggpubr',           \
                    'ggsci',            \
                    'gridExtra',        \
                    'haven',            \
                    'htmltools',        \
                    'iml',              \
                    'katex',            \
                    'knitr',            \
                    'latex2exp',        \
                    'lmtest',           \
                    'magrittr',         \
                    'markdown',         \
                    'MASS',             \
                    'moments',          \
                    'nortest',          \
                    'plotly',           \
                    'psych',            \
                    'randomForest',     \
                    'reactable',        \
                    'readr',            \
                    'readxl',           \
                    'reshape2',         \
                    'remotes',          \
                    'ResourceSelection',\
                    'rpart',            \
                    'rpart.plot',       \
                    'rstatix',          \
                    'shapviz',          \
                    'shiny',            \
                    'shinyalert',       \
                    'shinyjs',          \
                    'shinyMatrix',      \
                    'shinythemes',      \
                    'shinyvalidate',    \
                    'shinyWidgets',     \
                    'skedastic',        \
                    'sortable',         \
                    'stringr',          \
                    'sur',              \
                    'thematic',         \
                    'tibble',           \
                    'tidyr',            \
                    'tinytex',          \
                    'tippy',            \
		    'tools',            \
                    'treeshap',         \
                    'waiter',           \
                    'writexl',          \
                    'xgboost',          \
                    'xml2',             \
                    'xtable'),          \
                  dependencies = TRUE); \
  remotes::install_github('deepanshu88/shinyDarkmode'); \
  remotes::install_github('goodekat/ggResidpanel'); \
  remotes::install_github('rsquaredacademy/olsrr');"

# copy our application into the server, where 'R' is the local path to the app
# Usage: docker build --build-arg APPLICATION_DIRECTORY=<the directory of the
# cougarstats git repo> . "." in the command only indicates that the Dockerfile
# is in the current directory. If you are one directory level above the git
# repository you may write: Usage: docker build --build-arg
# APPLICATION_DIRECTORY=cougarstats -f cougarstats/Dockerfile

ARG APPLICATION_DIRECTORY=.
COPY $APPLICATION_DIRECTORY /srv/shiny-server

## Grant access to server directory
RUN sudo chown -R shiny:shiny /srv/shiny-server

EXPOSE 3838

# Marks the container unhealthy when the app stops answering page requests
# (the page is cached after the first request, so the check is cheap).
HEALTHCHECK --interval=30s --timeout=10s --start-period=120s --retries=3 \
  CMD curl -fsS -o /dev/null http://localhost:3838/ || exit 1

# The app runs as one single-threaded R process: every session shares it, so a
# long computation in one session delays all others, and a crash ends every
# session. Production runs on DigitalOcean App Platform with 1 shared vCPU and
# 2 GB RAM; to test locally under the same limits:
#   docker run -d --restart unless-stopped --cpus 1 --memory 2g -p 3838:3838 <image>
# The app idles at about 370 MB. The largest Random Forest allowed
# (RF_MAX_TRAIN_TREES in R/randomForest.R) peaked at 0.9 GB in a 2 GB test.
# A container that runs out of memory is killed with every session in it, so
# raise that limit only together with a bigger instance.
# Removing the single-process blocking needs more than one replica of this
# container behind a proxy with sticky sessions (websocket support, long read
# timeout), so each user stays on one R process.
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=3838)"]
