# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest

# system libraries of general use
## install debian packages
RUN apt-get update && apt-get install -y \
    sudo \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libtcl \ 
    libtk
    
## Install R packages
RUN R -e "install.packages(c('aplpack', 'base', 'bslib', 'car', 'colourpicker', 'DescTools', 'dplyr', 'DT', 'generics', 'ggplot2', 'ggpubr', 'ggsci', 'e1071', 'markdown', 'nortest', 'plotly', 'readr', 'readxl', 'remotes', 'rstatix', 'shiny', 'shinythemes', 'shinyjs', 'shinyMatrix', 'shinyvalidate', 'shinyWidgets', 'tinytex', 'tools', 'writexl', 'xtable', 'MASS'), dependencies = T)" 
RUN R -e "remotes::install_github("deepanshu88/shinyDarkmode")"

# copy our application into the server, where 'R' is the local path to the app
COPY CougarStats /srv/shiny-server

## Grant access to server directory
RUN sudo chown -R shiny:shiny /srv/shiny-server

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=3838)"]