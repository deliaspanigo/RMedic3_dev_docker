# Base image https://hub.docker.com/u/rocker/
# https://hub.docker.com/layers/rocker/shiny/4.4.2/images/sha256-9a3ce1da32e046349297e927fe9fc7e696ff3b1065dcdb1e1f18479d59570424

#FROM rocker/shiny:latest
FROM rocker/shiny:4.4.2

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    cmake \
    nano \
    sudo 
    
## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# copy necessary files
## app folder
COPY /app ./app
## renv.lock file
COPY /app/renv.lock ./renv.lock

# install renv & restore packages
RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::restore()'

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]



