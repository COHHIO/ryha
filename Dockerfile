FROM rocker/r-ver:4.3.1

# System requirements for R packages used in the app
RUN apt-get update && apt-get install --no-install-recommends -y \
    ## {RPostgres} requirements:
    libpq-dev \
    libssl-dev \
    ## {httpuv} requirement:
    libz-dev \
    ## {curl} requirement:
    libcurl4-openssl-dev \
    ## {xlm2} requirement:
    libxml2-dev \
&& rm -rf /var/lib/apt/lists/*

# Pin renv version
## warn=2 upgrades warnings to errors
ENV RENV_VERSION 1.0.3
RUN R -q -e "options(warn=2); install.packages('remotes')"
RUN R -q -e "options(warn=2); remotes::install_version('renv', '${RENV_VERSION}')"

# Install R dependencies
## We do this before copying the app code to ensure the layer is cached
WORKDIR /build
COPY renv.lock /build/renv.lock
RUN R -q -e 'options(warn=2); renv::restore()'

# Copy app files
## Add any additional files unique to the app here
COPY R           /build/package_dir/R
COPY inst        /build/package_dir/inst
COPY data        /build/package_dir/data
COPY NAMESPACE   /build/package_dir/NAMESPACE
COPY DESCRIPTION /build/package_dir/DESCRIPTION

# Install our package
RUN R CMD INSTALL /build/package_dir

# Once installed, remove the temporary directory
RUN rm -rf /build

EXPOSE 3838

CMD R -e "options('shiny.port'=3838,shiny.host='0.0.0.0');library(ryha);ryha::run_app()"
