FROM rocker/r-ver:4.3.1

# System library dependency for the app
RUN apt-get update && apt-get install --no-install-recommends -y \
    # RPostgres package requirements
    libpq-dev \
    libssl-dev \
    # httpuv package requirement
    libz-dev \
    # curl package requirement
    libcurl4-openssl-dev \
    # xlm2 package requirement
    libxml2-dev \
    # httpgd package requirement (VS Code development package)
    libfontconfig1-dev \
&& rm -rf /var/lib/apt/lists/*

# Pin renv version
ENV RENV_VERSION 1.0.3
RUN R -q -e "options(warn=2); install.packages('remotes')"
RUN R -q -e "options(warn=2); remotes::install_version('renv', '${RENV_VERSION}')"

# Install R dependencies
# Do this before copying the app-code, to ensure this layer is cached
WORKDIR /build
COPY renv.lock /build/renv.lock
RUN R -q -e 'options(warn=2); renv::restore()'

ADD . /build

RUN R -e 'install.packages("languageserver")'
RUN R -e 'install.packages("httpgd")'
