
<!-- README.md is generated from README.Rmd. Please edit that file -->

# R Youth Homelessness Analyzer *(RYHA)*

<!-- badges: start -->
<!-- badges: end -->

The goal of **ryha** is to provide a web-based application for in-depth,
user-friendly provision & consumption of Youth Homelessness data across
the State of Ohio.

## Installation

This Shiny application is built using the [{golem}
framework](https://thinkr-open.github.io/golem/), which means that this
app is actually an *R package*.

You can install **ryha** with:

``` r
# install.packages("devtools")
devtools::install_github("COHHIO/ryha")
```

## Running the App

Before launching the app:

- You must determine the environment that will be used to create the
  data model. This is accomplished by setting `APP_BACKEND`
  environmental variable. Valid options are:

  - `prod`, which connects to the database in production
  - `dev`, which connects to an internal database that is set up using
    dev containers. Check `vignette("dev-environment")` to learn how to
    set up this development environment.
  - `file`, which reads a provided `.rds` data model object and does not
    require any database connection.

- If you are connecting to a database (i.e. `APP_BACKEND=prod` or
  `APP_BACKEND=dev`) you must have the following additional
  configuration to be able to upload data to the app and write to the
  database:

  - Environment Variable (for *uploading* data to the app):

    - `UPLOAD_PWD`: sets the password required to upload data to the
      database.

  - Hash keys developed for encrypting the data during the writing
    process (stored at the root of the directory, needed for *writing
    to* the database)

- If you are connecting to the database in production
  (i.e. `APP_BACKEND=prod`) you must have the following additional
  configurations set to interact with the database:

  - Environment Variables:

    - `DB_NAME`: database name. It specifies the name of the specific
      database within the PostgreSQL server to which you want to
      connect.
    - `DB_HOST`: host address of the PostgreSQL server. In our case, it
      will be a particular domain name (e.g. db.example.com).
    - `DB_PORT`: port number on which the PostgreSQL server is listening
      for connections.
    - `DB_USER`: username used to authenticate with the PostgreSQL
      server. The user must have the necessary permissions to connect to
      the specified database.
    - `DB_PWD`: password associated with the specified user account. It
      is used for authentication purposes to ensure that the connection
      is secure.

Environmental variables should be stored in a `.Renviron` file located
at the root of the directory.

After installing the package, you can launch the app using the package’s
`run_app()` function.

``` r
ryha::run_app()
```
