
<!-- README.md is generated from README.Rmd. Please edit that file -->

# R Youth Homelessness Analyzer *(RYHA)*

<!-- badges: start -->
<!-- badges: end -->

The goal of `{ryha}` is to provide a web-based application for in-depth,
user-friendly provision & consumption of Youth Homelessness data across
the State of Ohio.

## Installation

This Shiny application is built using the [{golem}
framework](https://thinkr-open.github.io/golem/), which means that this
app is actually an *R package*.

You can install `{ryha}` with:

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
    dev containers. Check [Development
    Environment](#development-environment) section to learn how to set
    up this development environment.
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

## Data

### Source

The data within this application are collected by each grantee in a
format compliant with the *HMIS Data Standards*. This data originates
from one of a few separate HMIS databases in the State. The HMIS
databases have the capability to query the database and export a .zip
file. This .zip file can be uploaded into the **COHHIO Youth Data
Dashboard** app via the app’s “*Upload*” page.

### Requirements

The following requirements must be satisfied in order for the .zip file
to be successfully processed and its data written to the database:

- The file must be *.zip* extension
- The .zip file must contain (at least) the following .csv files
  - **Client.csv**
  - **Disabilities.csv**
  - **EmploymentEducation.csv**
  - **CurrentLivingSituation.csv**
  - **HealthAndDV.csv**
  - **IncomeBenefits.csv**
  - **Enrollment.csv**
  - **Services.csv**
  - **Project.csv**
  - **Organization.csv**
  - **Exit.csv**
  - **Export.csv**
- The **Organization.csv** file must contain exactly one (1)
  organization
- The column naming conventions must match the expectations of the
  *ryha* database

### Data Dictionary

The data dictionary for each element across each file can be found
[here](https://files.hudexchange.info/resources/documents/HMIS-Data-Dictionary-2024.pdf).
This data dictionary was used to develop the package datasets, which are
created in [data-raw](data-raw) and stored in [data](data).

## Architecture

The following architecture diagram shows how the app can be used for
both uploading (.zip file) HMIS data, and visualizing previously
uploaded HMIS data.

![](man/figures/README/architecture-diagram.png)
