% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_create_dm.R
\name{create_dm}
\alias{create_dm}
\title{Create the data model}
\usage{
create_dm(env, file = tail(list.files("db_data", full.names = TRUE), n = 1))
}
\arguments{
\item{env}{Character. Indicates the environment used to create the data model.
Valid options include \code{"prod"}, \code{"dev"} and \code{"file"}:
\itemize{
\item \code{"prod"} connects to the database in production
\item \code{"dev"} connects to an internal database that is set up using dev containers
\item \code{"file"} reads a dm object snapshot
}}

\item{file}{Character. Path to a \code{.rds} file that represents a dm object snapshot.
Defaults to the last element in \code{db_data} directory.}
}
\value{
List of data frames, based upon the tables in the PostgreSQL database,
with some minor manipulations to reduce the number of data transformations
on-the-fly in the server-side of the app
}
\description{
\code{create_dm()} creates a list of data frames from a determined
environment.
}
\details{
When \code{env} is \code{"prod"} or \code{"dev"}, \code{create_dm()} connects to the database and,
for each table, reads the columns used in the app. \code{gender} and \code{ethnicity}
data frames are derived from \code{client} table.

To create the \code{.rds} object required when \code{env} is \code{"file"}, a person with access
to the database in production should save and share the \code{dm} object that is
generated in \code{app_server.R}.
}
\examples{
\dontrun{
dm <- create_dm(env = "prod")
}
}
