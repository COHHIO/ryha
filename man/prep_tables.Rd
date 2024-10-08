% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_process_data.R
\name{prep_tables}
\alias{prep_tables}
\title{Prepare data for database tasks}
\usage{
prep_tables(data, conn)
}
\arguments{
\item{data}{List of dataframes returned by \code{process_data()}.}

\item{conn}{A database connection object.}
}
\value{
A list of processed data frames.
}
\description{
\code{prep_tables()} processes the list returned by \code{process_data()} and returns
a new list which is used by \code{delete_from_db()} and \code{send_to_db()}.
}
\details{
\code{prep_tables()} handles the addition of new organization and project entries to
the database, adds \code{project_id} and \code{organization_id} columns to \code{enrollment}
file data, adds \code{organization_id} to the remaining files (e.g. client,
disabilities, education, ...) and returns them as a list of dataframes.

\code{organization} and \code{project} dataframes are excluded from the returned list.
}
