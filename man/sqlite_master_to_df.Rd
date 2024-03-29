% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_checking.R
\name{sqlite_master_to_df}
\alias{sqlite_master_to_df}
\title{Transform sqlite_master table to a dataframe.}
\usage{
sqlite_master_to_df(conn, temp = FALSE)
}
\arguments{
\item{conn}{An object of class \code{SQLiteConnection} to a sqlite database.}

\item{temp}{Should \code{sqlite_temp_master} be queried, instead of
\code{sqlite_master}? Default is FALSE. This can be useful when looking for
temporary tables and indexes on them.}
}
\value{
A dataframe with type, name, tbl_name and sql statement
from sqlite_master.
}
\description{
This is a wrapper to query sqlite_master from the sqlite database.
}
\examples{
conn <- connect_to_db(db_example("AcademicGraph.sqlite"))
dplyr::glimpse(sqlite_master_to_df(conn))
}
