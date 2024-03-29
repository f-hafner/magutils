% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{doc_common_args}
\alias{doc_common_args}
\title{Collect documentation of commonly used function input}
\usage{
doc_common_args(conn, tbl)
}
\arguments{
\item{conn}{An object of class \code{SQLiteConnection} to a sqlite database.}

\item{tbl}{A query from \code{conn} with dbplyr and lazily evaluated.}
}
\description{
Store the documentation for arguments commonly used in the package.
The function itself does nothing. It is just for easy reference and
maintenance. In the future more arguments may be added in a similar manner.
}
\keyword{internal}
