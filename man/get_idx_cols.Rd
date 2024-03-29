% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_checking.R
\name{get_idx_cols}
\alias{get_idx_cols}
\title{Extract constituting columns from a "create index" statement}
\usage{
get_idx_cols(stmt)
}
\arguments{
\item{stmt}{A SQLite statement to create an index on a table on some columns.
The function extracts the columns on which the index is created. As the
example illustrates, "ASC" and "DESC" statements are removed.}
}
\value{
A character vector with the column names defining the index.
}
\description{
This function takes a string from a SQL statement that creates an index
on a table, and returns the columns that constitute the index.
}
\examples{
magutils:::get_idx_cols("CREATE INDEX idx1 ON mytable (col1 ASC, col2 ASC)")
# gives  c("col1", "col2")
}
\keyword{internal}
