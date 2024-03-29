% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{valid_sql_limit}
\alias{valid_sql_limit}
\title{Check if a scalar is a valid limit for a SQL query}
\usage{
valid_sql_limit(x)
}
\arguments{
\item{x}{A scalar (numeric or Inf).}
}
\value{
Logical.
}
\description{
Check whether input is a non-negative integer or Inf, which can be
passed on as \verb{LIMIT x} to SQL.
}
\keyword{internal}
