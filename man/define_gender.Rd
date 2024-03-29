% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{define_gender}
\alias{define_gender}
\title{Define gender based on first name.}
\usage{
define_gender(tbl, conn, firstname_left, drop_missing)
}
\arguments{
\item{tbl}{A query from \code{conn} with dbplyr and lazily evaluated.}

\item{conn}{An object of class \code{SQLiteConnection} to a sqlite database.}

\item{firstname_left}{Column containing the firstname in \code{table} and to
be used for joining gender on.}

\item{drop_missing}{If TRUE, drops records without clear gender assigned.
Clear assignment is when probability of either gender is 0.8 or higher.}
}
\value{
\code{tbl} augmented by a gender column.
}
\description{
Given a database table \code{tbl}, assigns the likely gender of the person
given the firstname. The firstname needs to be present as a column in \code{tbl} and passed
as argument \code{firstname_left}.
}
\details{
The function uses the internal table \code{FirstNamesGender}, which
assigns the likely gender to each first name. The table is generated from
\href{https://genderize.io/}{genderize.io}.

\code{firstname_left} should be free of middle names and middle
initials, as otherwise the gender assignment fails (even though using only
the firstname would result in a high-confidence assignment.)
}
\examples{
\dontrun{
new_table <- define_gender(
conn = conn, table = old_table,
firstname_left = "firstname_old", drop_missing = TRUE
)
}

}
