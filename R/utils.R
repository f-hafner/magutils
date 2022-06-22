# This files collects various functions. Many of them are internal.

# Define some parameters used in functions below
threshold_prob_female <- 0.8

#' Connect to the database
#'
#' A wrapper around \code{\link[DBI]{dbConnect}} for Sqlite.
#' Create an object of class `SQLiteConnection` to a sqlite database stored in
#' `db_file`. Prints the output from \code{\link[dbplyr]{src_dbi}} and returns
#' the connection object.
#'
#' @param db_file full name (including path) of the file.
#'
#' @return An object of class `SQLiteConnection` to a sqlite database.
#' @export
#'
#' @examples conn <- connect_to_db(db_example("AcademicGraph.sqlite"))
connect_to_db <- function(db_file) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  cat("The database connection is: \n")
  print(dbplyr::src_dbi(con))
  return(con)
}


#' Define gender based on first name.
#'
#' Given a database table `tbl`, assigns the likely gender of the person
#' given the firstname. The firstname needs to be present as a column in `tbl` and passed
#' as argument `firstname_left`.
#'
#' @inheritParams doc_common_args
#' @param drop_missing If TRUE, drops records without clear gender assigned.
#' Clear assignment is when probability of either gender is 0.8 or higher.
#' @param firstname_left Column containing the firstname in `table` and to
#' be used for joining gender on.
#'
#' @return `tbl` augmented by a gender column.
#'
#' @details The function uses the internal table `FirstNamesGender`, which
#' assigns the likely gender to each first name. The table is generated from
#' \href{https://genderize.io/}{genderize.io}.
#'
#' `firstname_left` should be free of middle names and middle
#' initials, as otherwise the gender assignment fails (even though using only
#' the firstname would result in a high-confidence assignment.)
#'
#' @export
#'
#' @examples \dontrun{
#' new_table <- define_gender(
#' conn = conn, table = old_table,
#' firstname_left = "firstname_old", drop_missing = TRUE
#' )
#' }
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
define_gender <- function(tbl, conn, firstname_left, drop_missing) {

  names_gender <- dplyr::tbl(conn, "FirstNamesGender") %>%
    dplyr::select(-.data$PersonCount)

  tbl <- tbl %>%
    dplyr::left_join(names_gender,
                     by = stats::setNames(nm = firstname_left, "FirstName")) %>%
    dplyr::mutate(gender = dplyr::case_when(
      .data$ProbabilityFemale >= threshold_prob_female ~ "Female",
      .data$ProbabilityFemale <= 1 - threshold_prob_female ~ "Male")
    ) %>%
    dplyr::select(-.data$ProbabilityFemale)

  if (drop_missing) {
    tbl <- tbl %>%
      dplyr::filter(!is.na(.data$gender))
  }

  return(tbl)
}


#' Check if a scalar is a valid limit for a SQL query
#'
#' Check whether input is a non-negative integer or Inf, which can be
#' passed on as `LIMIT x` to SQL.
#'
#' @param x A scalar (numeric or Inf).
#'
#' @return Logical.
#'
#' @keywords internal
valid_sql_limit <- function(x) {
  if (length(x) > 1 | is.null(x)) {
    return(FALSE)
  } else if (is.na(x) | is.logical(x) | is.character(x)) {
    return(FALSE)
  } else if (is.finite(x)) {
    return(all.equal(x, as.integer(x))
           & x > 0)
  } else if (is.infinite(x)) {
    return(x > 0)
  }
}



#' Make table output from a SQL query
#'
#' This function is used at the end of user-facing functions to query the
#' database. It limits the query to some number of rows and collects if
#' if specified.
#'
#' @inheritParams doc_common_args
#' @param limit  LIMIT of the query. A positive integer or Inf.
#' Default is Inf, in which case all records are returned.
#' @param lazy If TRUE, does not `collect()` the query into a dataframe.
#' This is useful if other tables from the database are joined later on.
#'
#' @return A query, evaluated with `limit` and `lazy`.
#' @keywords internal
make_tbl_output <- function(tbl, limit, lazy) {

  stopifnot(valid_sql_limit(limit))

  if (limit < Inf) {
    tbl <- utils::head(tbl, limit)
  }

  if (!lazy) {
    tbl <- tbl %>% dplyr::collect()
  }

  return(tbl)
}


#' Handle dots for making table output
#'
#' This is a helper function to transform ... from user-facing functions
#' into arguments for \code{\link{make_tbl_output}}.
#'
#' @param ... Ellipsis, passed on from a higher function
#'
#' @return If `...` is empty, returns NULL. Otherwise it converts
#'  `...` into a named list with elements "lazy" and "limit".
#'
#' @details For non-empty but partially specified arguments, it adds
#' the following defaults: `lazy = TRUE`, `limit = Inf`.
#' For instance, if only limit = 3 is passed, \code{\link{make_tbl_output}}
#' will return a lazily evaluated query.
#' To safeguard against accidentally loading large queries into memory
#' by only specifying `lazy = FALSE`, an error is thrown in this case.
#' @keywords internal
dots_tbl_output <- function(...) {
  dots <- list(...)
  if (length(dots > 0)) {
    if (! "lazy" %in% names(dots)) {
      dots$lazy = TRUE
    }
    if (! "limit" %in% names(dots)) {
      if (!dots$lazy) {
        stop(strwrap(
          "You specified `lazy = FALSE` but did not give a `limit`. If you want
          to load the query into memory, run the function without specifying
          neither of `limit` and `lazy` and explicitly `collect` afterwards.",
          prefix = " ", initial = ""
        ))
      }
      dots$limit = Inf
    }
  }
  else {
    dots <- NULL
  }
  return(dots)
}



#' Extract the names of a lazily evaluated table
#'
#' This is a helper function to extract the column names from a table.
#'
#' @inheritParams doc_common_args
#'
#' @return A vector with the names of the dataframe if `tbl` was `collect`ed.
#' @importFrom magrittr %>%
#' @keywords internal
names_tbl_lazy <- function(tbl) {
  out <- make_tbl_output(tbl, limit = 1, lazy = FALSE) %>%
    dplyr::collect() %>%
    names()
  return(out)
}



#' Collect documentation of commonly used function input
#'
#' Store the documentation for arguments commonly used in the package.
#' The function itself does nothing. It is just for easy reference and
#' maintenance. In the future more arguments may be added in a similar manner.
#'
#' @param conn An object of class `SQLiteConnection` to a sqlite database.
#' @param tbl A query from `conn` with dbplyr and lazily evaluated.
#'
#' @return NULL
#' @keywords internal
#'
doc_common_args <- function(conn, tbl) {
  NULL
}











