
# Define some variables used in functions below
threshold_prob_female <- 0.8


geemp_fields <- c("geology", "geography", "environmental science",
                  "mathematics", "computer science", "engineering",
                  "chemistry", "physics", "economics")
lps_fields <- c("biology", "psychology", "sociology", "political science")



#' Connect to the database
#'
#' @param db_file full name (including path) of the file.
#'
#' @return Returns an object of the DBIConnection class and prints the db info
#' of the connection with `dbplyr::src_dbi()`.
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
#' @param tbl A lazily evaluated table sourced from `conn`.
#' @inheritParams doc_sqlite_connection
#' @param drop_missing If TRUE, drops records without clear gender assigned.
#' Clear assignment is when probability of either gender is 0.8 or higher.
#' @param firstname_left Column containing the firstname in `table` and to
#' be used for joining gender on.
#'
#' @return `table` augmented by a gender column.
#'
#' @details Note that  `firstname_left` should be free of middle names and middle
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


#' Check whether input is a non-negative integer or Inf.
#'
#' @param x A scalar (numeric or Inf).
#'
#' @return Logical.
#' @export
#'
#' @examples valid_sql_limit(3)
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



#' Make final table output from a sql query
#'
#' @param tbl A lazily evaluated query from sql with dbplyr.
#' @param limit  LIMIT of the query. A positive integer or Inf.
#' Default is Inf, in which case all records are returned.
#' @param lazy If TRUE, does not `collect()` the query into a dataframe.
#' This is useful if other tables from the database are joined later on.
#'
#' @return A query, evaluated with `limit` and `lazy`.
#' @export
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


#' Helper function to handle dots for making table output
#'
#' @param ... Ellipsis, passed on from a higher function
#'
#' @return A named list built from `...`, with defaults for `make_tbl_output`
#' added:
#' `lazy` is by default `TRUE`, `limit` is by default `Inf`. Thus, if you only
#' pass on limit = 3, you get a lazily evaluated query.
#' To safeguard against accidentally loading large queries into memory
#' by only specifying `lazy = FALSE`, an error is thrown in this case.
#' @export
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
#' @param tbl A lazily evaluated table from dplyr.
#'
#' @return A vector with the names of the dataframe if `tbl` was `collect`ed.
#' @export
#' @importFrom magrittr %>%
names_tbl_lazy <- function(tbl) {
  out <- make_tbl_output(tbl, limit = 1, lazy = FALSE) %>%
    dplyr::collect() %>%
    names()
  return(out)
}














