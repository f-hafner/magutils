

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






