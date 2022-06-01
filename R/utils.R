

#' Check whether input is a non-negative integer or Inf.
#'
#' @param x An atomic vector.
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
    return(all.equal(x, as.integer(x)) & x > 0
           )
  } else if (is.infinite(x)) {
    return(x > 0)
  }
}

