

#' Augment a table with additional columns.
#'
#' @param tbl A lazily evaluated table from dbplyr.
#' @param conn An object of the DBIConnection class to a database.
#' @param with_info Which info should `tbl` be augmented with?
#' A column vector with the following options, specified as strings:
#' - affiliation: joins information on unit-year for units in `on_col`
#' - output: joins research output on unit-year for units in `on_col`
#' - coauthor: joins the (academic lifetime) co-authors of units in `on_col`
#' @param on_col On which column should the information be joined? The default
#' is "AuthorId", the unit of authors in MAG. Alternatively, use "CoAuthorId" to
#' join information on co-authors (see below for details).
#' @param ... Additional arguments to be passed on to \code{\link{make_tbl_output}}.
#'
#' @return A new `tbl` with the columns specified `with_info` added.
#'
#' @details
#' There are two main purposes for which this function can be currently
#' used:
#' 1. Join output and/or affiliation information to
#' author units. This works directly with one call to `augment_tbl`.
#' 2. Join information on affiliations of co-authors
#' of author units in `tbl`. To do this, you need to call `augment_tbl` twice:
#' First, to join the co-author information of author units in `tbl`, and then
#' again to join the affiliations of co-authors,
#' using the option `on_col = "CoAuthorId`.
#'
#' Mixing the purposes is discouraged because it creates duplicated records.
#'
#' @export
#'
#' @examples \dontrun{
#' conn <- db_example("AcademicGraph.sqlite")
#' graduates <- get_links(conn, from = "graduates") %>%
#' augment_tbl(conn, with_info = "output")
#' }
#' @importFrom magrittr %>%
augment_tbl <- function(tbl, conn, with_info, on_col = "AuthorId", ...) {

  tbl_classes <- attributes(tbl)$class
  stopifnot("tbl_lazy" %in% tbl_classes
            & "tbl_sql" %in%  tbl_classes)
  stopifnot(on_col %in% c("AuthorId", "CoAuthorId"))
  stopifnot(with_info %in% c("affiliation", "output", "coauthor"))

  if ("coauthor" %in% with_info
      & ("affiliation" %in% with_info
         | "output" %in% with_info)
      ) {
    warning(strwrap(
    "Joining co-authors and a panel-dimension at the author level
    is discouraged because it leads to duplicated records.",
    prefix = " ", initial = "")
    )
  }

  ## ---------------- join affiliation -----------------------
  if ("affiliation" %in% with_info) {
    join_cols <- stats::setNames(nm = on_col, "AuthorId")
    if ("Year" %in% names_tbl_lazy(tbl)) {
      join_cols <- c(
        join_cols,
        stats::setNames(nm = "Year", "Year")
      )
      message("Joining affiliation by unit-time.")
    }

    affiliations <- dplyr::tbl(conn, "AuthorAffiliation")
    tbl <- tbl %>%
      dplyr::left_join(affiliations,
                       by = join_cols
      )
  }
  ## ---------------- join output -----------------------
  if ("output" %in% with_info) {
    join_cols <- stats::setNames(nm = on_col, "AuthorId")
    if ("Year" %in% names_tbl_lazy(tbl)) {
      join_cols <- c(
        join_cols,
        stats::setNames(nm = "Year", "Year")
      )
      message("Joining output by unit-time.")
    }

    output <- dplyr::tbl(conn, "author_output") %>%
      dplyr::select(.data$AuthorId,
                    .data$Year,
                    .data$PaperCount,
                    .data$TotalForwardCitations)
    tbl <- tbl %>%
      dplyr::left_join(output,
                       by = join_cols) # TODO: also year!!
  }
  ## ---------------- join coauthor -----------------------
  if ("coauthor" %in% with_info) {
    join_cols <- stats::setNames(nm = on_col, "AuthorId")
    coauthors <- dplyr::tbl(conn, "author_coauthor")
    tbl <- tbl %>%
      dplyr::left_join(coauthors,
                       by = join_cols)
  }

  dots <- list(...)
  if (length(dots > 0)) {
    if ((! "lazy" %in% names(dots)) | (! "limit" %in% names(dots))) {
      stop("You need to specify both `lazy` and `limit` in ... .")
    }
    tbl <- make_tbl_output(tbl, limit = dots$limit, lazy = dots$lazy)
  }

  return(tbl)
}


