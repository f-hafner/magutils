

#' Load links between MAG and ProQuest
#'
#' Load the links between records in MAG and in ProQuest. The links are
#' stored as a table in the database. The links can be either between
#' PhD graduates and MAG authors, or PhD advisors and MAG authors.
#'
#' @inheritParams doc_common_args
#' @param from A string with options to be queried: "advisors" or "graduates".
#' @param min_score Minimum score for links to accept. Numeric between 0 and 1.
#' @param ... Additional arguments to be passed to
#'  \code{\link{make_tbl_output}}.
#' If not specified, a lazily evaluated table without limit is returned.
#' Partially specified arguments are completed with
#' \code{\link{dots_tbl_output}}.
#'
#' @details For advisor links, a high threshold for `min_score` (0.95 or higher)
#' is recommended, and the function throws a message if that is not the case.
#' The reason for this is that there seem to be many false positives when using
#' a lower threshold.
#'
#' @return A query of linked goid-AuthorId.
#' @export
#'
#' @examples
#' conn <- connect_to_db(db_example("AcademicGraph.sqlite"))
#' graduate_links <- get_links(conn, from = "graduates", min_score = 0.7)
#' advisor_links <- get_links(conn, from = "advisors", min_score = 0.7)
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
get_links <- function(conn, from, min_score = 0.7, ...) {

  tbl_info <- list(
    graduates = list(
      tbl_name = "current_links",
      pq_id = "goid",
      unique_idx = c("goid")
    ),
    advisors = list(
      tbl_name = "current_links_advisors",
      pq_id = "relationship_id",
      unique_idx = c("relationship_id")
    )
  )

  right_score <- (is.double(min_score)
                  & min_score >= 0
                  & min_score <= 1)
  right_info <- from %in% names(tbl_info)
  if (!right_score | ! right_info) {
    stop("Invalid arguments.")
  }

  from_tbl <- tbl_info[[from]][["tbl_name"]]
  pq_id <- tbl_info[[from]][["pq_id"]]
  unique_idx_cols <- tbl_info[[from]][["unique_idx"]]

  if (from == "advisors" & min_score < 0.95) {
    message(strwrap(
      "Note: At the moment, using a link score below 0.95 for advisors
      can result in suspiciously many false positives. Carefully inspect the
      linked records you use.",
      prefix = " ", initial = "")
    )
  }

  has_unique_index <- has_idx(conn = conn, on_tbl = from_tbl,
                              on_cols = unique_idx_cols,
                              keep_unique = T)
  if (!has_unique_index) {
    stop(strwrap(
      paste0("Expected table ", from_tbl, " to have unique index on (",
             paste(unique_idx_cols, collapse = ","),
             "), but this is not true. This check is to ensure that entites ",
             "are not linked more than once."),
      prefix = " ", initial = ""
    ))
  }

  query_links <- paste0("
    SELECT AuthorId, ", pq_id, ", link_score
    FROM ?link_table
    WHERE link_score >= ?minimum_score
  ")

  query_links <- DBI::sqlInterpolate(
    conn,
    query_links,
    link_table = from_tbl,
    minimum_score = min_score
  )


  links <- dplyr::tbl(conn,
                      dbplyr::sql(query_links))

  dots <- dots_tbl_output(...)
  if (!is.null(dots)) {
    links <- make_tbl_output(links, limit = dots$limit, lazy = dots$lazy)
  }

  return(links)
}
