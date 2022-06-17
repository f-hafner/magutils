

#' Load links between MAG and ProQuest
#'
#' @param conn An object of the DBIConnection class to a database.
#' @param from The table with the links to be used.
#' Must be "advisors" or "graduates"
#' @param min_score Minimum score for links to accept. Numeric between 0 and 1.
#' @param ... additional arguments to be passed on to
#'  \code{\link{make_tbl_output}}.
#' If not specified, a lazily evaluated table without limit is returned.
#' Partially specified arguments are completed with \code{\link{dots_tbl_output}}.
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
      pq_id = "goid"
    ),
    advisors = list(
      tbl_name = "current_links_advisors",
      pq_id = "relationship_id"
    )
  )

  stopifnot(is.double(min_score)
            & min_score >= 0
            & min_score <= 1)
  stopifnot(from %in% names(tbl_info))
  from_tbl <- tbl_info[[from]][["tbl_name"]]
  pq_id <- tbl_info[[from]][["pq_id"]]

  if (from == "advisors" & min_score < 0.95) {
    message(strwrap(
      "Note: At the moment, using a link score below 0.95 for advisors
      can result in suspiciously many fals positives. Carefully inspect the
      linked records you use.",
      prefix = " ", initial = "")
    )
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
