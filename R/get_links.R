

#' Load links between MAG and ProQuest
#'
#' @param conn An object of the DBIConnection class.
#' @param from The table with the links to be used.
#' Must be "advisors" or "graduates"
#' @param min_score Minimum score for links to accept. Numeric between 0 and 1.
#' @param limit LIMIT of the query. A positive integer or Inf.
#' Default is Inf, in which case all records are returned.
#' @param lazy If TRUE (the default), does not `collect()` the query into a dataframe.
#' This is useful if other tables from the database are joined later on.
#'
#' @return A query of linked goid-AuthorId.
#' @export
#'
#' @examples
#' conn <- connect_to_db(db_example("AcademicGraph.sqlite"))
#' links <- get_links(conn, from = "graduates", min_score = 0.7)
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
get_links <- function(conn, from, min_score = 0.7,
                      limit = Inf, lazy = TRUE) {

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
  stopifnot(valid_sql_limit(limit))
  stopifnot(is.double(min_score)
            & min_score >= 0
            & min_score <= 1)
  stopifnot(from %in% names(tbl_info))
  from_tbl <- tbl_info[[from]][["tbl_name"]]
  pq_id <- tbl_info[[from]][["pq_id"]]

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
                      dbplyr::sql(query_links)) %>%
    make_tbl_output(limit = limit,
                    lazy = lazy)

  return(links)
}
