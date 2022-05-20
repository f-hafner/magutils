
# Define some variables used in functions below
threshold_prob_female <- 0.8

degree_year_start <- 1985
degree_year_end <- 2005


geemp_fields <- c("geology", "geography", "environmental science",
                  "mathematics", "computer science", "engineering",
                  "chemistry", "physics", "economics")
lps_fields <- c("biology", "psychology", "sociology", "political science")



#' Connect to the database
#'
#' @param db_file full name (including path) to the file
#'
#' @return A DBI::dbConnect object
#' @export
#'
#' @examples con <- connect_to_db("mydb.sqlite")
connect_to_db <- function(db_file) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  cat("The database connection is: \n")
  print(dbplyr::src_dbi(con))
  return(con)
}


# TODO: how to have good examples that work to a db that lies elsewhere?
  # add toy database in the /data/? also for tests?

# see the following links:
# https://cran.r-project.org/web/packages/dittodb/vignettes/dittodb.html
# https://stackoverflow.com/questions/56474392/testthat-set-up-database-connection-available-to-all-tests
# https://dittodb.jonkeane.com/
# https://github.com/ropensci/dittodb

#' Load links between MAG and ProQuest
#'
#' @param conn A DBI connection.
#' @param keep_unique If TRUE (the default), drops graduates that have multiple links to MAG.
#'
#' @return A lazy query of linked goid-AuthorId.
#' @export
#'
#' @examples
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
get_linked_graduates <- function(conn, keep_unique = TRUE) {

  if (keep_unique) {
    drop_links <- dplyr::tbl(conn, "current_links") %>%
      dplyr::select(.data$AuthorId, .data$goid) %>%
      dplyr::collect() %>%
      dplyr::group_by(.data$goid) %>%
      dplyr::mutate(n_links = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::filter(.data$n_links > 1) %>%
      dplyr::pull(.data$goid) %>%
      unique()

    where_stmt <- paste0(
      "WHERE goid NOT IN (",
      paste0(drop_links, collapse = ", "),
      " ) AND link_score > 0.7"
    )
  } else {
    where_stmt <- "WHERE links_score > 0.7"
  }

  query_links <- paste0(
    "SELECT AuthorId, goid, link_score
      FROM current_links ",
    where_stmt)

  links <- dplyr::tbl(conn, dbplyr::sql(query_links))

  return(links)
}








