
# Define some variables used in functions below
threshold_prob_female <- 0.8


geemp_fields <- c("geology", "geography", "environmental science",
                  "mathematics", "computer science", "engineering",
                  "chemistry", "physics", "economics")
lps_fields <- c("biology", "psychology", "sociology", "political science")



#' Connect to the database
#'
#' @param db_file full name (including path) to the file.
#'
#' @return A DBI::dbConnect object.
#' @export
connect_to_db <- function(db_file) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  cat("The database connection is: \n")
  print(dbplyr::src_dbi(con))
  return(con)
}


#' Load links between MAG and ProQuest
#'
#' @param conn A DBI connection.
#' @param keep_unique If TRUE (the default), drops graduates that have multiple links to MAG.
#' @param limit LIMIT of the query. An integer or NULL. Default is NULL.
#'
#' @return A lazy query of linked goid-AuthorId.
#' @export
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
get_graduate_links <- function(conn, keep_unique = TRUE, limit = Inf, lazy = TRUE) {

  # TODO: this should go into a separate function and a test: "check_valid_limit" or something
  stopifnot(!is.na(limit) & limit > 0 & !is.logical(limit))
  if (is.finite(limit)) {
    stopifnot(all.equal(limit, as.integer(limit)))
  }

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
    where_stmt <- "WHERE link_score > 0.7"
  }



  query_links <- paste0(
    "SELECT AuthorId, goid, link_score
      FROM current_links ",
    where_stmt)

  if (!is.null(limit)) {
    query_links <- paste0(query_links, " LIMIT ", limit)
  }

  links <- dplyr::tbl(conn, dbplyr::sql(query_links))

  if (!lazy) {
    links <- links %>% dplyr::collect()
  }

  return(links)
}


#' Define gender based on first name.
#'
#' @param conn A DBI connection.
#'
#' @param table A lazily evaluated table sourced from `conn`.
#' @param drop_missing If TRUE, drops records without clear gender assigned.
#' @param firstname_left Column containing the firstname in `table` and to be used for joining gender on.
#'
#' @return `table` augmented by a gender column.
#' @export
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
define_gender <- function(conn, table, firstname_left, drop_missing) {

  names_gender <- dplyr::tbl(conn, "FirstNamesGender") %>%
    dplyr::select(-.data$PersonCount)
  # TODO: check whether Firstname in table?

  table <- table %>%
    dplyr::inner_join(names_gender,
                      by = stats::setNames(nm = firstname_left, "FirstName")) %>%
    dplyr::mutate(gender = dplyr::case_when(
      .data$ProbabilityFemale >= threshold_prob_female ~ "Female",
      .data$ProbabilityFemale <= 1 - threshold_prob_female ~ "Male")
    ) %>%
    dplyr::select(-.data$ProbabilityFemale)

  if (drop_missing) {
    table <- table %>%
      dplyr::filter(!is.na(.data$gender))
  }

  return(table)
}


#' Source PhD graduates
#'
#' @param conn A DBI connection.
#' @param start_year Lowest graduation year to consider. Default: 1985.
#' @param end_year Highets graduation year to consider. Default: 2005.
#'
#' @return A lazily evaluated table with U.S. PhD graduates and their gender.
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
authors_proquest <- function(conn, start_year = 1985, end_year = 2005) {

  query_keep_us <- "
    SELECT university_id
    FROM pq_unis
    WHERE location LIKE '%United States%'
  "
  us_universities <- dplyr::tbl(conn, dbplyr::sql(query_keep_us))

  d <- dplyr::tbl(conn, "pq_authors") %>%
    dplyr::inner_join(us_universities, by = "university_id") %>%
    dplyr::filter(.data$degree_year >= start_year
                  & .data$degree_year <= end_year) %>%
    dplyr::select(.data$goid, .data$degree_year,
                  .data$university_id, firstname_pq = .data$firstname)

  d <- define_gender(conn = conn,
                     table = d,
                     firstname_left = "firstname_pq",
                     drop_missing = TRUE) %>%
    dplyr::select(-.data$firstname_pq)

  return(d)

}




