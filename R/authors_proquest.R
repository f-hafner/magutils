#' Source PhD graduates
#'
#' @param conn An object of the DBIConnection class.
#' @param start_year Lowest graduation year to consider. Default: 1985.
#' @param end_year Highest graduation year to consider. Default: 2005.
#' @param lazy If TRUE (the default), does not `collect()` the query into a dataframe.
#' This is useful if other tables from the database are joined later on.
#' @param limit  LIMIT of the query. A positive integer or Inf.
#' Default is Inf, in which case all records are returned.
#'
#' @examples
#' conn <- connect_to_db(db_example("AcademicGraph.sqlite"))
#' d_graduates <- authors_proquest(conn = conn)
#'
#' @return A table with U.S. PhD graduates and their gender.
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
authors_proquest <- function(conn, start_year = 1985, end_year = 2005,
                             lazy = TRUE, limit = Inf) {

  stopifnot(valid_sql_limit(limit))
  stopifnot(is.double(start_year) && is.double(end_year))

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

  d <- d %>%
    dplyr::left_join(graduate_fields(conn),
                     by = "goid") %>%
    make_tbl_output(limit = limit,
                    lazy = lazy)

  return(d)

}
