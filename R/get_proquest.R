#' Source ProQuest data on advisors or graduates.
#'
#' @param conn An object of the DBIConnection class.
#' @param from A string with options to be queried: "advisors" or "graduates".
#' @param start_year Lowest graduation year to consider. Default: 1985.
#' @param end_year Highest graduation year to consider. Default: 2005.
#' @param ... additional arguments to be passed on to be passed on to
#'  \code{\link{make_tbl_output}}
#'
#' @details
#' For simplicity, does not return the degree year and university information to
#' advisors. You can add this information to advisors by joining the
#' output of `get_proquest(from = "graduates")` to the output of
#' `get_proquest(from = "advisors")`.
#'
#' @examples
#' conn <- connect_to_db(db_example("AcademicGraph.sqlite"))
#' d_graduates <- get_proquest(conn = conn, from = "graduates")
#'
#' @return
#' For graduates, returns a table with degree year, university id,
#' fieldname and gender.
#' For advisors, returns a table with gender and relationship id.
#'
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
get_proquest <- function(conn, from, start_year = 1985, end_year = 2005, ...) {

  stopifnot(is.double(start_year) && is.double(end_year))
  stopifnot(from %in% c("graduates", "advisors"))

  query_keep_us <- "
    SELECT university_id
    FROM pq_unis
    WHERE location LIKE '%United States%'
  "
  us_universities <- dplyr::tbl(conn, dbplyr::sql(query_keep_us))

  graduates <- dplyr::tbl(conn, "pq_authors") %>%
    dplyr::inner_join(us_universities, by = "university_id") %>%
    dplyr::filter(.data$degree_year >= start_year
                  & .data$degree_year <= end_year)


  ## ---------------- prepare graduates -----------------------
  if (from == "graduates") {
    out <- graduates %>%
      dplyr::select(.data$goid,
                    .data$degree_year,
                    .data$university_id,
                    firstname_pq = .data$firstname)

    out <- out %>%
      define_field(conn = conn, from = from)

  }
  ## ---------------- prepare advisors -----------------------
  else if (from == "advisors") {
    qry <- "
    SELECT goid
        , position
        , relationship_id
        -- # Note: need to drop the middle names and initials
        , SUBSTR(TRIM(firstname),
                  1, instr(trim(firstname)||' ',' ') - 1)
          AS firstname_pq
     FROM pq_advisors
    "
    out <- dplyr::tbl(conn, dbplyr::sql(qry)) %>%
      dplyr::inner_join(graduates %>%
                          dplyr::select(.data$goid),
                        by = "goid")
  }

  out <- out %>%
    define_gender(conn = conn,
                  firstname_left = "firstname_pq",
                  drop_missing = FALSE)  %>%
    dplyr::select(-.data$firstname_pq)


  dots <- list(...)
  if (length(dots > 0)) {
    if (! "lazy" %in% names(dots)) {
      dots$lazy = TRUE
    }
    if (! "limit" %in% names(dots)) {
      dots$limit = Inf
    }
    out <- make_tbl_output(out, limit = dots$limit, lazy = dots$lazy)
  }

  return(out)

}


# update dots: add what is default in docs
# re-run queries for generating mock dbs and re-test



