


#' Calculate connected items of various degrees
#'
#' @param tbl Input table to be processed
#' @param conn Connection to database.
#' @param ref_year Reference year. A string of one of the columns in `tbl`.
#' @param id_col Person identifier. A string with one of the columns in `tbl`.
#' @param via A column vector of strings defining the path to calculate the connections.
#' @param time_window A two-element numeric column vector.
#' @param output_entity Entity to be returned: affiliations or papers
#'
#' @return
#' @export
#'
#' @examples
add_collaboration_connections <- function(
  tbl,
  conn,
  ref_year,
  id_col = "AuthorId",
  via = c("coauthor"),
  time_windows = list(
    collab = c(-10, 0), # means that collaborations from ref_year - 10 until degree_year are included
    entity = c(-5, 0) # means that the recorded entity (paper, affiliation) needs to be observed from ref_year - 5 ot ref_year
  ),
  output_entity = "AffiliationId" ) {

  names_tbl_in <- names_tbl_lazy(tbl = tbl)
  stopifnot(ref_year %in% names_tbl_in
            & id_col %in% names_tbl_in)

  collab <- dplyr::tbl(
    src = conn,
    "author_collab"
    ) #author-coauthor-year pairs
  author_affiliaton <- dplyr::tbl(
    src = conn,
    "AuthorAffiliation"
    )

  # first round of co-authors
  d_out <- tbl %>%
    dplyr::left_join(collab,
                     by = "AuthorId") %>%
    # keep only "recent" collaborations
    filter(.data$Year - .data[[ref_year]] >= min(time_windows$collab) # TODO: write this as helper function?
           & .data$Year - .data[[ref_year]] <= max(time_windows$collab)) %>%
    select(-Year)

  # for multiple rounds of authors, add this here
    # adjust the name of the CoAuthorId column below

  # join entity
  d_out <- d_out %>%
    dplyr::left_join(
      author_affiliaton,
      by = c("CoAuthorId" = "AuthorId") # this needs to be done differently (?)
    ) %>%
    dplyr::filter(.data$Year - .data[[ref_year]] >= min(time_windows$entity)
           & .data$Year - .data[[ref_year]] <= max(time_windows$entity)) %>%
    dplyr::select(-Year)

  return(d_out)

}

# input: authorid, graduation_year

advisors <- get_links(conn = con, from = "advisors")
pq_advisors <- get_proquest(conn = con, from = "advisors", start_year = 1985, end_year = 2022)
pq_graduates <- get_proquest(conn = con, from = "graduates", start_year = 1985, end_year = 2022)

advisors_in <- advisors %>%
  dplyr::left_join(pq_advisors %>%
              dplyr::select(goid, position, relationship_id),
            by = "relationship_id") %>%
  dplyr::left_join(pq_graduates %>%
                     dplyr::select(goid, degree_year),
                   by = "goid"
                   ) %>%
  dplyr::select(AuthorId, advisee_degree_year = degree_year)


collab <- dplyr::tbl(src = conn, "author_collab")

dk <- advisors_in %>%
  dplyr::left_join(collab,
                   by = "AuthorId")



