#' Calculate connected items of various degrees
#'
#' @param tbl Input table to be processed
#' @param conn Connection to database.
#' @param ref_year Reference year. A string of one of the columns in `tbl`.
#' @param degrees An integer vector indicating the degrees. Up to 2 are supported.
#' @param time_window A two-element numeric column vector. Not: the same will be applied to all author and co-author connections!!!
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
  degrees = c(1),
  time_windows = list(
    collab = c(-10, 0), # means that collaborations from ref_year - 10 until degree_year are included
    entity = c(-5, 0) # means that the recorded entity (paper, affiliation) needs to be observed from ref_year - 5 ot ref_year
  ),
  output_entity = "AffiliationId" ) {

  cat("Checking inputs... \n")
  names_tbl_in <- names_tbl_lazy(tbl = tbl)
  stopifnot(ref_year %in% names_tbl_in)

  cat("Making tables on database...\n")
  # extract connections
  current_degree <- 1
  tbl_in <- tbl
  ls_out <- list()
  while (current_degree <= max(degrees)) {
    ref_col <- "AuthorId" # reference column to get the connection from
    if (current_degree > 1) {
      ref_col <- paste0("CoAuthorId_degree", current_degree - 1)
    }
    coauthor_id_col <- paste0("CoAuthorId_degree", current_degree) # name of the column of the co-author that will be outputted
    d_collab <- add_info(
      tbl = tbl_in,
      from = "collaborations",
      conn = conn,
      ref_col = ref_col,
      ref_year = ref_year,
      diff_min = min(time_windows$collab),
      diff_max = max(time_windows$collab),
      coauthor_id_col = coauthor_id_col
    )

    ls_out[[paste0("degree", current_degree)]] <- d_collab
    tbl_in <- d_collab
    current_degree <- current_degree + 1
  }

  # add affiliation
  names_out <- names(ls_out)
  ls_out <- purrr::map(
    .x = names_out,
    .f = ~add_info(
      tbl = ls_out[[.x]],
      from = "affiliations",
      conn = conn,
      ref_col = paste0("CoAuthorId_", .x),
      ref_year = ref_year,
      diff_min = min(time_windows$entity),
      diff_max = max(time_windows$entity)
    ) %>%
      dplyr::select(.data$AuthorId, .data$AffiliationId)
  )
  names(ls_out) <- names_out

  cat("Collecting output... \n")
  # Here we return the unique id_col-affiliation ids
    # an alternative would be to also return the year
      # and author id of the most recent connection to this affiliation
  # but this would become complicated when looking at >1 degrees
    # (multiple ids, multiple years would have to be returned)

  required_degrees <- paste0("degree", degrees) # if degree = 2, do not load degree = 1
  ls_out <- purrr::map(
    .x = required_degrees,
    .f = ~ls_out[[.x]] %>%
      dplyr::collect() %>%
      dplyr::filter(
        !duplicated(paste0(.data$AuthorId, .data$AffiliationId))
      )
  )
  return(ls_out)
}


add_info <- function(tbl, conn, ref_col, ref_year, diff_min, diff_max,
                     from,
                     coauthor_id_col = "CoAuthorId") {
  stopifnot(from %in% c("collaborations", "affiliations"))

  tbl_other <- list(
    collaborations =
      dplyr::tbl(
        src = conn,
        "author_collab"
        ) %>%
      dplyr::rename(!! {{ coauthor_id_col }} := CoAuthorId),
    affiliations =
      dplyr::tbl(
        src = conn,
        "AuthorAffiliation"
        ) %>%
      dplyr::inner_join(
        dplyr::tbl(src = conn, "Affiliations") %>%
          dplyr::filter(.data$Iso3166Code == "US") %>%
          dplyr::select(.data$AffiliationId),
        by = "AffiliationId"
      )
  )

  join_cols <- stats::setNames(nm = ref_col, "AuthorId")

  out <- tbl %>%
    dplyr::left_join(tbl_other[[from]],
                     by = join_cols) %>%
    # keep only "recent" collaborations
    dplyr::filter(
      .data$Year - .data[[ref_year]] >= diff_min
      & .data$Year - .data[[ref_year]] <= diff_max) %>%
    dplyr::select(-.data$Year)
  return(out)
}
#
# add_affil <- function(tbl, conn, ref_col, ref_year, diff_min, diff_max) {
#   #author-coauthor-year pairs
#   # only US
#   author_affiliation <- dplyr::tbl(
#     src = conn,
#     "AuthorAffiliation"
#   ) %>%
#     dplyr::inner_join(
#       dplyr::tbl(src = conn, "Affiliations") %>%
#         dplyr::filter(.data$Iso3166Code == "US") %>%
#         dplyr::select(.data$AffiliationId),
#       by = "AffiliationId"
#     )
#   join_cols <- stats::setNames(nm = ref_col, "AuthorId")
#
#   tbl <- tbl %>%
#     dplyr::left_join(
#       author_affiliation,
#       by = join_cols
#     ) %>%
#     dplyr::filter(
#       .data$Year - .data[[ref_year]] >= diff_min
#       & .data$Year - .data[[ref_year]] <= diff_max
#     ) %>%
#     dplyr::select(-.data$Year)
#
#   return(tbl)
# }


# input: authorid, graduation_year

db_file <- "/mnt/ssd/AcademicGraph/AcademicGraph.sqlite"
con <- connect_to_db(db_file)
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


dk <- add_collaboration_connections(
  tbl = advisors_in, conn = con, ref_year = "advisee_degree_year", degrees = c(1)
)

# next steps
# how to add advisor? -> use author, but for own authorid?
# test: no duplicates, correct values, right time frame ...?
# can we make it faster? --> parallel! but who? loop over authorid-degree year pairs; but for this we need
  # to collect, and I am not sure we can then do it so easily?

# TODO: add gender of co-author? how for 2nd degree when multiple connections point to the same institution? use the most recent connection?
  # or return all links, and then collapse at the authorid-institution level?
# make a parallel implementation: loop over the
# TODO: what I forgot: we need also the graduation year!
  # ie, if an advisor has a student in 1995 and 2000, we need all her links in 1995 and 2000
  # but if an advisor has two students in the same year, we only need the data once









