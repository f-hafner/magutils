#' Define the field of study
#'
#' This function defines the field of study at level 0 in MAG of records
#' in `tbl`. To do so, it queries tables in the database that store the
#' field of study of the person. See the details for more information.
#'
#' @inheritParams doc_common_args
#' @inheritParams get_links
#' @param from A string with options to be queried: "mag_authors" or "graduates".
#'
#' @return  `tbl` augmented by a column with the name of the field of study at
#' level 0 as defined by MAG.
#'
#' @details
#'
#' ## When using `from` = "graduates"
#' The field is defined with a custom mapping between the reported field of
#' study in ProQuest and the fields in MAG. The mapping is stored in the
#' database.
#' A record can have multiple fields, and currently the function returns the
#' first reported. A missing `fieldname0_mag` indicates that it is not possible
#' to map the field at position 0 in ProQuest to the MAG fields.
#' In future, this may be made more flexible to consider any of the reported
#' fields in ProQuest.
#'
#' ## When using `from` = "mag_authors"
#' Returns the one field for which the person is observed the most likely to
#' publish over their career. The field is defined based on a confidence score
#' of the likely field of study across all publications in the career.
#'
#'
#' @keywords internal
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
define_field <- function(tbl, conn, from, ...) {

  stopifnot(from %in% c("mag_authors", "graduates"))

  FieldsOfStudy <- dplyr::tbl(conn, "FieldsOfStudy")

  # prepare person-field table
  if (from == "graduates") {
    person_id <- "goid"
    person_field <- dplyr::tbl(conn, "pq_fields_mag") %>%
      dplyr::group_by(.data[[person_id]]) %>%
      dplyr::filter(.data$position == min(.data$position)) %>%
      dplyr::ungroup() %>%
      dplyr::select(.data[[person_id]],
                    fieldname_pq = .data$fieldname,
                    .data$mag_field0)
  } else if (from == "mag_authors") {
    person_id <- "AuthorId"
    person_field <- dplyr::tbl(conn, "author_fields") %>%
      dplyr::filter(.data$FieldClass == "main") %>%
      dplyr::select(.data[[person_id]],
                    mag_field0 = .data$FieldOfStudyId)
  }

  # join name of field0 in mag
  person_field <- person_field %>%
    dplyr::left_join(FieldsOfStudy %>%
                       dplyr::select(.data$FieldOfStudyId,
                                     fieldname0_mag = .data$NormalizedName),
                     by = c("mag_field0" = "FieldOfStudyId")) %>%
    dplyr::select(.data[[person_id]],
                  .data$fieldname0_mag)

  # make final output
  out <- tbl %>%
    dplyr:: left_join(person_field,
                      by = stats::setNames(nm = person_id, person_id))

  dots <- dots_tbl_output(...)
  if (!is.null(dots)) {
    out <- make_tbl_output(out, limit = dots$limit, lazy = dots$lazy)
  }

  return(out)
}
