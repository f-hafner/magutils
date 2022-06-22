
#' Define the field of study for records in a table.
#'
#' @inheritParams doc_common_args
#' @param from A string with options to be queried: "mag_authors" or "graduates".
#' @param ... additional arguments to be passed on to
#'  \code{\link{make_tbl_output}}.
#' If not specified, a lazily evaluated table without limit is returned.
#' Partially specified arguments are completed with
#' \code{\link{dots_tbl_output}}.
#'
#' @return
#' A table with one field name for each person id.
#' The field name is the name of the field at level 0 in MAG.
#' The person id is `AuthorId` for `from` = "mag_authors" and
#' `goid` for `from` = "graduates".
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
#' @export
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
      dplyr::filter(.data$position == 0) %>%
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
