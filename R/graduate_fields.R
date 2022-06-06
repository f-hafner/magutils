
#' Define the field of study for graduates.
#'
#' @param conn An object of the DBIConnection class.
#' @param lazy If TRUE (the default), does not `collect()` the query into a
#' dataframe. This is useful if other tables from the database are joined later on.
#' @param limit LIMIT of the query. A positive integer or Inf.
#' Default is Inf, in which case all records are returned.
#'
#' @return A table with one field name for each goid.
#' The field name is the name of the field at level 0 in MAG. The field is
#' defined with a custom mapping between the reported field of study in ProQuest
#' and the fields in MAG. The mapping is stored in the database.
#'
#' **Note**: A record can have multiple fields, and currently the function returns the
#' first reported. A missing `fieldname0_mag` indicates that it is not possible
#' to map the field at position 0 in ProQuest to the MAG fields.
#' In future, this may be made more flexible to consider any of the reported
#' fields in ProQuest.
#' @export
#'
#' @examples \dontrun{
#' d <- graduate_fields(db_example("AcademicGraph.sqlite"))
#' }
#' @importFrom rlang .data
#' @importFrom magrittr %>%
graduate_fields <- function(conn, lazy = TRUE, limit = Inf) {

  stopifnot(valid_sql_limit(limit))

  FieldsOfStudy <- dplyr::tbl(conn, "FieldsOfStudy")

  goid_field <- dplyr::tbl(conn, "pq_fields_mag") %>%
    dplyr::filter(.data$position == 0) %>%
    dplyr::select(.data$goid,
                  fieldname_pq = .data$fieldname,
                  .data$mag_field0) %>%
    dplyr::left_join(FieldsOfStudy %>%
                       dplyr::select(.data$FieldOfStudyId,
                                     fieldname0_mag = .data$NormalizedName),
                     by = c("mag_field0" = "FieldOfStudyId")) %>%
    dplyr::select(.data$goid,
                  .data$fieldname0_mag)

  goid_field <- make_tbl_output(tbl = goid_field,
                                limit = limit,
                                lazy = lazy)

  return(goid_field)
}
