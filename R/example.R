


#' Generate the path to the example database
#'
#' Make it easy to access the database for examples.
#'
#' @param file Name of sqlite database for examples.
#'
#' @return A string with the path to the database.
#' @export
#'
#' @examples db_example("AcademicGraph.sqlite")
db_example <- function(file) {
  system.file("extdata", file, package = "magutils", mustWork = TRUE)
}
