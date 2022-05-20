

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
  print(src_dbi(con))
  return(con)
}



