
# This file contains helpers for mocking the database for tests


#' Capture a function call to a production database in mock fixtures.
#'
#' This is a wrapper for capturing database fixtures with dittodb.
#' It connects to the production database, captures the query, and disconnects
#' from the production database.
#'
#' @param production_db The production database with the full data
#' @param mock_db The path to the mock database containing the fixtures for
#' dittodb. Default is `tests/testthat/mock_db/`.
#' @param f The function with SQL calls to be captured with all options
#' set inside.
#'
#' @return Saves fixtures to `mock_db/dir/` where `dir` is created and
#' defined by dittodb
#' based on name of `production_db`. For instance, "/mypath/database.sqlite"
#' becomes "_mypath_database.sqlite/".
#' @keywords internal
#' @export
capture_mockdb <- function(production_db,
                           mock_db = "./tests/testthat/mock_db/", f) {
  dittodb::start_db_capturing(mock_db)

  conn <- DBI::dbConnect(RSQLite::SQLite(), production_db)
  f
  DBI::dbDisconnect(conn)

  dittodb::stop_db_capturing()
}


#' Copy fixtures to make available for tests
#'
#' A wrapper to around \code{\link[base]{dput}} to copy a database fixture
#' from `origin` to a directory with fixtures used by dittodb.
#'
#' @inheritParams capture_mockdb
#' @param origin The origin directory where dittodb saved the fixtures
#' from capturing
#' @param filename The name of the file to be copied.
#' @param verbose Logical. Should filenames be printed?
#' The filename consist of the first SQL verb of the query
#' (often "SELECT") and the
#' hash of the query that is sent.
#' See the dittodb documentation for details.
#'
#' @return Copies the fixture from `mock_db/origin/` to `mock_db/`.
#' @keywords internal
#' @export
copy_fixture <- function(mock_db = "./tests/testthat/mock_db/", origin,
                         filename, verbose = FALSE) {
  if (verbose) {
    cat(filename, "\n")
  }
  df_fixt <- source(paste0(mock_db, origin, filename, ".R"),
                    keep.source = FALSE)$value
  # perhaps do some operation: https://dittodb.jonkeane.com/articles/dittodb.html
  dput(df_fixt,
       file = paste0(mock_db, filename, ".R"),
       control = c("all", "hexNumeric"))
}







