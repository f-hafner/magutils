# This file contains helper functions to check indexes on a table,
# and may be extended in the future according to the following use cases:
  # 1. does a table have a (unique) index on some columns? (implemented)
  # 2. list all indexes and their constituting columns on a given table
  # 3. list all indexes that are formed on that column (not implemented)
  # 4. which columns does index x cover? (not implemented)


## Main functions for export
#-----------------------------
#-----------------------------


#' Extract indexes from a table
#'
#' Create a list of indexes from a table. The list reports on which columns the
#' index exists and whether has the UNIQUE constraint. The function processes
#' output from \code{\link{sqlite_master_to_df}}.
#'
#' @inheritParams doc_common_args
#' @param on_tbl The name of the table.
#' @param temp Should `sqlite_temp_master` be queried, instead of
#' `sqlite_master`? Default is FALSE. This can be useful when looking for
#' temporary tables and indexes on them.
#'
#' @return A named list of lists. Each list corresponds to one index on `on_tbl`.
#' A elements (top-level) of the list are named according to the name of the
#' indexes in the database.
#' Each element is a list with two entries:
#' - `idx_unique`: A logical indicating whether the index satisfies the UNIQUE
#'     constraint as in `CREATE UNIQUE INDEX`.
#' - `idx_cols`: A character vector with the index columns.
#'
#' @export
#' @examples
#' conn <- connect_to_db(db_example("AcademicGraph.sqlite"))
#' get_tbl_idx(conn, "author_output")
#' @importFrom rlang .data
#' @importFrom magrittr %>%
get_tbl_idx <- function(conn, on_tbl, temp = FALSE) {

  df <- sqlite_master_to_df(conn, temp = temp) %>%
    dplyr::filter(.data[["tbl_name"]] == on_tbl & .data[["type"]] == "index") %>%
    dplyr::mutate(sql = tidy_string(.data[["sql"]]),
                  idx_unique = grepl("create unique index",
                                     tolower(.data[["sql"]])
                                     )
    )

  tryCatch(
    error = function(cnd) {
      stop("Can't find any information for table ", on_tbl,
           ". Is it in the database?")
    },
    out <- apply(df, 1, function(x) {
      out <- list(
        idx_unique = as.logical(x[["idx_unique"]]),
        idx_cols = get_idx_cols(x[["sql"]])
      )
    })
  )
  names(out) <- df$name

  return(out)
}



#' Check if a table has an index
#'
#' Check if the table holds an index on the specified columns
#' and optionally check whether it is unique.
#'
#' @inheritParams doc_common_args
#' @param on_tbl A table in the database.
#' @param on_cols A character vector with the columns to check.
#' @param keep_unique A logical. Additionally check if the index has the
#' UNIQUE constraint. Default is FALSE.
#' @inheritParams get_tbl_idx
#'
#' @return A logical.
#' @details The function only checks *exact* matching of `on_cols`, that is, the
#' order of columns in `on_cols` needs to correspond to the order of columns in
#' the CREATE INDEX statement: If `on_cols = c("col1", "col2")`, but the
#' database has `"CREATE UNIQUE INDEX idx1 ON mytable(col2, col1)"`,
#' the function will return FALSE.
#' @export
#'
#' @examples
#' conn <- connect_to_db(db_example("AcademicGraph.sqlite"))
#' has_idx(conn, "author_output", "AuthorId", keep_unique = TRUE)
has_idx <- function(conn, on_tbl, on_cols, keep_unique = FALSE, temp = FALSE) {

  indexes <- get_tbl_idx(conn, on_tbl = on_tbl, temp = temp)
  has_index <- sapply(indexes, function(x) {
    identical(x[["idx_cols"]], on_cols)
  })

  if (!keep_unique) {
    indexes <- indexes[has_index]
  } else {
    is_unique <- sapply(indexes, function(x) {
      x[["idx_unique"]] == T
    })
    indexes <- indexes[has_index & is_unique]
  }

  out <- length(indexes) > 0
  return(out)

}

#' Transform sqlite_master table to a dataframe.
#'
#' This is a wrapper to query sqlite_master from the sqlite database.
#'
#' @inheritParams doc_common_args
#' @inheritParams get_tbl_idx

#' @return A dataframe with type, name, tbl_name and sql statement
#' from sqlite_master.
#' @export
#'
#' @examples
#' conn <- connect_to_db(db_example("AcademicGraph.sqlite"))
#' dplyr::glimpse(sqlite_master_to_df(conn))
sqlite_master_to_df <- function(conn, temp = FALSE) {

  src_tbl <- "sqlite_master"
  if (temp) src_tbl <- "sqlite_temp_master"
  res <- DBI::dbSendQuery(
    conn = conn,
    statement = paste0("SELECT type, name, tbl_name, sql FROM ", src_tbl)
  )
  df <- DBI::dbFetch(res) # returns a df with the relevant information
  DBI::dbClearResult(res)
  return(df)
}

## Helpers for main functions
#-----------------------------
#-----------------------------

#' Remove redundant white spaces and new lines
#'
#' @param s A string.
#'
#' @return The string, with new lines and excess white spaces removed.
#' Excess white spaces refer to both trailing and leading as well as
#' multiple white spaces next to each other.
#'
#' @keywords internal
#'
#' @examples magutils:::tidy_string("  some string     with  many   white spaces. ")
tidy_string <- function(s) {
  s <- gsub("\\n", "", s)
  s <- trimws(gsub("\\s+", " ", s))
  return(s)
}




#' Extract constituting columns from a "create index" statement
#'
#' This function takes a string from a SQL statement that creates an index
#' on a table, and returns the columns that constitute the index.
#'
#' @param stmt A SQLite statement to create an index on a table on some columns.
#' The function extracts the columns on which the index is created. As the
#' example illustrates, "ASC" and "DESC" statements are removed.
#'
#' @return A character vector with the column names defining the index.
#'
#' @keywords internal
#' @examples
#' magutils:::get_idx_cols("CREATE INDEX idx1 ON mytable (col1 ASC, col2 ASC)")
#' # gives  c("col1", "col2")
get_idx_cols <- function(stmt) {

  if (!grepl("create index|create unique index", tolower(stmt))) {
    stop("This statement does not seem to create an index.")
  }

  start <- gregexpr( "(?=\\()", stmt, perl = TRUE)[[1]][1] # positive look-ahead
  end <- gregexpr( "(?<=\\))", stmt, perl = TRUE)[[1]][1] # positive look-behind

  idx_cols <- substr(stmt, start + 1, end -2)
  idx_cols <- gsub("ASC|DESC", "", idx_cols)
  idx_cols <- trimws(idx_cols)

  out <- strsplit(idx_cols, ",")[[1]]
  # out <- lapply(out, trimws)
  out <- sapply(out, trimws, USE.NAMES = F)
  if (length(out) == 0) {
    stop("Could not extract any columns. Are you sure `stmt` is correctly specified?")
  }

  return(out)
}










