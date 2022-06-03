
# Define some variables used in functions below
threshold_prob_female <- 0.8


geemp_fields <- c("geology", "geography", "environmental science",
                  "mathematics", "computer science", "engineering",
                  "chemistry", "physics", "economics")
lps_fields <- c("biology", "psychology", "sociology", "political science")



#' Connect to the database
#'
#' @param db_file full name (including path) of the file.
#'
#' @return Returns an object of the DBIConnection class and prints the db info
#' of the connection with `dbplyr::src_dbi()`.
#' @export
#'
#' @examples conn <- connect_to_db(db_example("AcademicGraph.sqlite"))
connect_to_db <- function(db_file) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
  cat("The database connection is: \n")
  print(dbplyr::src_dbi(con))
  return(con)
}


#' Load links between MAG and ProQuest
#'
#' @param conn An object of the DBIConnection class.
#' @param limit LIMIT of the query. A positive integer or Inf.
#' Default is Inf, in which case all records are returned.
#' @param lazy If TRUE (the default), does not `collect()` the query into a dataframe.
#' This is useful if other tables from the database are joined later on.
#'
#' @return A query of linked goid-AuthorId.
#' @export
#'
#' @examples
#' conn <- connect_to_db(db_example("AcademicGraph.sqlite"))
#' links <- get_graduate_links(conn)
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
get_graduate_links <- function(conn, limit = Inf, lazy = TRUE) {

  stopifnot(valid_sql_limit(limit))

  query_links <- "
    SELECT AuthorId, goid, link_score
    FROM current_links
    WHERE link_score > 0.7
  "

  if (limit < Inf) {
    query_links <- paste0(query_links, " LIMIT ?value")
    query_links <- DBI::sqlInterpolate(
      conn,
      query_links,
      value = limit
    )
  }

  links <- dplyr::tbl(conn, dbplyr::sql(query_links))

  if (!lazy) {
    links <- links %>% dplyr::collect()
  }

  return(links)
}


#' Define gender based on first name.
#'
#' @param conn An object of the DBIConnection class.
#'
#' @param table A lazily evaluated table sourced from `conn`.
#' @param drop_missing If TRUE, drops records without clear gender assigned.
#' Clear assignment is when probability of either gender is >= 0.8
#' @param firstname_left Column containing the firstname in `table` and to be used for joining gender on.
#'
#' @return `table` augmented by a gender column.
#' @export
#'
#' @examples \dontrun{
#' new_table <- define_gender(
#' conn = conn, table = old_table,
#' firstname_left = "firstname_old", drop_missing = TRUE
#' )
#' }
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
define_gender <- function(conn, table, firstname_left, drop_missing) {

  names_gender <- dplyr::tbl(conn, "FirstNamesGender") %>%
    dplyr::select(-.data$PersonCount)
  # TODO: check whether Firstname in table?

  table <- table %>%
    dplyr::inner_join(names_gender,
                      by = stats::setNames(nm = firstname_left, "FirstName")) %>%
    dplyr::mutate(gender = dplyr::case_when(
      .data$ProbabilityFemale >= threshold_prob_female ~ "Female",
      .data$ProbabilityFemale <= 1 - threshold_prob_female ~ "Male")
    ) %>%
    dplyr::select(-.data$ProbabilityFemale)

  if (drop_missing) {
    table <- table %>%
      dplyr::filter(!is.na(.data$gender))
  }

  return(table)
}


#' Source PhD graduates
#'
#' @param conn An object of the DBIConnection class.
#' @param start_year Lowest graduation year to consider. Default: 1985.
#' @param end_year Highest graduation year to consider. Default: 2005.
#' @param lazy If TRUE (the default), does not `collect()` the query into a dataframe.
#' This is useful if other tables from the database are joined later on.
#' @param limit  LIMIT of the query. A positive integer or Inf.
#' Default is Inf, in which case all records are returned.
#'
#' @examples
#' conn <- connect_to_db(db_example("AcademicGraph.sqlite"))
#' d_graduates <- authors_proquest(conn = conn)
#'
#' @return A table with U.S. PhD graduates and their gender.
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
authors_proquest <- function(conn, start_year = 1985, end_year = 2005,
                             lazy = TRUE, limit = Inf) {

  stopifnot(valid_sql_limit(limit))
  stopifnot(is.double(start_year) && is.double(end_year))

  query_keep_us <- "
    SELECT university_id
    FROM pq_unis
    WHERE location LIKE '%United States%'
  "
  us_universities <- dplyr::tbl(conn, dbplyr::sql(query_keep_us))

  d <- dplyr::tbl(conn, "pq_authors") %>%
    dplyr::inner_join(us_universities, by = "university_id") %>%
    dplyr::filter(.data$degree_year >= start_year
                  & .data$degree_year <= end_year) %>%
    dplyr::select(.data$goid, .data$degree_year,
                  .data$university_id, firstname_pq = .data$firstname)

  d <- define_gender(conn = conn,
                     table = d,
                     firstname_left = "firstname_pq",
                     drop_missing = TRUE) %>%
    dplyr::select(-.data$firstname_pq)

  d <- d %>%
    dplyr::left_join(graduate_fields(conn),
                     by = "goid")

  if (limit < Inf) {
    d <- utils::head(d, limit)
  }

  if (!lazy) {
    d <- d %>% dplyr::collect()
  }

  return(d)

}


#' Define the field of study for graduates.
#'
#' @param conn An object of the DBIConnection class.
#' @param lazy If TRUE (the default), does not `collect()` the query into a dataframe.
#' This is useful if other tables from the database are joined later on.
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

  if (limit < Inf) {
    goid_field <- utils::head(goid_field, limit)
  }

  if (!lazy) {
    goid_field <- goid_field %>% dplyr::collect()
  }

  return(goid_field)
}


