
# Prepare the sqlite database used in the examples.

# Setup

library(devtools)
load_all()
library(dplyr)
library(RSQLite)


send_db_stmt <- function(conn, stmt) {
  res <- RSQLite::dbSendStatement(
    conn = conn,
    statement = stmt
  )
  RSQLite::dbClearResult(res)
}


db_file <- "/mnt/ssd/AcademicGraph/AcademicGraph.sqlite"

example_dir <- "./inst/extdata/"
example_file <- "AcademicGraph.sqlite"

# Connections
conn <- connect_to_db(db_file = db_file)
example_con <- connect_to_db(db_file = paste0(example_dir, example_file))


# Source some ids for which we have all the data
graduates <- get_graduate_links(conn = conn, limit = 10, lazy = FALSE)

mag_ids <- graduates$AuthorId
pq_ids <- graduates$goid


# Create tables according to function in pkg

## 1. current_links
qry <- paste0("SELECT * FROM current_links WHERE AuthorId IN (",
            paste(mag_ids, collapse = ", "),
            ")")

current_links <- tbl(conn, sql(qry)) %>% collect()

RSQLite::dbWriteTable(conn = example_con,
                      name = "current_links",
                      value = current_links,
                      overwrite = TRUE)

send_db_stmt(conn = example_con,
             stmt =  "CREATE UNIQUE INDEX idx_cl_Authorgoid ON
                            current_links (AuthorId ASC, goid ASC)"
             )


## 2. proquest authors

qry <- paste0("
  SELECT goid, firstname, degree_year, university_id
  FROM pq_authors
  WHERE goid IN (",
  paste(pq_ids, collapse = ", "), ")"
)

### pq_authors table
pq_authors <- tbl(conn, sql(qry)) %>% collect()
uni_ids <- unique(pq_authors$university_id)
author_firstnames <- unique(pq_authors$firstname)

RSQLite::dbWriteTable(conn = example_con,
                      name = "pq_authors",
                      value = pq_authors,
                      overwrite = TRUE)

idx_authors <- c(
  "CREATE UNIQUE INDEX idx_pqaut_goid ON pq_authors (goid ASC)",
  "CREATE INDEX idx_pqaut_uni ON pq_authors (university_id ASC)",
  "CREATE INDEX idx_pqaut_fname ON pq_authors (firstname ASC)"
)

purrr::map(idx_authors,
           .f = ~send_db_stmt(conn = example_con, stmt = .x)
           )

### pq_unis table
qry <- paste0("
  SELECT university_id, location
  FROM pq_unis
  WHERE university_id IN (",
  paste0(uni_ids, collapse = ", "),
  ")"
)

pq_unis <- tbl(conn, sql(qry)) %>% collect()

RSQLite::dbWriteTable(conn = example_con,
                      name = "pq_unis",
                      value = pq_unis,
                      overwrite = TRUE)

send_db_stmt(conn = example_con,
             stmt = "CREATE UNIQUE INDEX idx_pqu_id ON pq_unis (university_id ASC)")


### FirstNamesGender table
qry <- paste0("
  SELECT FirstName, PersonCount, ProbabilityFemale
  FROM FirstNamesGender
  WHERE FirstName IN (",
  paste0(paste0("'", author_firstnames, "'"), collapse = ", "),
  ")"
)

FirstNamesGender <- tbl(conn, sql(qry)) %>% collect()

RSQLite::dbWriteTable(conn = example_con,
                      name = "FirstNamesGender",
                      value = FirstNamesGender,
                      overwrite = TRUE)

send_db_stmt(conn = example_con,
             stmt = "CREATE INDEX idx_fng_FirstName ON FirstNamesGender (FirstName ASC)")



DBI::dbDisconnect(conn)
DBI::dbDisconnect(example_con)

