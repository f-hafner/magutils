# This file prepares the sqltie database for use in the examples.

# NOTE: to copy the indexes from the production db to the example and
# mockdb, can use: sqlite_master_to_df(production_db) %>% dplyr::filter(tbl_name == "x" & type == "index")
# and then apply over the sql column to create the index

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
graduates <- get_links(conn = conn, from = "graduates",
                       limit = 10, lazy = FALSE)

mag_ids <- graduates$AuthorId
pq_ids <- graduates$goid


advisors <- get_links(conn = conn, from = "advisors",
                      limit = 10, lazy = FALSE)

mag_ids_advisors <- advisors$AuthorId
pq_ids_advisors <- advisors$relationship_id


# Create tables according to functions in pkg

## 1. a) current_links
qry <- paste0("SELECT * FROM current_links WHERE AuthorId IN (",
            paste(mag_ids, collapse = ", "),
            ")")

current_links <- tbl(conn, sql(qry)) %>% collect()

RSQLite::dbWriteTable(conn = example_con,
                      name = "current_links",
                      value = current_links,
                      overwrite = TRUE)

idx_cl <- c(
  "CREATE UNIQUE INDEX idx_t_AuthorIdgoid ON
                            current_links (AuthorId ASC, goid ASC)",
  "CREATE UNIQUE INDEX idx_t_goid ON current_links (goid ASC)"
)

purrr::map(idx_cl,
           .f = ~send_db_stmt(conn = example_con, stmt = .x))

## 1. b) current_links_advisors
qry <- paste0("SELECT * FROM current_links_advisors WHERE AuthorId IN (",
              paste(mag_ids_advisors, collapse = ", "),
              ")")

current_links_advisors <- tbl(conn, sql(qry)) %>% collect()

RSQLite::dbWriteTable(conn = example_con,
                      name = "current_links_advisors",
                      value = current_links_advisors,
                      overwrite = TRUE)

idx_cla <- c(
  "CREATE UNIQUE INDEX idx_cla_AuthorIdrelid
    ON current_links_advisors (AuthorId ASC, relationship_id ASC)",
  "CREATE UNIQUE INDEX idx_cla_relid on current_links_advisors (relationship_id ASC)"
)

purrr::map(idx_cla,
           .f = ~send_db_stmt(conn = example_con, stmt = .x)
)


## 2. proquest authors / advisors

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

### pq_advisors table
  # need both for links and for get_proquest

qry <- paste0("
  SELECT goid, position, lastname, relationship_id
    , SUBSTR(TRIM(firstname),
                  1, instr(trim(firstname)||' ',' ') - 1)
      AS firstname
  FROM pq_advisors
  WHERE relationship_id IN (",
              paste(
                paste0("'",
                       pq_ids_advisors,
                       "'"),
                collapse = ", "),
              ")
    -- ## also add the advisor or graduates in the db
    OR goid IN (",  paste(pq_ids, collapse = ", "),   ")"
)


pq_advisors <- tbl(conn, sql(qry)) %>% collect()
advisor_firstnames <- unique(pq_advisors$firstname) # TODO: add to firstnamesgender to keep!

RSQLite::dbWriteTable(conn = example_con,
                      name = "pq_advisors",
                      value = pq_advisors,
                      overwrite = TRUE)

idx_advisors <- c(
  "CREATE UNIQUE INDEX idx_pqav_relid ON pq_advisors (relationship_id ASC)",
  "CREATE INDEX idx_pqadv_fname ON pq_advisors (firstname ASC)",
  "CREATE INDEX idx_pqadv_idpos ON pq_advisors (goid ASC)"
)

purrr::map(idx_advisors,
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
firstnames_keep <- unique(
  c(author_firstnames, advisor_firstnames)
)
qry <- paste0("
  SELECT FirstName, PersonCount, ProbabilityFemale
  FROM FirstNamesGender
  WHERE FirstName IN (",
  paste0(paste0("'", firstnames_keep, "'"), collapse = ", "),
  ")"
)

FirstNamesGender <- tbl(conn, sql(qry)) %>% collect()

RSQLite::dbWriteTable(conn = example_con,
                      name = "FirstNamesGender",
                      value = FirstNamesGender,
                      overwrite = TRUE)

send_db_stmt(conn = example_con,
             stmt = "CREATE INDEX idx_fng_FirstName ON FirstNamesGender (FirstName ASC)")


### MAG FoS in PQ
qry <- paste0("
  SELECT goid, position, mag_field0, fieldname
  FROM pq_fields_mag
  WHERE goid IN (",
  paste0(pq_ids, collapse = ", "),
  ")"
)

pq_fields_mag <- tbl(conn, sql(qry)) %>% collect()

mag_fields0 <- unique(pq_fields_mag$mag_field0)

RSQLite::dbWriteTable(conn = example_con,
                      name = "pq_fields_mag",
                      value = pq_fields_mag,
                      overwrite = TRUE)

idx_fields <- c(
  "CREATE UNIQUE INDEX idx_pqfm_id on pq_fields_mag (goid ASC, mag_field0 ASC)",
  "CREATE INDEX idx_pqfm_fos on pq_fields_mag (mag_field0 ASC)"
)

purrr::map(idx_fields,
           .f = ~send_db_stmt(conn = example_con, stmt = .x)
)

### MAG FoS in MAG
qry <- paste0("
  SELECT FieldOfStudyId, NormalizedName
  FROM FieldsOfStudy
  WHERE FieldOfStudyId IN (",
  paste0(mag_fields0, collapse = ", "),
              ")"
)

FieldsOfStudy <- tbl(conn, sql(qry)) %>% collect()

RSQLite::dbWriteTable(conn = example_con,
                      name = "FieldsOfStudy",
                      value = FieldsOfStudy,
                      overwrite = TRUE)

idx_fos <- c(
  "CREATE UNIQUE INDEX idx_fos_FieldOfStudyId
                ON FieldsOfStudy (FieldOfStudyId ASC)"
)

purrr::map(idx_fos,
           .f = ~send_db_stmt(conn = example_con, stmt = .x)
)


## 3. affiliations, co-authors and output of the relevant mag ids
mag_ids_keep <- unique(c(mag_ids, mag_ids_advisors))

### Affiliations
qry <- paste0("
  SELECT *
  FROM AuthorAffiliation
  WHERE AuthorId IN (", paste0(mag_ids_keep, collapse = ", "), ")"
)

AuthorAffiliation <- tbl(conn, sql(qry)) %>%
  collect() %>%
  # keep only 2 obs per person
  group_by(AuthorId) %>%
  filter(row_number() <= 2)


RSQLite::dbWriteTable(conn = example_con,
                      name = "AuthorAffiliation",
                      value = AuthorAffiliation,
                      overwrite = TRUE)

idx_aa <- c(
  "CREATE UNIQUE INDEX idx_aa_AuthorAffilYear ON
    AuthorAffiliation (AuthorId ASC, AffiliationId ASC, YEAR)",
  "CREATE INDEX idx_aa_Affil ON AuthorAffiliation (AffiliationId ASC)"
)

purrr::map(idx_aa,
           .f = ~send_db_stmt(conn = example_con, stmt = .x)
)

### Output
qry <- paste0("
  SELECT AuthorId, Year, PaperCount, TotalForwardCitations
  FROM author_output
  WHERE AuthorId IN (", paste0(mag_ids_keep, collapse = ", "), ")"
)

author_output <- tbl(conn, sql(qry)) %>%
  collect() %>%
  # keep only 2 obs per person
  group_by(AuthorId) %>%
  filter(row_number() <= 2)


RSQLite::dbWriteTable(conn = example_con,
                      name = "author_output",
                      value = author_output,
                      overwrite = TRUE)

idx_aa <- c(
  "CREATE UNIQUE INDEX idx_ao_AuthorIdYear ON
    author_output (AuthorId ASC, YEAR)"
)

purrr::map(idx_aa,
           .f = ~send_db_stmt(conn = example_con, stmt = .x)
)


### Co-Authors
qry <- paste0("
  SELECT AuthorId, CoAuthorId
  FROM author_coauthor
  WHERE AuthorId IN (", paste0(mag_ids_keep, collapse = ", "), ")"
)

author_coauthor <- tbl(conn, sql(qry)) %>%
  collect() %>%
  # keep only 2 obs per person
  group_by(AuthorId) %>%
  filter(row_number() <= 2)


RSQLite::dbWriteTable(conn = example_con,
                      name = "author_coauthor",
                      value = author_coauthor,
                      overwrite = TRUE)

idx_aco <- c(
  "CREATE UNIQUE INDEX idx_aco_AuthorIdCoAuthorId ON
    author_coauthor (AuthorId ASC, CoAuthorId ASC)"
)

purrr::map(idx_aco,
           .f = ~send_db_stmt(conn = example_con, stmt = .x)
)



DBI::dbDisconnect(conn)
DBI::dbDisconnect(example_con)

