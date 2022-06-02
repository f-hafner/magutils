## code to prepare `DATASET` dataset goes here

#usethis::use_data(DATASET, overwrite = TRUE)


library(devtools)
library(dplyr)
library(RSQLite)
load_all()

db_file <- "/mnt/ssd/AcademicGraph/AcademicGraph.sqlite"
conn <- connect_to_db(db_file = db_file)

example_dir <- "./inst/extdata/"
example_file <- "AcademicGraph.sqlite"
example_con <- DBI::dbConnect(RSQLite::SQLite(),
                              paste0(example_dir, example_file))

## Source some ids for which we have all the data
graduates <- get_graduate_links(conn = conn, limit = 10, lazy = FALSE)

mag_ids <- graduates$AuthorId
pq_ids <- graduates$goid


## 1. current_links
q <- paste0("SELECT * FROM current_links WHERE AuthorId IN (",
            paste(mag_ids, collapse = ", "),
            ")")

current_links <- tbl(conn, sql(q)) %>% collect()

RSQLite::dbWriteTable(conn = example_con,
                      name = "current_links",
                      value = current_links,
                      overwrite = TRUE)

res <- RSQLite::dbSendStatement(
  conn = example_con,
  statement = "CREATE UNIQUE INDEX idx_cl_Authorgoid ON
              current_links (AuthorId ASC, goid ASC)"
  )

RSQLite::dbClearResult(res)


DBI::dbDisconnect(conn)


