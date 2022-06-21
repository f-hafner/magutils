

# Test small functions
test_that("tidy_string() works", {
  expect_equal(tidy_string("  weird \n string  \n\n"), "weird string")
})



sql1 <- "CREATE INDEX idx1 ON mytable (col1 ASC, col2 ASC)"
sql2 <- "CREATE INDEX idx1 ON mytable (col1, col2))"

test_that("get_idx_cols() works", {
  expect_equal(get_idx_cols(sql1), c("col1", "col2"))
  expect_equal(get_idx_cols(sql2), c("col1", "col2"))
  expect_error(get_idx_cols("CREATE ON mytable"))
  expect_error(get_idx(cols("CREATE UNIQUE INDEX idx1 ON mytable ()")))
})


# Prepare in-memory db for larger functions

memcon <- dbConnect(RSQLite::SQLite(), ":memory:")
df1 <- dplyr::tibble(a = c(1, 2), b = c(100, 500))
df2 <- dplyr::bind_rows(df1, df1)
dplyr::copy_to(dest = memcon, df = df1, name = "table1", overwrite = T)
dplyr::copy_to(dest = memcon, df = df2, name = "table2", overwrite = T)

idx_queries <- c(
  "create unique index idx1 on table1 (a)",
  "create index idx2 on table2 (a)"
)

idx <- lapply(idx_queries, function(q) {
  res <- DBI::dbSendQuery(memcon, q)
  res <- DBI::dbClearResult(res)
})

df_master <- sqlite_master_to_df(memcon, temp = T)
test_that("sqlite_master_to_df() works", {
  expect_equal(names(df_master),
               c("type", "name", "tbl_name", "sql"))
  expect_equal(tolower(df_master[df_master$name == "idx1", "sql"]),
               idx_queries[1])
})

test_that("has_idx() works", {
  expect_true(has_idx(memcon, tbl = "table1", temp = T, on_cols = "a"))
  expect_false(has_idx(memcon, tbl = "table2", temp = T,
                       on_cols = "a", keep_unique = TRUE))
})

test_that("get_tbl_idx works", {
  expected_list <- list(
    idx1 = list(
      idx_unique = TRUE,
      idx_cols = "a"
    )
  )
  expect_identical(get_tbl_idx(conn = memcon, tbl = "table1", temp = T),
                   expected_list)
})


dbDisconnect(memcon)



