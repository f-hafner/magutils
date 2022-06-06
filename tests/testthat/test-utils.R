
test_that("valid_sql_limit is non-negative integer or infinite", {
  expect_true(valid_sql_limit(3))
  expect_true(valid_sql_limit(Inf))
  expect_false(valid_sql_limit(-1))
  expect_false(valid_sql_limit(-Inf))
  expect_false(valid_sql_limit(NA))
  expect_false(valid_sql_limit("3"))
  expect_false(valid_sql_limit("a"))
  expect_false(valid_sql_limit(c(1, 2)))
  expect_false(valid_sql_limit(NULL))
})


with_mock_db({
  con <- DBI::dbConnect(RSQLite::SQLite(), "mock_db")

  test_that("we get a dataframe with the right nrow", {
    d_df <- make_tbl_output(dplyr::tbl(con, "current_links"),
                            limit = 2,
                            lazy = FALSE)
    expect_s3_class(d_df, "data.frame")
    expect_equal(nrow(d_df), 2)
  })

  test_that("we get a lazily evaluated table", {
    d_lazy <- make_tbl_output(dplyr::tbl(con, "current_links"),
                              limit = 2,
                              lazy = TRUE)
    expect_s3_class(d_lazy, "tbl_lazy")
    expect_s3_class(d_lazy, "tbl_sql")
  })

})
