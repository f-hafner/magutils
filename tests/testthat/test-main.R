

with_mock_db({
  con <- DBI::dbConnect(RSQLite::SQLite(), "mock_db")

  test_that("we get the right columns from linked graduates", {
    d <- get_graduate_links(conn = con,
                            limit = 1,
                            lazy = F)
    expect_s3_class(d, "data.frame")
    expect_equal(names(d), c("AuthorId", "goid", "link_score"))
  })

  test_that("we get a lazily evaluated table", {
    d <- get_graduate_links(conn = con,
                            limit = 1,
                            lazy = T)
    expect_s3_class(d, "tbl_lazy")
    expect_s3_class(d, "tbl_sql")
  })

})


with_mock_db({
  con <- DBI::dbConnect(RSQLite::SQLite(), "mock_db")

  test_that("we get right columns for proquest authors", {
    d <- authors_proquest(conn = con,
                          lazy = FALSE,
                          limit = 3)
    expect_s3_class(d, "data.frame")
    expect_equal(names(d), c("goid", "degree_year", "university_id", "gender"))
  })

  test_that("we get a lazily evaluated table", {
    d <- authors_proquest(conn = con, limit = 3)
    expect_s3_class(d, "tbl_lazy")
    expect_s3_class(d, "tbl_sql")
  })

  test_that("we cannot pass non-numerics", {
    expect_error(authors_proquest(conn = con, limit = 3, start_year = "a"))
    expect_error(authors_proquest(conn = con, limit = 3, end_year = "b"))
  })

})







