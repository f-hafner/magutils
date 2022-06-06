with_mock_db({
  con <- DBI::dbConnect(RSQLite::SQLite(), "mock_db")

  test_that("we get right columns for proquest authors", {
    d <- authors_proquest(conn = con,
                          lazy = FALSE,
                          limit = 3)
    expect_equal(names(d), c("goid", "degree_year",
                             "university_id", "gender",
                             "fieldname0_mag"))
  })

  test_that("we cannot pass non-numerics", {
    expect_error(authors_proquest(conn = con, limit = 3, start_year = "a"))
    expect_error(authors_proquest(conn = con, limit = 3, end_year = "b"))
  })

})
