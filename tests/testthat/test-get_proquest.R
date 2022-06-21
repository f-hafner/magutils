with_mock_db({
  con <- DBI::dbConnect(RSQLite::SQLite(), "mock_db")

  test_that("get_proquest() gives the right columns for graduates", {
    d <- get_proquest(conn = con,
                      from = "graduates",
                      lazy = FALSE,
                      limit = 3)
    expect_equal(names(d), c("goid", "degree_year",
                             "university_id", "fieldname0_mag",
                             "gender"))
  })

  test_that("get_proquest() gives the right columns for advisors", {
    d <- get_proquest(conn = con,
                      from = "advisors",
                      lazy = FALSE,
                      limit = 3)
    expect_equal(names(d),
                 c("goid", "position", "relationship_id", "gender"))
  })

  test_that("get_proquest() does not accept
              non-numerics for start_year and end_year", {
    expect_error(get_proquest(conn = con, from = "advisors", limit = 3, start_year = "a"),
                 regexp = "Invalid arguments.")
    expect_error(get_proquest(conn = con, from = "advisors", limit = 3, end_year = "b"),
                 regexp = "Invalid arguments.")
    expect_error(get_proquest(conn = con, from = "advisors", limit = 3, end_year = TRUE),
                 regexp = "Invalid arguments.")
  })

})
