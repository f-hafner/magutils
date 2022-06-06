with_mock_db({
  con <- DBI::dbConnect(RSQLite::SQLite(), "mock_db")

  d <- graduate_fields(conn = con,
                       limit = 1,
                       lazy = FALSE)

  test_that("we get the right columns from graduate fields", {
    expect_equal(names(d), c("goid", "fieldname0_mag"))
  })

})
