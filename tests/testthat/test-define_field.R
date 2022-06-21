with_mock_db({
  con <- DBI::dbConnect(RSQLite::SQLite(), "mock_db")

  d <- dplyr::tbl(con, "Authors") %>%
    dplyr::select(AuthorId) %>%
    define_field(conn = con,
                 from = "mag_authors",
                 limit = 1,
                 lazy = FALSE)

  test_that("define_field() gives the right columns from graduate fields", {
    expect_equal(names(d), c("AuthorId", "fieldname0_mag"))
  })

})
