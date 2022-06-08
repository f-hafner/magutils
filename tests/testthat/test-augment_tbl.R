with_mock_db({
  con <- DBI::dbConnect(RSQLite::SQLite(), "mock_db")

  df <- tidyr::tibble(AuthorId = c(1, 2),
                      goid = c(3,4))

  # re-use fixtures from get_links
  graduates <- get_links(conn = con,
                         from = "graduates",
                         limit = 1,
                         lazy = TRUE)

  graduates_affil <- augment_tbl(graduates, con, with_info = "affiliation",
                                 lazy = FALSE, limit = 1)
  graduates_output <- augment_tbl(graduates, con, with_info = "output",
                                  lazy = FALSE, limit = 1)

  test_that("we cannot pass dataframes ", {
    expect_error(augment_tbl(df, con, with_info = "affiliation"))
  })

  test_that("we get an error when passing wrong options ", {
    expect_error(augment_tbl(graduates, con, with_info = "affil"))
    expect_error(augment_tbl(graduates, con))
    expect_error(augment_tbl(graduates, con, with_info = "affiliation", on_col = "authorid"))
  })

  test_that("we get the right columns ", {
    expect_equal(names(graduates_affil),
                 c("AuthorId", "goid", "link_score", "AffiliationId", "Year"))
    expect_equal(names(graduates_output),
                 c("AuthorId", "goid", "link_score", "Year",
                   "PaperCount", "TotalForwardCitations"))
  })



})
