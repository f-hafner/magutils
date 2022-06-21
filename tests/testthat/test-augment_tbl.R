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

  error_nonvalid_args <- "Non-valid arguments."

  test_that("augment_tbl() does not accept dataframes", {
    expect_error(augment_tbl(df, con, with_info = "affiliation"),
                 regexp = error_nonvalid_args)
  })

  test_that("augment_tbl() gives an error when passing wrong options ", {
    expect_error(augment_tbl(graduates, con, with_info = "affil"),
                 regexp = error_nonvalid_args)
    expect_error(augment_tbl(graduates, con,
                             with_info = "affiliation", on_col = "authorid"),
                 regexp = error_nonvalid_args)
  })

  test_that("augment_tbl() gives the right columns ", {
    expect_equal(names(graduates_affil),
                 c("AuthorId", "goid", "link_score", "AffiliationId", "Year"))
    expect_equal(names(graduates_output),
                 c("AuthorId", "goid", "link_score", "Year",
                   "PaperCount", "TotalForwardCitations"))
  })



})
