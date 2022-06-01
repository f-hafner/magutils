

with_mock_db({
  con <- DBI::dbConnect(RSQLite::SQLite(), "mock_db")

  test_that("we get the right columns from linked graduates", {
    d_graduates <- get_graduate_links(conn = con, limit = 1, keep_unique = F, lazy = F)
    expect_s3_class(d_graduates, "data.frame")
    expect_equal(names(d_graduates), c("AuthorId", "goid", "link_score"))
  })
})

# what to add
  # expect lazy df when lazy = T
  # unique?
#




