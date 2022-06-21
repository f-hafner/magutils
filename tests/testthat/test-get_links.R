
with_mock_db({
  con <- DBI::dbConnect(RSQLite::SQLite(), "mock_db")

  invalid_args <- "Invalid arguments."

  test_that("get_links() does not accept non-matching strings to `from`", {
    expect_error(get_links(conn = con,
                           from = "random-string",
                           limit = 1,
                           lazy = FALSE),
                 regex = invalid_args)
  })

  graduates <- get_links(conn = con,
                         from = "graduates",
                         limit = 1,
                         lazy = FALSE)
  advisors <- get_links(conn = con,
                        from = "advisors",
                        min_score = 0.99,
                        limit = 1,
                        lazy = FALSE)

  test_that("get_links() gives a dataframe with the right length", {
    expect_s3_class(graduates, "data.frame")
    expect_equal(nrow(graduates), 1)
  })

  test_that("get_links() gives the right columns", {
    expect_equal(names(graduates),
                 c("AuthorId", "goid", "link_score"))
    expect_equal(names(advisors),
                 c("AuthorId", "relationship_id", "link_score"))
  })


  test_that("get_links() gives a lazily evaluated table", {
    d <- get_links(conn = con,
                   from = "graduates",
                   limit = 1,
                   lazy = TRUE)
    expect_s3_class(d, "tbl_lazy")
    expect_s3_class(d, "tbl_sql")
  })

  test_that("get_links() does not accept linking score outside 0-1 range", {
    expect_error(get_links(conn = con,
                           from = "graduates",
                           min_score = TRUE),
                 regex = invalid_args)
    expect_error(get_links(conn = con,
                           from = "graduates",
                           min_score = -1),
                 regexp = invalid_args)
    expect_error(get_links(conn = con,
                           from = "graduates",
                           min_score = 10),
                 regex = invalid_args)
  })

})



