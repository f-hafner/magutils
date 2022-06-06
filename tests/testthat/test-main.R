

with_mock_db({
  con <- DBI::dbConnect(RSQLite::SQLite(), "mock_db")

  test_that("we cannot pass non-matching strings to `from`", {
    expect_error(get_links(conn = con,
                           from = "random-string",
                           limit = 1,
                           lazy = FALSE))
  })

  graduates <- get_links(conn = con,
                         from = "graduates",
                         limit = 1,
                         lazy = FALSE)
  advisors <- get_links(conn = con,
                        from = "advisors",
                        limit = 1,
                        lazy = FALSE)

  test_that("we get the a dataframe with the right length", {
    expect_s3_class(graduates, "data.frame")
    expect_equal(nrow(graduates), 1)
  })

  test_that("we get the right columns from linked graduates", {
    expect_equal(names(graduates), c("AuthorId", "goid", "link_score"))
  })

  test_that("we get the right column from linked advisors", {
    expect_equal(names(advisors), c("AuthorId", "relationship_id", "link_score"))
  })

  test_that("we get a lazily evaluated table", {
    d <- get_links(conn = con,
                   from = "graduates",
                   limit = 1,
                   lazy = TRUE)
    expect_s3_class(d, "tbl_lazy")
    expect_s3_class(d, "tbl_sql")
  })

  test_that("we cannot pass linking score outside 0-1 range", {
    expect_error(get_links(conn = con,
                           from = "graduates",
                           min_score = TRUE))
    expect_error(get_links(conn = con,
                           from = "graduates",
                           min_score = -1))
    expect_error(get_links(conn = con,
                           from = "graduates",
                           min_score = 10))
  })

})


with_mock_db({
  con <- DBI::dbConnect(RSQLite::SQLite(), "mock_db")

  test_that("we get right columns for proquest authors", {
    d <- authors_proquest(conn = con,
                          lazy = FALSE,
                          limit = 3)
    expect_s3_class(d, "data.frame")
    expect_equal(nrow(d), 3)
    expect_equal(names(d), c("goid", "degree_year",
                             "university_id", "gender",
                             "fieldname0_mag"))
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







