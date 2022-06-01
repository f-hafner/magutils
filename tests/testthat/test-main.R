

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

})


# capture_mockdb(db_file, f= get_graduate_links(conn = conn,
#                                            limit = 1,
#                                            lazy = F)
#                )
#
# copy_fixture(origin = "_mnt_ssd_AcademicGraph_AcademicGraph.sqlite/",
#              filename = "SELECT-fa6bad")


# capture_mockdb(db_file, f= authors_proquest(conn = conn, lazy = FALSE, limit = 3)
#                )
#
# copy_fixture(origin = "_mnt_ssd_AcademicGraph_AcademicGraph.sqlite/",
#              filename = "SELECT-f684d8")







