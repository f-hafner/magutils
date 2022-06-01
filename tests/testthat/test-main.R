

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


# capture_mockdb(db_file, f= get_graduate_links(conn = conn,
#                                            limit = 1,
#                                            lazy = F)
#                )
#
# copy_fixture(origin = "_mnt_ssd_AcademicGraph_AcademicGraph.sqlite/",
#              filename = "SELECT-fa6bad")



# what to add
  # expect lazy df when lazy = T
  # unique?
#




