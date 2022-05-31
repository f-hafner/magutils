test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})



# this should be deleted I suppose? where can I put it?
start_db_capturing("tests/testthat/mock_db/")

conn <- DBI::dbConnect(RSQLite::SQLite(), db_file)
get_graduate_links(conn = conn, limit = 1, keep_unique = F, lazy = F)
DBI::dbDisconnect(conn)

stop_db_capturing()

# what do I learn?
  # get_graduate_links queries the db twice; this is why there are multiple files
    # can I simplify this?
  # the mockdb directory is created at connecting. can I not change the name?



# which tests to run?
# expect s4 from connect? why not s3? not sure that also works with dittodb?
# get gender? thresholds; keep / drop na, "male", "female"
# generally that we get the right names? or not?
# expect lazy connection!! -- how can I test for this?
# which tests are important? what am I confident working with?


# problem: the test will fail if the links change!
# need a more flexible test
# test non-unique / unique!
with_mock_db({
  con <- DBI::dbConnect(RSQLite::SQLite(), "mock_db")

  test_that("we get the right columns from linked graduates", {
    d_graduates <- get_graduate_links(conn = con, limit = 1, keep_unique = F, lazy = F)
    # expect_equal(d_graduates$AuthorId, 2661192675)
    # expect_equal(d_graduates$goid, 89188005)
    expect_s3_class(d_graduates, "data.frame")
    expect_equal(names(d_graduates), c("AuthorId", "goid", "link_score"))
  })
})

# so: collect() in the test does not work b/c it is not a recorded db operation
# key issue:
  # want

df_fixt <- source("./tests/testthat/mock_db/_mnt_ssd_AcademicGraph_AcademicGraph.sqlite/SELECT-6c2292.R", keep.source = FALSE)$value

# filter out anything after february and all days after the 9th of the month
df_fixt <- dplyr::filter(df_fixt, month <= 2 & day < 10)

# save the fixture for use in tests
dput(df_fixt, file = "./tests/testthat/mock_db/SELECT-6c2292.R", control = c("all", "hexNumeric"))


