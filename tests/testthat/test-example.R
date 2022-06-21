

test_that("db_example() finds right file", {
  expect_error(db_example("randomfile"),
               regexp = "no file found|No file found")
  # R CMD check gives "no file found", test() gives "No file found"
  expect_match(db_example("AcademicGraph.sqlite"), "AcademicGraph")
})
