

test_that("correct file is found", {
  expect_error(db_example("randomfile"))
  expect_match(db_example("AcademicGraph.sqlite"), "AcademicGraph")
})
