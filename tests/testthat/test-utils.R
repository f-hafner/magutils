
test_that("valid_sql_limit is non-negative integer or infinite", {
  expect_true(valid_sql_limit(3))
  expect_true(valid_sql_limit(Inf))
  expect_false(valid_sql_limit(-1))
  expect_false(valid_sql_limit(-Inf))
  expect_false(valid_sql_limit(NA))
  expect_false(valid_sql_limit("3"))
  expect_false(valid_sql_limit("a"))
  expect_false(valid_sql_limit(c(1, 2)))
  expect_false(valid_sql_limit(NULL))
})
