test_that("The log odds ratio of all equal groups is 0", {
  expect_equal(log_odds(1, 1, 1, 1, print.result = "vector")[[1]], 0)
})

test_that("If n00 and n10 are equal, and n01 and n11 are equal, then the log odds ratio is 0", {
  expect_equal(log_odds(2, 1, 2, 1, print.result = "vector")[[1]], 0)

})

test_that("Setting alpha to 1 returns confidence intervals at 0", {
  expect_equal(log_odds(2, 1, 2, 1, print.result = "vector", alpha = 1)[[2]], 0)
  expect_equal(log_odds(2, 1, 2, 1, print.result = "vector", alpha = 1)[[3]], 0)
})

test_that("Setting alpha to 1 returns infinite intervals", {
  expect_equivalent(log_odds(2, 1, 2, 1, print.result = "vector", alpha = 0)[[2]], -Inf)
  expect_equal(log_odds(2, 1, 2, 1, print.result = "vector", alpha = 0)[[3]], Inf)
})

test_that("default returns a tibble dataframe", {
  expect_true(tibble::is_tibble(log_odds(2, 1, 2, 1)))
})

test_that("prin.result = 'vector' returns a vector", {
  expect_true(is.vector(log_odds(2, 1, 2, 1, print.result = "vector")))
})

test_that("Error is given when non-numeric values are used", {
  expect_error(log_odds("2", 1, 2, 1))
})

test_that("Default output is rounded properly", {
  expect_equal(log_odds(10, 1, 2, 1)[[1]], 1.609)
})
