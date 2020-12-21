test_that("4 represents a 100 % increase compared to 2 with default values", {
  expect_equal(perc_change(2, 4), 1)
})

test_that("4 represents a 100 % increase compared to 2 with print.result ='value'", {
  expect_equal(perc_change(2, 4), 1)
})

test_that("4 represents a 100 % increase compared to 2 with print.result ='value'", {
  expect_equal(perc_change(2, 4, print.result = "percentage"), 100)
})

test_that("4 represents a 100 % increase compared to 2 with print.result ='text'", {
  expect_match(perc_change(2, 4, print.result = "text"), "The percentage change from 2 to 4 is 100%")
})

test_that("default returns a numeric expression", {
  expect_true(is.numeric(perc_change(2, 4)))
})

test_that("print.result ='text' a character expression", {
  expect_true(is.character(perc_change(2, 4, print.result = "text")))
})

test_that("Error is given when non-numeric values are used", {
  expect_error(perc_change("a", 4,))
})

test_that("Default output is rounded properly", {
  expect_equal(perc_change(7, 5), -0.2857143)
})
