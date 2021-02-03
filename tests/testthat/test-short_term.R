test_that("the change from 10 to 12 is two, when the first value is older", {
  expect_equal(short_term(
    data = data.frame(x = c(10, 12), stg = c(1, 2)),
    value = x,
    bin = stg
  )[[2, 3]], 2)
})

test_that("the change from 10 to 12 is minus two, when the first value is younger", {
  expect_equal(short_term(
    data = data.frame(x = c(10, 12), stg = c(1, 2)),
    value = x,
    bin = stg,
    bin.one = "youngest"
  )[[2, 3]], -2)
})



test_that("when there is no previous bin, NA is returned", {
  expect_equal(short_term(
    data = data.frame(x = c(10, 12), stg = c(1, 2)),
    value = x,
    bin = stg,
    bin.one = "youngest"
  )[[1, 3]], NA_integer_)
})

test_that("the output has the same length as the input for one observation per bin", {
  expect_equal(nrow(short_term(
    data = data.frame(x = c(10, 12), stg = c(1, 2)),
    value = x,
    bin = stg
  )), 2)
})

test_that("the output has the same length as unique stages", {
  expect_equal(nrow(short_term(
    data = data.frame(x = c(1:20), stg = rep(1:10, 2)),
    value = x,
    bin = stg,
    mult.observations = TRUE
  )), length(unique(rep(1:10, 2))))
})

test_that("the output is a tibble for default", {
  expect_equal(tibble::is_tibble(short_term(
    data = data.frame(x = c(1:20), stg = rep(1:10, 2)),
    value = x,
    bin = stg,
    mult.observations = TRUE
  )), TRUE)

  expect_equal(tibble::is_tibble(short_term(
    data = data.frame(x = c(10, 12), stg = c(1, 2)),
    value = x,
    bin = stg
  )), TRUE)
})

test_that("the output is a vector for print.result = 'vector'", {
  expect_equal(is.vector(short_term(
    data = data.frame(x = c(1:20), stg = rep(1:10, 2)),
    value = x,
    bin = stg,
    mult.observations = TRUE,
    print.result = 'vector'
  )), TRUE)

  expect_equal(tibble::is.vector(short_term(
    data = data.frame(x = c(10, 12), stg = c(1, 2)),
    value = x,
    bin = stg,
    print.result = 'vector'
  )), TRUE)
})

test_that("if either bin or value is not specified, return error", {
  expect_error(nrow(short_term(
    data = data.frame(x = c(10, 12), stg = c(1, 2)),
    value = x
  )))

  expect_error(nrow(short_term(
    data = data.frame(x = c(10, 12), stg = c(1, 2)),
    bin = stg
  )))
})
