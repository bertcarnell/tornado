# Copyright 2021 Robert Carnell

context("test-create_endpoints")

test_that("PercentChange - No factors", {
  used_variables <- rownames(attr(stats::terms(model_nofactors), "factors"))[-1]
  training_data <- subset(mtcars, select = used_variables)
  means <- .create_means(training_data)
  ret <- .create_endpoints(training_data, means, type = "PercentChange",
                           alpha = 0.10)

  expect_equal(dim(ret$endpoints), c(2,3))
  expect_true(is.data.frame(ret$endpoints))
  expect_type(ret$endpoints$cyl, "double")
})

test_that("PercentChange - Some factors", {
  # one factor
  used_variables <- rownames(attr(stats::terms(model_onefactor), "factors"))[-1]
  training_data <- subset(my_mtcars, select = used_variables)
  means <- .create_means(training_data)
  ret <- .create_endpoints(training_data, means, type = "PercentChange",
                           alpha = 0.10)

  expect_equal(dim(ret$endpoints), c(2,3))
  expect_true(is.data.frame(ret$endpoints))
  expect_true(is.factor(ret$endpoints$cyl))

  # two factors
  used_variables <- rownames(attr(stats::terms(model_twofactors), "factors"))[-1]
  training_data <- subset(my_mtcars, select = used_variables)
  means <- .create_means(training_data)
  ret <- .create_endpoints(training_data, means, type = "PercentChange",
                           alpha = 0.10)

  expect_equal(dim(ret$endpoints), c(2,4))
  expect_true(is.data.frame(ret$endpoints))
  expect_true(is.factor(ret$endpoints$cyl))
})

test_that("PercentChange - All factors", {
  used_variables <- rownames(attr(stats::terms(model_allfactors), "factors"))[-1]
  training_data <- subset(my_mtcars, select = used_variables)
  means <- .create_means(training_data)
  ret <- .create_endpoints(training_data, means, type = "PercentChange",
                           alpha = 0.10)

  expect_equal(dim(ret$endpoints), c(2,2))
  expect_true(is.data.frame(ret$endpoints))
  expect_type(ret$endpoints$cyl, "logical")
})

################################################################################

test_that("percentile - No factors", {
  used_variables <- rownames(attr(stats::terms(model_nofactors), "factors"))[-1]
  training_data <- subset(mtcars, select = used_variables)
  means <- .create_means(training_data)
  ret <- .create_endpoints(training_data, means, type = "percentiles",
                           alpha = 0.10)

  expect_equal(dim(ret$endpoints), c(2,3))
  expect_true(is.data.frame(ret$endpoints))
  expect_type(ret$endpoints$cyl, "double")
})

test_that("percentile - Some factors", {
  # one factor
  used_variables <- rownames(attr(stats::terms(model_onefactor), "factors"))[-1]
  training_data <- subset(my_mtcars, select = used_variables)
  means <- .create_means(training_data)
  ret <- .create_endpoints(training_data, means, type = "percentiles",
                           alpha = 0.10)

  expect_equal(dim(ret$endpoints), c(2,3))
  expect_true(is.data.frame(ret$endpoints))
  expect_true(is.factor(ret$endpoints$cyl))

  # two factors
  used_variables <- rownames(attr(stats::terms(model_twofactors), "factors"))[-1]
  training_data <- subset(my_mtcars, select = used_variables)
  means <- .create_means(training_data)
  ret <- .create_endpoints(training_data, means, type = "percentiles",
                           alpha = 0.10)

  expect_equal(dim(ret$endpoints), c(2,4))
  expect_true(is.data.frame(ret$endpoints))
  expect_true(is.factor(ret$endpoints$cyl))
})

test_that("percentile - All factors", {
  used_variables <- rownames(attr(stats::terms(model_allfactors), "factors"))[-1]
  training_data <- subset(my_mtcars, select = used_variables)
  means <- .create_means(training_data)
  ret <- .create_endpoints(training_data, means, type = "percentiles", alpha = 0.10)

  expect_equal(dim(ret$endpoints), c(2,2))
  expect_true(is.data.frame(ret$endpoints))
  expect_type(ret$endpoints$cyl, "logical")
})

################################################################################

test_that("ranges - No factors", {
  used_variables <- rownames(attr(stats::terms(model_nofactors), "factors"))[-1]
  training_data <- subset(mtcars, select = used_variables)
  means <- .create_means(training_data)
  ret <- .create_endpoints(training_data, means, type = "ranges", alpha = NA)

  expect_equal(dim(ret$endpoints), c(2,3))
  expect_true(is.data.frame(ret$endpoints))
  expect_type(ret$endpoints$cyl, "double")
})

test_that("ranges - Some factors", {
  # one factor
  used_variables <- rownames(attr(stats::terms(model_onefactor), "factors"))[-1]
  training_data <- subset(my_mtcars, select = used_variables)
  means <- .create_means(training_data)
  ret <- .create_endpoints(training_data, means, type = "ranges", alpha = NA)

  expect_equal(dim(ret$endpoints), c(2,3))
  expect_true(is.data.frame(ret$endpoints))
  expect_true(is.factor(ret$endpoints$cyl))

  # two factors
  used_variables <- rownames(attr(stats::terms(model_twofactors), "factors"))[-1]
  training_data <- subset(my_mtcars, select = used_variables)
  means <- .create_means(training_data)
  ret <- .create_endpoints(training_data, means, type = "ranges", alpha = NA)

  expect_equal(dim(ret$endpoints), c(2,4))
  expect_true(is.data.frame(ret$endpoints))
  expect_true(is.factor(ret$endpoints$cyl))
})

test_that("ranges - All factors", {
  used_variables <- rownames(attr(stats::terms(model_allfactors), "factors"))[-1]
  training_data <- subset(my_mtcars, select = used_variables)
  means <- .create_means(training_data)

  ret <- .create_endpoints(training_data, means, type = "ranges", alpha = NA)
  expect_equal(dim(ret$endpoints), c(2,2))
  expect_true(is.data.frame(ret$endpoints))
  expect_type(ret$endpoints$cyl, "logical")
})
