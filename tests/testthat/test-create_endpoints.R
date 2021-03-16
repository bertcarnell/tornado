# Copyright 2021 Robert Carnell

setup_percent_change <- function()
{
  assign("type", "PercentChange", envir = parent.frame())
  assign("alpha", 0.10, envir = parent.frame())
  assign("alt.order", NA, envir = parent.frame())
  assign("dict", NA, envir = parent.frame())
  assign("model_data", mtcars, envir = parent.frame())
}

setup_percentile <- function()
{
  assign("type", "percentiles", envir = parent.frame())
  assign("alpha", 0.10, envir = parent.frame())
  assign("alt.order", NA, envir = parent.frame())
  assign("dict", NA, envir = parent.frame())
  assign("model_data", mtcars, envir = parent.frame())
}

setup_ranges <- function()
{
  assign("type", "ranges", envir = parent.frame())
  assign("alpha", NA, envir = parent.frame())
  assign("alt.order", NA, envir = parent.frame())
  assign("dict", NA, envir = parent.frame())
  assign("model_data", mtcars, envir = parent.frame())
}

test_that("PercentChange - No factors", {
  setup_percent_change()

  model <- lm(mpg ~ cyl*wt*hp, data = model_data)
  used_variables <- rownames(attr(stats::terms(model), "factors"))[-1]
  training_data <- subset(model_data, select = used_variables)
  means <- .create_means(training_data)
  ret <- .create_endpoints(training_data, means, type = type, alpha = alpha)

  expect_equal(dim(ret$endpoints), c(2,3))
  expect_true(is.data.frame(ret$endpoints))
  expect_type(ret$endpoints$cyl, "double")
})

test_that("PercentChange - Some factors", {
  setup_percent_change()

  # one factor
  model_data$cyl <- factor(model_data$cyl)
  model <- lm(mpg ~ cyl*wt*hp, data = model_data)
  used_variables <- rownames(attr(stats::terms(model), "factors"))[-1]
  training_data <- subset(model_data, select = used_variables)
  means <- .create_means(training_data)
  ret <- .create_endpoints(training_data, means, type = type, alpha = alpha)
  expect_equal(dim(ret$endpoints), c(2,3))
  expect_true(is.data.frame(ret$endpoints))
  expect_true(is.factor(ret$endpoints$cyl))

  # two factors
  setup_percent_change()

  model_data$cyl <- factor(model_data$cyl)
  model_data$vs <- factor(model_data$vs, labels = c("v","s"))
  model <- lm(mpg ~ cyl*wt*hp + vs, data = model_data)
  used_variables <- rownames(attr(stats::terms(model), "factors"))[-1]
  training_data <- subset(model_data, select = used_variables)
  means <- .create_means(training_data)
  ret <- .create_endpoints(training_data, means, type = type, alpha = alpha)

  expect_equal(dim(ret$endpoints), c(2,4))
  expect_true(is.data.frame(ret$endpoints))
  expect_true(is.factor(ret$endpoints$cyl))
})

test_that("PercentChange - All factors", {
  setup_percent_change()

  model_data$cyl <- factor(model_data$cyl)
  model_data$vs <- factor(model_data$vs, labels = c("v","s"))
  model <- lm(mpg ~ cyl*vs, data = model_data)
  used_variables <- rownames(attr(stats::terms(model), "factors"))[-1]
  training_data <- subset(model_data, select = used_variables)
  means <- .create_means(training_data)
  ret <- .create_endpoints(training_data, means, type = type, alpha = alpha)

  expect_equal(dim(ret$endpoints), c(2,2))
  expect_true(is.data.frame(ret$endpoints))
  expect_type(ret$endpoints$cyl, "logical")
})

################################################################################

test_that("percentile - No factors", {
  setup_percentile()

  model <- lm(mpg ~ cyl*wt*hp, data = model_data)
  used_variables <- rownames(attr(stats::terms(model), "factors"))[-1]
  training_data <- subset(model_data, select = used_variables)
  means <- .create_means(training_data)
  ret <- .create_endpoints(training_data, means, type = type, alpha = alpha)

  expect_equal(dim(ret$endpoints), c(2,3))
  expect_true(is.data.frame(ret$endpoints))
  expect_type(ret$endpoints$cyl, "double")
})

test_that("percentile - Some factors", {
  setup_percentile()

  # one factor
  model_data$cyl <- factor(model_data$cyl)
  model <- lm(mpg ~ cyl*wt*hp, data = model_data)
  used_variables <- rownames(attr(stats::terms(model), "factors"))[-1]
  training_data <- subset(model_data, select = used_variables)
  means <- .create_means(training_data)
  ret <- .create_endpoints(training_data, means, type = type, alpha = alpha)

  expect_equal(dim(ret$endpoints), c(2,3))
  expect_true(is.data.frame(ret$endpoints))
  expect_true(is.factor(ret$endpoints$cyl))

  # two factors
  setup_percentile()

  model_data$cyl <- factor(model_data$cyl)
  model_data$vs <- factor(model_data$vs, labels = c("v","s"))
  model <- lm(mpg ~ cyl*wt*hp + vs, data = model_data)
  used_variables <- rownames(attr(stats::terms(model), "factors"))[-1]
  training_data <- subset(model_data, select = used_variables)
  means <- .create_means(training_data)
  ret <- .create_endpoints(training_data, means, type = type, alpha = alpha)

  expect_equal(dim(ret$endpoints), c(2,4))
  expect_true(is.data.frame(ret$endpoints))
  expect_true(is.factor(ret$endpoints$cyl))
})

test_that("percentile - All factors", {
  setup_percentile()

  model_data$cyl <- factor(model_data$cyl)
  model_data$vs <- factor(model_data$vs, labels = c("v","s"))
  model <- lm(mpg ~ cyl*vs, data = model_data)
  used_variables <- rownames(attr(stats::terms(model), "factors"))[-1]
  training_data <- subset(model_data, select = used_variables)
  means <- .create_means(training_data)
  ret <- .create_endpoints(training_data, means, type = type, alpha = alpha)

  expect_equal(dim(ret$endpoints), c(2,2))
  expect_true(is.data.frame(ret$endpoints))
  expect_type(ret$endpoints$cyl, "logical")
})

################################################################################

test_that("ranges - No factors", {
  setup_ranges()

  model <- lm(mpg ~ cyl*wt*hp, data = model_data)
  used_variables <- rownames(attr(stats::terms(model), "factors"))[-1]
  training_data <- subset(model_data, select = used_variables)
  means <- .create_means(training_data)
  ret <- .create_endpoints(training_data, means, type = type, alpha = alpha)

  expect_equal(dim(ret$endpoints), c(2,3))
  expect_true(is.data.frame(ret$endpoints))
  expect_type(ret$endpoints$cyl, "double")
})

test_that("ranges - Some factors", {
  setup_ranges()

  # one factor
  model_data$cyl <- factor(model_data$cyl)
  model <- lm(mpg ~ cyl*wt*hp, data = model_data)
  used_variables <- rownames(attr(stats::terms(model), "factors"))[-1]
  training_data <- subset(model_data, select = used_variables)
  means <- .create_means(training_data)
  ret <- .create_endpoints(training_data, means, type = type, alpha = alpha)

  expect_equal(dim(ret$endpoints), c(2,3))
  expect_true(is.data.frame(ret$endpoints))
  expect_true(is.factor(ret$endpoints$cyl))

  # two factors
  setup_ranges()

  model_data$cyl <- factor(model_data$cyl)
  model_data$vs <- factor(model_data$vs, labels = c("v","s"))
  model <- lm(mpg ~ cyl*wt*hp + vs, data = model_data)
  used_variables <- rownames(attr(stats::terms(model), "factors"))[-1]
  training_data <- subset(model_data, select = used_variables)
  means <- .create_means(training_data)
  ret <- .create_endpoints(training_data, means, type = type, alpha = alpha)

  expect_equal(dim(ret$endpoints), c(2,4))
  expect_true(is.data.frame(ret$endpoints))
  expect_true(is.factor(ret$endpoints$cyl))
})

test_that("ranges - All factors", {
  setup_ranges()

  model_data$cyl <- factor(model_data$cyl)
  model_data$vs <- factor(model_data$vs, labels = c("v","s"))
  model <- lm(mpg ~ cyl*vs, data = model_data)
  used_variables <- rownames(attr(stats::terms(model), "factors"))[-1]
  training_data <- subset(model_data, select = used_variables)
  means <- .create_means(training_data)

  ret <- .create_endpoints(training_data, means, type = type, alpha = alpha)
  expect_equal(dim(ret$endpoints), c(2,2))
  expect_true(is.data.frame(ret$endpoints))
  expect_type(ret$endpoints$cyl, "logical")
})
