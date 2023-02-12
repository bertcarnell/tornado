context("test-tornado_glmnet")

test_that("tornado glmnet works", {
  testthat::skip_if_not_installed("glmnet")

  torn <- tornado(glmnet_model, modeldata = mtcars, form = glmnet_form, s = "lambda.1se",
                  type = "PercentChange", alpha = 0.10)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test: glmnet")
  plot(g)
})

test_that("tornado glmnet works with weights", {
  testthat::skip_if_not_installed("glmnet")

  torn <- tornado(glmnet_model_weighted, modeldata = mtcars, form = glmnet_form, s = "lambda.1se",
                  type = "PercentChange", alpha = 0.10)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test: glmnet with weights")
  plot(g)
})
