context("test-importance_glmnet")

test_that("importance glmnet works", {
  testthat::skip_if_not_installed("glmnet")

  imp <- importance(glmnet_model, mtcars, glmnet_form, nperm = 25)
  expect_equal(class(imp), "importance_plot")
  plot(imp)
})

test_that("importance glmnet works with weights", {
  testthat::skip_if_not_installed("glmnet")

  imp <- importance(glmnet_model_weighted, mtcars, glmnet_form, nperm = 25)
  expect_equal(class(imp), "importance_plot")
  plot(imp)
})
