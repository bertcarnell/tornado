context("test-importance_train")

test_that("importance_train works", {
  testthat::skip_if_not_installed("caret")
  testthat::skip_if_not_installed("randomForest")

  model_final <- caret::train(x = subset(mtcars, select = -mpg), y = mtcars$mpg, method = "rf")
  imp <- importance(model_final)
  expect_equal(class(imp), "importance_plot")
  plot(imp)
})

test_that("importance_train works with weights", {
  testthat::skip_if_not_installed("caret")
  testthat::skip_if_not_installed("randomForest")

  w <- sample(1:8, size = nrow(mtcars), replace=TRUE)
  model_final <- caret::train(x = subset(mtcars, select = -mpg), y = mtcars$mpg, method = "rf", weights = w)
  imp <- importance(model_final)
  expect_equal(class(imp), "importance_plot")
  plot(imp)
})
