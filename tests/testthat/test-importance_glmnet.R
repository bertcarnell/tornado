context("test-importance_glmnet")

test_that("importance glmnet works", {
  testthat::skip_if_not_installed("glmnet")

  form <- formula(mpg ~ cyl*wt*hp)
  mf <- model.frame(form, data = mtcars)
  mm <- model.matrix(mf, mf)
  gtest <- glmnet::cv.glmnet(x = mm, y = mtcars$mpg, family = "gaussian")
  imp <- importance(gtest, mtcars, form, nperm = 100)
  expect_equal(class(imp), "importance_plot")
  plot(imp)
})

test_that("importance glmnet works with weights", {
  testthat::skip_if_not_installed("glmnet")

  form <- formula(mpg ~ cyl*wt*hp)
  mf <- model.frame(form, data = mtcars)
  mm <- model.matrix(mf, mf)
  w <- runif(nrow(mtcars))
  gtest <- glmnet::cv.glmnet(x = mm, y = mtcars$mpg, family = "gaussian", weights = w)
  imp <- importance(gtest, mtcars, form, nperm = 100)
  expect_equal(class(imp), "importance_plot")
  plot(imp)
})
