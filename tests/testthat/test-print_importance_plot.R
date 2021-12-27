context("test-print_importance_plot")

test_that("printing importance works", {
  gtest <- lm(mpg ~ cyl*wt*hp, data = mtcars)
  gtest0 <- lm(mpg ~ 1, data = mtcars)
  imp <- importance(gtest, gtest0)
  expect_output(print(imp), regexp = "Importance")
})
