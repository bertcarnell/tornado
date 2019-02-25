context("test-importance_lm")

test_that("importance lm works", {
  gtest <- lm(mpg ~ cyl*wt*hp + gear + carb, data = mtcars)
  gtestreduced <- lm(mpg ~ 1, data = mtcars)
  g <- importance(gtest, gtestreduced)
  expect_equal(class(g), "importance_plot")
  expect_equal(class(g$plot), c("gtable", "gTree", "grob", "gDesc"))

  gtest <- lm(mpg ~ cyl + wt + hp + gear + carb, data = mtcars)
  gtestreduced <- lm(mpg ~ 1, data = mtcars)
  g <- importance(gtest, gtestreduced)
  expect_equal(class(g), "importance_plot")
  expect_equal(class(g$plot), c("gtable", "gTree", "grob", "gDesc"))
})
