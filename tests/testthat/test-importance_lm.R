context("test-importance_lm")

test_that("importance lm works", {
  gtest <- lm(mpg ~ cyl*wt*hp + gear + carb, data = mtcars)
  gtestreduced <- lm(mpg ~ 1, data = mtcars)
  g <- importance(gtest, gtestreduced)
  expect_equal(class(g), "importance_plot")
  expect_equal(class(g$plot), c("gtable", "gTree", "grob", "gDesc"))
  plot(g)

  gtest <- lm(mpg ~ cyl + wt + hp + gear + carb, data = mtcars)
  gtestreduced <- lm(mpg ~ 1, data = mtcars)
  g <- importance(gtest, gtestreduced)
  expect_equal(class(g), "importance_plot")
  expect_equal(class(g$plot), c("gtable", "gTree", "grob", "gDesc"))
  plot(g)

  gtest <- lm(mpg ~ cyl*wt*hp + gear + carb, data = mtcars)
  gtestreduced <- lm(mpg ~ 1, data = mtcars)
  dict <- list(Orig.Node.Name = c("cyl", "wt", "hp", "gear", "carb"),
               Description.for.Presentation = c("Cylinders", "Weight", "Horsepower", "Num Gears", "Carbeurated"))
  g <- importance(gtest, gtestreduced, dict = dict)
  expect_equal(class(g), "importance_plot")
  expect_equal(class(g$plot), c("gtable", "gTree", "grob", "gDesc"))
  plot(g)

  gtest <- lm(mpg ~ cyl + wt + hp + gear + carb, data = mtcars)
  gtestreduced <- lm(mpg ~ 1, data = mtcars)
  g <- importance(gtest, gtestreduced)
  expect_equal(class(g), "importance_plot")
  expect_equal(class(g$plot), c("gtable", "gTree", "grob", "gDesc"))
  plot(g)
})
