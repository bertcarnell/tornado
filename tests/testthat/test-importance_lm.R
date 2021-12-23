context("test-importance_lm")

test_that("importance lm works", {
  gtest <- lm(mpg ~ cyl*wt*hp + gear + carb, data = mtcars)
  gtestreduced <- lm(mpg ~ 1, data = mtcars)
  imp <- importance(gtest, gtestreduced)
  expect_equal(class(imp), "importance_plot")
  plot(imp)
  plot(imp, nvar = 3)
  plot(imp, geom_bar_control = list(width = 0.2))

  gtest <- lm(mpg ~ cyl + wt + hp + gear + carb, data = mtcars)
  gtestreduced <- lm(mpg ~ 1, data = mtcars)
  imp <- importance(gtest, gtestreduced)
  expect_equal(class(imp), "importance_plot")
  plot(imp)

  gtest <- lm(mpg ~ cyl*wt*hp + gear + carb, data = mtcars)
  gtestreduced <- lm(mpg ~ 1, data = mtcars)
  dict <- list(Orig.Node.Name = c("cyl", "wt", "hp", "gear", "carb"),
               Description.for.Presentation = c("Cylinders", "Weight", "Horsepower", "Num Gears", "Carbeurated"))
  imp <- importance(gtest, gtestreduced, dict = dict)
  expect_equal(class(imp), "importance_plot")
  plot(imp)

  gtest <- lm(mpg ~ cyl + wt + hp + gear + carb, data = mtcars)
  gtestreduced <- lm(mpg ~ 1, data = mtcars)
  imp <- importance(gtest, gtestreduced)
  expect_equal(class(imp), "importance_plot")
  plot(imp)
})
