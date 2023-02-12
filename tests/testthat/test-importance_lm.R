context("test-importance_lm")

test_that("importance lm works", {
  model1 <- lm(mpg ~ cyl*wt*hp + gear + carb, data = mtcars)
  model2 <- lm(mpg ~ cyl + wt + hp + gear + carb, data = mtcars)
  model_reduced <- lm(mpg ~ 1, data = mtcars)

  imp <- importance(model1, model_reduced)
  expect_equal(class(imp), "importance_plot")
  plot(imp)
  plot(imp, nvar = 3)
  plot(imp, geom_bar_control = list(width = 0.2))

  imp <- importance(model2, model_reduced)
  expect_equal(class(imp), "importance_plot")
  plot(imp)

  dict <- list(old = c("cyl", "wt", "hp", "gear", "carb"),
               new = c("Cylinders", "Weight", "Horsepower", "Num Gears", "Carbeurated"))
  imp <- importance(model1, model_reduced, dict = dict)
  expect_equal(class(imp), "importance_plot")
  plot(imp)

  imp <- importance(model2, model_reduced)
  expect_equal(class(imp), "importance_plot")
  plot(imp)
})

test_that("importance lm works with weights", {
  model1 <- lm(mpg ~ cyl*wt*hp + gear + carb, data = mtcars, weights = rep(1:2, nrow(mtcars) / 2))
  model_reduced <- lm(mpg ~ 1, data = mtcars, weights = rep(1:2, nrow(mtcars) / 2))

  imp <- importance(model1, model_reduced)
  expect_equal(class(imp), "importance_plot")
  plot(imp)
})
