context("test-tornado_lm")

test_that("linear model tornado works", {
  gtest <- lm(mpg ~ cyl*wt*hp, data = mtcars)
  g <- tornado(gtest, type = "PercentChange", alpha = 0.10, xlabel = "MPG")
  expect_equal(class(g), c("gg","ggplot"))
  g <- tornado(gtest, type = "percentiles", alpha = 0.05, xlabel = "MPG")
  expect_equal(class(g), c("gg","ggplot"))
  g <- tornado(gtest, type = "ranges", xlabel = "MPG")
  expect_equal(class(g), c("gg","ggplot"))
})
