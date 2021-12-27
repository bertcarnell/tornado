context("test-print_tornado_plot")

test_that("printing Tornado works", {
  gtest <- lm(mpg ~ cyl*wt*hp, data = mtcars)
  torn <- tornado(gtest, type = "PercentChange", alpha = 0.10)
  expect_output(print(torn), regexp = "Tornado")
})
