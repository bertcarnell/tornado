context("test-plot_tornado_plot")

test_that("tornado plot warning", {
  gtest <- lm(mpg ~ cyl*wt*hp, data = mtcars)
  torn <- tornado(gtest, type = "PercentChange", alpha = 0.10)
  expect_warning(g <- plot(torn, plot = FALSE, xlabel = "MPG", geom_bar_control = list(fill = "red")))
})
