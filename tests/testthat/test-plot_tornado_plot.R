context("test-plot_tornado_plot")

test_that("tornado plot warning", {
  torn <- tornado(model_numeric, type = "PercentChange", alpha = 0.10)
  expect_warning(g <- plot(torn, plot = FALSE, xlabel = "MPG", geom_bar_control = list(fill = "red")))
})

test_that("a theme can be changed from the default", {
  torn <- tornado(model_numeric, type = "PercentChange", alpha = 0.10)
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Dark theme over the default")
  g <- g + theme_dark()
  expect_true("ggplot" %in% class(g))
  plot(g)
})
