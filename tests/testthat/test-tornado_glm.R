context("test-tornado_glm")

test_that("plotting gaussian glm works", {
  torn <- tornado(base_glm_model, type = "PercentChange", alpha = 0.10)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test: Gaussian GLM PercentChange")
  plot(g)

  torn <- tornado(base_glm_model, type = "percentiles", alpha = 0.05)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test: Gaussian GLM percentiles")
  plot(g)

  torn <- tornado(base_glm_model, type = "ranges")
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test: Gaussian GLM ranges")
  plot(g)

  # factor variables
  mydat <- mtcars
  mydat$cyl <- factor(mydat$cyl)
  mydat$vs <- factor(mydat$vs)
  gtest <- glm(mpg ~ cyl*wt*hp + vs, data = mydat, family = gaussian)
  torn <- tornado(gtest, type = "PercentChange", alpha = 0.10)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test:  Gaussian GLM PercentChange with 2 factors")
  plot(g)

  torn <- tornado(gtest, type = "ranges", alpha = NA)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test:  Gaussian GLM ranges")
  plot(g)

  torn <- tornado(gtest, type = "percentiles", alpha = 0.1)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test:  Gaussian GLM percentiles")
  plot(g)

  # regression test.  This used to fail without "fill" in the geom_point_control
  torn <- tornado(gtest, type = "percentiles", alpha = 0.10)
  expect_equal(class(torn), "tornado_plot")
  expect_warning({
    plot(torn, xlabel = "MPG",
       geom_bar_control = list(width = 0.5),
       sensitivity_colors = c("#E41A1C", "#377EB8"),
       geom_point_control = list(col = "red"))
  })
})

test_that("plotting binomial glm works", {
  torn <- tornado(base_glm_binomial_model, type = "PercentChange", alpha = 0.10)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "VS")
  g <- g + ggtitle("Test: Binomial GLM PercentChange")
  plot(g)

  g <- tornado(base_glm_binomial_model, type = "percentiles", alpha = 0.05)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test: Binomial GLM percentiles")
  plot(g)

  g <- tornado(base_glm_binomial_model, type = "ranges")
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test: Binomial GLM ranges")
  plot(g)
})

test_that("glm tornado works with weighted models", {
  torn <- tornado(weigthed_glm_model, type = "PercentChange", alpha = 0.10)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test: Gaussian GLM PercentChange with Weights")
  plot(g)

  torn <- tornado(weighted_glm_binomial_model, type = "PercentChange", alpha = 0.10)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "VS")
  g <- g + ggtitle("Test: Binomial GLM PercentChange with weights")
  plot(g)
})
