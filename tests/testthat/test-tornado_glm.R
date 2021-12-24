context("test-tornado_glm")

test_that("plotting gaussian glm works", {
  gtest <- glm(mpg ~ cyl*wt*hp, data = mtcars, family = gaussian)
  torn <- tornado(gtest, type = "PercentChange", alpha = 0.10)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test: Gaussian GLM PercentChange")
  plot(g)

  torn <- tornado(gtest, type = "percentiles", alpha = 0.05)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test: Gaussian GLM percentiles")
  plot(g)

  torn <- tornado(gtest, type = "ranges")
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test: Gaussian GLM ranges")
  plot(g)

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
  g <- g + ggtitle("Test:  Gaussian GLM ranges with dictionary")
  plot(g)

  torn <- tornado(gtest, type = "percentiles", alpha = 0.1)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test:  Gaussian GLM percentiles with dictionary")
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
  gtest <- glm(vs ~ wt + disp + cyl, data = mtcars, family = binomial(link = "logit"))
  torn <- tornado(gtest, type = "PercentChange", alpha = 0.10)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "VS")
  g <- g + ggtitle("Test: Binomial GLM PercentChange")
  plot(g)

  g <- tornado(gtest, type = "percentiles", alpha = 0.05)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test: Binomial GLM percentiles")
  plot(g)

  g <- tornado(gtest, type = "ranges")
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test: Binomial GLM ranges")
  plot(g)
})
