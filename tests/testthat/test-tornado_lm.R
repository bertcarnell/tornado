context("test-tornado_lm")

test_that("linear model tornado works", {
  gtest <- lm(mpg ~ cyl*wt*hp, data = mtcars)
  torn <- tornado(gtest, type = "PercentChange", alpha = 0.10)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test:  Linear model PercentChange")
  plot(g)

  torn <- tornado(gtest, type = "percentiles", alpha = 0.05)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test:  Linear model percentiles")
  plot(g)

  g <- tornado(gtest, type = "ranges")
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test:  Linear model ranges")
  plot(g)

  gtest <- lm(mpg ~ cyl*wt*hp, data = mtcars)
  dict <- list(Orig.Node.Name = c("cyl", "wt", "hp"),
               Description.for.Presentation = c("Cylinders", "Weight", "Horsepower"))
  torn <- tornado(gtest, type = "PercentChange", alpha = 0.10, dict = dict)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test:  Linear model PercentChange with dictionary")
  plot(g)

  # test a model with two factors
  mydat <- mtcars
  mydat$cyl <- factor(mydat$cyl)
  mydat$vs <- factor(mydat$vs)
  gtest <- lm(mpg ~ cyl + wt + hp + vs, data = mydat)
  torn <- tornado(gtest, type = "PercentChange", alpha = 0.10)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test:  Linear model PercentChange with 2 factors")
  plot(g)

  torn <- tornado(gtest, type = "ranges", alpha = NA)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test:  Linear model ranges with dictionary")
  plot(g)

  torn <- tornado(gtest, type = "percentiles", alpha = 0.1)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test:  Linear model percentiles with dictionary")
  plot(g)

  # test a variable with one factor
  mydat <- mtcars
  mydat$cyl <- factor(mydat$cyl)
  gtest <- lm(mpg ~ cyl + wt + hp, data = mydat)
  torn <- tornado(gtest, type = "PercentChange", alpha = 0.10)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test:  Linear model PercentChange with one factor")
  plot(g)

  torn <- tornado(gtest, type = "ranges", alpha = NA)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test:  Linear model ranges with one factor")
  plot(g)

  torn <- tornado(gtest, type = "percentiles", alpha = 0.1)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test:  Linear model percentiles with one factor")
  plot(g)
})
