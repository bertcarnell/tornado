context("test-tornado_glm")

test_that("plotting gaussian glm works", {
  gtest <- glm(mpg ~ cyl*wt*hp, data = mtcars, family = gaussian)
  g <- tornado(gtest, type = "PercentChange", alpha = 0.10, xlabel = "MPG")
  expect_equal(class(g), "tornado_plot")
  expect_equal(class(g$plot), c("gg","ggplot"))
  g$plot <- g$plot + ggtitle("Test: Gaussian GLM PercentChange")
  plot(g)

  g <- tornado(gtest, type = "percentiles", alpha = 0.05, xlabel = "MPG")
  expect_equal(class(g$plot), c("gg","ggplot"))
  g$plot <- g$plot + ggtitle("Test: Gaussian GLM percentiles")
  plot(g)

  g <- tornado(gtest, type = "ranges", xlabel = "MPG")
  expect_equal(class(g$plot), c("gg","ggplot"))
  g$plot <- g$plot + ggtitle("Test: Gaussian GLM ranges")
  plot(g)

  mydat <- mtcars
  mydat$cyl <- factor(mydat$cyl)
  mydat$vs <- factor(mydat$vs)
  gtest <- glm(mpg ~ cyl*wt*hp + vs, data = mydat, family = gaussian)
  g <- tornado(gtest, type = "PercentChange", alpha = 0.10, xlabel = "MPG")
  g$plot <- g$plot + ggtitle("Test:  Gaussian GLM PercentChange with 2 factors")
  plot(g)

  g <- tornado(gtest, type = "ranges", alpha = NA, xlabel = "MPG")
  expect_equal(class(g$plot), c("gg","ggplot"))
  g$plot <- g$plot + ggtitle("Test:  Gaussian GLM ranges with dictionary")
  plot(g)

  g <- tornado(gtest, type = "percentiles", alpha = 0.1, xlabel = "MPG")
  expect_equal(class(g$plot), c("gg","ggplot"))
  g$plot <- g$plot + ggtitle("Test:  Gaussian GLM percentiles with dictionary")
  plot(g)
})

test_that("plotting binomial glm works", {
  gtest <- glm(vs ~ wt + disp + cyl, data = mtcars, family = binomial(link = "logit"))
  g <- tornado(gtest, type = "PercentChange", alpha = 0.10, xlabel = "MPG")
  expect_equal(class(g$plot), c("gg","ggplot"))
  g$plot <- g$plot + ggtitle("Test: Binomial GLM PercentChange")
  plot(g)

  g <- tornado(gtest, type = "percentiles", alpha = 0.05, xlabel = "MPG")
  expect_equal(class(g$plot), c("gg","ggplot"))
  g$plot <- g$plot + ggtitle("Test: Binomial GLM percentiles")
  plot(g)

  g <- tornado(gtest, type = "ranges", xlabel = "MPG")
  expect_equal(class(g$plot), c("gg","ggplot"))
  g$plot <- g$plot + ggtitle("Test: Binomial GLM ranges")
  plot(g)
})
