context("test-tornado_glm")

test_that("plotting gaussian glm works", {
  gtest <- glm(mpg ~ cyl*wt*hp, data = mtcars, family = gaussian)
  g <- tornado(gtest, type = "PercentChange", alpha = 0.10, xlabel = "MPG")
  expect_equal(class(g), c("gg","ggplot"))
  g <- g + ggtitle("Test: Gaussian GLM PercentChange")
  plot(g)

  g <- tornado(gtest, type = "percentiles", alpha = 0.05, xlabel = "MPG")
  expect_equal(class(g), c("gg","ggplot"))
  g <- g + ggtitle("Test: Gaussian GLM percentiles")
  plot(g)

  g <- tornado(gtest, type = "ranges", xlabel = "MPG")
  expect_equal(class(g), c("gg","ggplot"))
  g <- g + ggtitle("Test: Gaussian GLM ranges")
  plot(g)
})

test_that("plotting binomial glm works", {
  gtest <- glm(vs ~ wt + disp + cyl, data = mtcars, family = binomial(link = "logit"))
  g <- tornado(gtest, type = "PercentChange", alpha = 0.10, xlabel = "MPG")
  expect_equal(class(g), c("gg","ggplot"))
  g <- g + ggtitle("Test: Binomial GLM PercentChange")
  plot(g)

  g <- tornado(gtest, type = "percentiles", alpha = 0.05, xlabel = "MPG")
  expect_equal(class(g), c("gg","ggplot"))
  g <- g + ggtitle("Test: Binomial GLM percentiles")
  plot(g)

  g <- tornado(gtest, type = "ranges", xlabel = "MPG")
  expect_equal(class(g), c("gg","ggplot"))
  g <- g + ggtitle("Test: Binomial GLM ranges")
  plot(g)
})
