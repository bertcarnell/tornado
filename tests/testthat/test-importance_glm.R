context("test-importance_glm")

test_that("importance glm works", {
  gtest <- glm(mpg ~ cyl*wt*hp + gear + carb, data = mtcars, family = gaussian)
  gtestreduced <- glm(mpg ~ 1, data = mtcars, family = gaussian)
  g <- importance(gtest, gtestreduced)
  expect_equal(class(g), "importance_plot")
  expect_equal(class(g$plot), c("gtable", "gTree", "grob", "gDesc"))

  gtest <- glm(mpg ~ cyl + wt + hp + gear + carb, data = mtcars, family = gaussian)
  gtestreduced <- glm(mpg ~ 1, data = mtcars, family = gaussian)
  g <- importance(gtest, gtestreduced)
  expect_equal(class(g), "importance_plot")
  expect_equal(class(g$plot), c("gtable", "gTree", "grob", "gDesc"))

  gtest <- glm(vs ~ wt + disp + gear, data = mtcars, family = binomial(link = "logit"))
  gtestreduced <- glm(vs ~ 1, data = mtcars, family = binomial(link = "logit"))
  g <- importance(gtest, gtestreduced)
  expect_equal(class(g), "importance_plot")
  expect_equal(class(g$plot), c("gtable", "gTree", "grob", "gDesc"))

  gtest <- glm(breaks ~ wool + tension, data = warpbreaks, family = poisson(link = "log"))
  gtestreduced <- glm(breaks ~ 1, data = warpbreaks, family = poisson(link = "log"))
  g <- importance(gtest, gtestreduced)
  expect_equal(class(g), "importance_plot")
  expect_equal(class(g$plot), c("gtable", "gTree", "grob", "gDesc"))

})
