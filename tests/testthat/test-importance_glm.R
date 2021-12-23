context("test-importance_glm")

test_that("importance glm works", {
  gtest <- glm(mpg ~ cyl*wt*hp + gear + carb, data = mtcars, family = gaussian)
  gtestreduced <- glm(mpg ~ 1, data = mtcars, family = gaussian)
  imp <- importance(gtest, gtestreduced)
  expect_equal(class(imp), "importance_plot")
  plot(imp)

  gtest <- glm(mpg ~ cyl + wt + hp + gear + carb, data = mtcars, family = gaussian)
  gtestreduced <- glm(mpg ~ 1, data = mtcars, family = gaussian)
  imp <- importance(gtest, gtestreduced)
  expect_equal(class(imp), "importance_plot")
  plot(imp)

  gtest <- glm(vs ~ wt + disp + gear, data = mtcars, family = binomial(link = "logit"))
  gtestreduced <- glm(vs ~ 1, data = mtcars, family = binomial(link = "logit"))
  imp <- importance(gtest, gtestreduced)
  expect_equal(class(imp), "importance_plot")
  plot(imp)

  gtest <- glm(breaks ~ wool + tension, data = warpbreaks, family = poisson(link = "log"))
  gtestreduced <- glm(breaks ~ 1, data = warpbreaks, family = poisson(link = "log"))
  imp <- importance(gtest, gtestreduced)
  expect_equal(class(imp), "importance_plot")
  plot(imp)
})
