context("test-importance_glm")

test_that("importance glm works", {
  gtest <- glm(mpg ~ cyl*wt*hp + gear + carb, data = mtcars, family = gaussian)
  gtestreduced <- glm(mpg ~ 1, data = mtcars, family = gaussian)
  imp1 <- importance(gtest, gtestreduced)
  expect_equal(class(imp1), "importance_plot")
  plot(imp1)

  gtest <- glm(mpg ~ cyl + wt + hp + gear + carb, data = mtcars, family = gaussian)
  gtestreduced <- glm(mpg ~ 1, data = mtcars, family = gaussian)
  imp2 <- importance(gtest, gtestreduced)
  expect_equal(class(imp2), "importance_plot")
  plot(imp2)

  gtest <- glm(vs ~ wt + disp + gear, data = mtcars, family = binomial(link = "logit"))
  gtestreduced <- glm(vs ~ 1, data = mtcars, family = binomial(link = "logit"))
  imp3 <- importance(gtest, gtestreduced)
  expect_equal(class(imp3), "importance_plot")
  plot(imp3)

  gtest <- glm(breaks ~ wool + tension, data = warpbreaks, family = poisson(link = "log"))
  gtestreduced <- glm(breaks ~ 1, data = warpbreaks, family = poisson(link = "log"))
  imp4 <- importance(gtest, gtestreduced)
  expect_equal(class(imp4), "importance_plot")
  plot(imp4)
})

test_that("importance glm works with weights", {
  gtest <- glm(vs ~ disp, data = mtcars, family = binomial(link = "logit"), weights=mtcars$gear)
  gtestreduced <- glm(vs ~ 1, data = mtcars, family = binomial(link = "logit"), weights=mtcars$gear)
  imp5 <- importance(gtest, gtestreduced)
  expect_equal(class(imp5), "importance_plot")
  plot(imp5)
})

