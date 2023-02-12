context("test-importance_glm")

test_that("importance glm works", {
  # Interactions
  gtestreduced <- glm(mpg ~ 1, data = mtcars, family = gaussian)
  imp1 <- importance(base_glm_model, gtestreduced)
  expect_equal(class(imp1), "importance_plot")
  plot(imp1)

  # no Interactions
  gtest <- glm(mpg ~ cyl + wt + hp + gear + carb, data = mtcars, family = gaussian)
  gtestreduced <- glm(mpg ~ 1, data = mtcars, family = gaussian)
  imp2 <- importance(gtest, gtestreduced)
  expect_equal(class(imp2), "importance_plot")
  plot(imp2)

  # binomial
  gtestreduced <- glm(vs ~ 1, data = mtcars, family = binomial(link = "logit"))
  imp3 <- importance(base_glm_binomial_model, gtestreduced)
  expect_equal(class(imp3), "importance_plot")
  plot(imp3)

  # Poisson
  gtest <- glm(breaks ~ wool + tension, data = warpbreaks, family = poisson(link = "log"))
  gtestreduced <- glm(breaks ~ 1, data = warpbreaks, family = poisson(link = "log"))
  imp4 <- importance(gtest, gtestreduced)
  expect_equal(class(imp4), "importance_plot")
  plot(imp4)
})

test_that("importance glm works with weights", {
  gtestreduced <- glm(vs ~ 1, data = mtcars, family = binomial(link = "logit"), weights = rep(1:2, nrow(mtcars) / 2))
  imp5 <- importance(weighted_glm_binomial_model, gtestreduced)
  expect_equal(class(imp5), "importance_plot")
  plot(imp5)

  gtestreduced <- glm(mpg ~ 1, data = mtcars, family = gaussian, weights = rep(1:2, nrow(mtcars) / 2))
  imp6 <- importance(weigthed_glm_model, gtestreduced)
  expect_equal(class(imp6), "importance_plot")
  plot(imp6)
})

