context("test-plot_importance_plot")

test_that("plotting works for base packages", {
  gtest <- lm(mpg ~ cyl + wt + hp + gear + carb, data = mtcars)
  gtestreduced <- lm(mpg ~ 1, data = mtcars)
  imp <- importance(gtest, gtestreduced)
  expect_equal(imp$type, "lm")
  plot(imp)
  plot(imp, nvar = 3)
  g <- plot(imp, plot = FALSE)
  # test there is a warning for plotting with fill
  expect_warning(g <- plot(imp, plot = FALSE, geom_bar_control = list(col = "black", fill = "red")))
  expect_silent(g <- plot(imp, plot = FALSE))
  expect_warning(g <- plot(imp, plot = FALSE, geom_bar_control = list(fill = "red")))

  gtest <- glm(mpg ~ cyl*wt*hp + gear + carb, data=mtcars, family=gaussian)
  gtestreduced <- glm(mpg ~ 1, data=mtcars, family=gaussian)
  imp <- importance(gtest, gtestreduced)
  expect_equal(imp$type, "glm")
  plot(imp)
  plot(imp, nvar = 3)
  # test there is an error if the type is changed
  imp2 <- imp
  imp2$type <- "blah"
  expect_error(g <- plot(imp2, plot = FALSE))


  gtest <- survival::survreg(survival::Surv(futime, fustat) ~ ecog.ps*rx + age,
                             data = survival::ovarian,
                             dist = "weibull")
  imp <- importance(gtest, survival::ovarian, nperm = n_permutation_tests)
  expect_equal(imp$type, "survreg")
  plot(imp)

  g <- plot(imp, plot = FALSE)
  g <- g + ggtitle("Testing Survival importance title")
  plot(g)

  plot(imp, nvar = 2)
})

test_that("plotting works for glmnet", {
  testthat::skip_if_not_installed("glmnet")

  form <- formula(mpg ~ cyl*wt*hp + vs + am)
  mf <- model.frame(form, data = mtcars)
  mm <- model.matrix(mf, mf)
  gtest <- glmnet::cv.glmnet(x = mm, y = mtcars$mpg, family = "gaussian")
  imp <- importance(gtest, mtcars, form, nperm = n_permutation_tests)
  expect_equal(imp$type, "cv.glmnet")
  plot(imp)

  plot(imp, nvar = 3)

  g <- plot(imp, plot = FALSE)
})

test_that("plotting works for caret::train", {
  testthat::skip_if_not_installed("caret")
  testthat::skip_if_not_installed("randomForest")

  model_final <- caret::train(x = subset(mtcars, select = -mpg), y = mtcars$mpg, method = "rf")
  imp <- importance(model_final)
  expect_equal(imp$type, "train")
  plot(imp)

  plot(imp, nvar = 3)

  g <- plot(imp, plot = FALSE)
})
