context("test-importance_glmnet")

test_that("importance glmnet works", {
  form <- formula(mpg ~ cyl*wt*hp)
  mf <- model.frame(form, data = mtcars)
  mm <- model.matrix(mf, mf)
  gtest <- glmnet::cv.glmnet(x = mm, y = mtcars$mpg, family = "gaussian")
  g <- importance(gtest, mtcars, form, nperm = 100)
  expect_equal(class(g), c("gg","ggplot"))
  g <- g + ggtitle("Imporance glmnet")
  plot(g)
})
