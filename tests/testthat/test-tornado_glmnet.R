context("test-tornado_glmnet")

test_that("tornado glmnet works", {
  mf <- model.frame(mpg ~ cyl*wt*hp, data = mtcars)
  mm <- model.matrix(mf, mf)
  gtest <- cv.glmnet(x = mm, y = mtcars$mpg, family = "gaussian")
  g <- tornado(gtest, mtcars, formula(mpg ~ cyl*wt*hp), s = "lambda.1se",
               type = "PercentChange", alpha = 0.10, xlabel = "MPG")
  expect_equal(class(g), c("gg","ggplot"))
})
