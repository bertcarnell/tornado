context("test-tornado_train")

test_that("tornado caret::train works", {
  gtest <- caret::train(x = subset(mtcars, select = -mpg), y = mtcars$mpg, method = "rf")
  g <- tornado(gtest, type = "PercentChange", alpha = 0.10, xlabel = "MPG")
  expect_equal(class(g), c("gg","ggplot"))
  g <- g + ggtitle("Test: caret::train randomforest")
  plot(g)

  mydat <- mtcars
  mydat$cyl <- factor(mydat$cyl)
  mydat$vs <- factor(mydat$vs)
  gtest <- caret::train(x = subset(mtcars, select = -mpg), y = mtcars$mpg, method = "rf")
  g <- tornado(gtest, type = "PercentChange", alpha = 0.10, xlabel = "MPG")
  g <- g + ggtitle("Test:  caret::train randomforest PercentChange with 2 factors")
  plot(g)

  gtest <- caret::train(x = subset(mtcars, select = -mpg), y = mtcars$mpg, method = "glmStepAIC", trace = 0)
  g <- tornado(gtest, type = "PercentChange", alpha = 0.10, xlabel = "MPG")
  expect_equal(class(g), c("gg","ggplot"))
  g <- g + ggtitle("Test: caret::train glmStepAIC")
  plot(g)
})
