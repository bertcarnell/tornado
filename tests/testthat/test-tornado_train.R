context("test-tornado_train")

test_that("tornado caret::train works", {
  testthat::skip_if_not_installed("caret")
  testthat::skip_if_not_installed("MASS")
  testthat::skip_if_not_installed("randomForest")

  # cyl and vs are numeric
  #   cyl and vs are not plotted on the tornado and not shown as factors
  gtest <- caret::train(x = subset(mtcars, select = -mpg), y = mtcars$mpg, method = "rf")
  torn <- tornado(gtest, type = "PercentChange", alpha = 0.10)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test: caret::train randomforest")
  plot(g)

  # add in factors
  mydat <- mtcars
  mydat$am <- factor(mydat$am)
  mydat$vs <- factor(mydat$vs)
  mydat$cyl <- factor(mydat$cyl)
  mydat$gear <- factor(mydat$gear)
  mydat$carb <- factor(mydat$carb)
  gtest <- caret::train(x = subset(mydat, select = -mpg), y = mydat$mpg, method = "rf")
  torn <- tornado(gtest, type = "PercentChange", alpha = 0.10)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test:  caret::train randomforest PercentChange with factors")
  plot(g)

  torn <- tornado(gtest, type = "percentiles", alpha = 0.10)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test:  caret::train randomforest percentiles with factors")
  plot(g)

  torn <- tornado(gtest, type = "ranges", alpha = 0.10, xlabel = "MPG")
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test:  caret::train randomforest ranges with factors")
  plot(g)

  gtest <- caret::train(x = subset(mtcars, select = -mpg),
                        y = mtcars$mpg, method = "glmStepAIC", trace = 0)
  torn <- tornado(gtest, type = "PercentChange", alpha = 0.10, xlabel = "MPG")
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test: caret::train glmStepAIC")
  plot(g)
})

test_that("tornado caret::train works with classification", {
  testthat::skip_if_not_installed("caret")
  testthat::skip_if_not_installed("MASS")
  testthat::skip_if_not_installed("randomForest")

  mydat <- mtcars
  mydat$am <- factor(mydat$am)
  mydat$vs <- factor(mydat$vs)
  mydat$cyl <- factor(mydat$cyl)
  mydat$gear <- factor(mydat$gear)
  mydat$carb <- factor(mydat$carb)
  gtest <- caret::train(x = subset(mydat, select = -vs), y = mydat$vs, method = "rf")
  torn <- tornado(gtest, type = "PercentChange", alpha = 0.10, class_number = 1)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "Probability of Class 1")
  g <- g + ggtitle("Test:  classifier caret::train randomforest PercentChange with factors")
  plot(g)

  torn <- tornado(gtest, type = "PercentChange", alpha = 0.10, class_number = 2)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "Probability of Class 2")
  g <- g + ggtitle("Test:  classifier caret::train randomforest PercentChange with factors")
  plot(g)

  torn <- tornado(gtest, type = "percentiles", alpha = 0.10, class_number = 1)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "Probability of Class 1")
  g <- g + ggtitle("Test:  classifier caret::train randomforest percentiles with factors")
  plot(g)

  torn <- tornado(gtest, type = "ranges", alpha = 0.10, class_number = 1)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "Probability of Class 1")
  g <- g + ggtitle("Test:  classifier caret::train randomforest ranges with factors")
  plot(g)

  # change colors
  torn <- tornado(gtest, type = "ranges", alpha = 0.10,
                  class_number = 1)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "Probability of Class 1", sensitivity_colors = c("red", "blue"))
  g <- g + ggtitle("Test:  classifier caret::train randomforest ranges with factors - colors")
  plot(g)

  # change bar width
  torn <- tornado(gtest, type = "ranges", alpha = 0.10, class_number = 1)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "Probability of Class 1", geom_bar_control = list(width = 0.1))
  g <- g + ggtitle("Test:  classifier caret::train randomforest ranges with factors - bar width")
  plot(g)

  # change point color and type
  g <- tornado(gtest, type = "ranges", alpha = 0.10,
               class_number = 1)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "Probability of Class 1",
            geom_point_control = list(fill = "red", shape = 19, size = 2, col = "red"))
  g <- g + ggtitle("Test:  classifier caret::train randomforest ranges with factors - points")
  plot(g)
})
