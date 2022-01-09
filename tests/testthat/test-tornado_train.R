context("test-tornado_train")

test_that("tornado caret::train works", {
  testthat::skip_if_not_installed("caret")
  testthat::skip_if_not_installed("MASS")
  testthat::skip_if_not_installed("randomForest")

  # cyl and vs are numeric
  #   cyl and vs are not plotted on the tornado and not shown as factors
  torn <- tornado(rf_model_numeric, type = "PercentChange", alpha = 0.10)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test: caret::train randomforest")
  plot(g)

  # add in factors
  torn <- tornado(rf_model_factor, type = "PercentChange", alpha = 0.10)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test:  caret::train randomforest PercentChange with factors")
  plot(g)

  torn <- tornado(rf_model_factor, type = "percentiles", alpha = 0.10)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test:  caret::train randomforest percentiles with factors")
  plot(g)

  torn <- tornado(rf_model_factor, type = "ranges", alpha = 0.10, xlabel = "MPG")
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test:  caret::train randomforest ranges with factors")
  plot(g)

  torn <- tornado(stepAIC_model_numeric, type = "PercentChange", alpha = 0.10, xlabel = "MPG")
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test: caret::train glmStepAIC")
  plot(g)
})

test_that("tornado caret::train works with classification", {
  testthat::skip_if_not_installed("caret")
  testthat::skip_if_not_installed("MASS")
  testthat::skip_if_not_installed("randomForest")

  torn <- tornado(rf_model_class, type = "PercentChange", alpha = 0.10, class_number = 1)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "Probability of Class 1")
  g <- g + ggtitle("Test:  classifier caret::train randomforest PercentChange with factors")
  plot(g)

  torn <- tornado(rf_model_class, type = "PercentChange", alpha = 0.10, class_number = 2)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "Probability of Class 2")
  g <- g + ggtitle("Test:  classifier caret::train randomforest PercentChange with factors")
  plot(g)

  torn <- tornado(rf_model_class, type = "percentiles", alpha = 0.10, class_number = 1)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "Probability of Class 1")
  g <- g + ggtitle("Test:  classifier caret::train randomforest percentiles with factors")
  plot(g)

  torn <- tornado(rf_model_class, type = "ranges", alpha = 0.10, class_number = 1)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "Probability of Class 1")
  g <- g + ggtitle("Test:  classifier caret::train randomforest ranges with factors")
  plot(g)

  # change colors
  torn <- tornado(rf_model_class, type = "ranges", alpha = 0.10,
                  class_number = 1)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "Probability of Class 1", sensitivity_colors = c("red", "blue"))
  g <- g + ggtitle("Test:  classifier caret::train randomforest ranges with factors - colors")
  plot(g)

  # change bar width
  torn <- tornado(rf_model_class, type = "ranges", alpha = 0.10, class_number = 1)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "Probability of Class 1", geom_bar_control = list(width = 0.1))
  g <- g + ggtitle("Test:  classifier caret::train randomforest ranges with factors - bar width")
  plot(g)

  # change point color and type
  g <- tornado(rf_model_class, type = "ranges", alpha = 0.10,
               class_number = 1)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "Probability of Class 1",
            geom_point_control = list(fill = "red", shape = 19, size = 2, col = "red"))
  g <- g + ggtitle("Test:  classifier caret::train randomforest ranges with factors - points")
  plot(g)
})
