context("test-tornado_lm")

test_that("linear model tornado works", {
  itypes <- c("PercentChange", "percentiles", "ranges", "StdDev")
  ialpha <- c(0.10, 0.05, NA, 2)

  for (i in 1:4)
  {
    torn <- tornado(model_nofactors, type = itypes[i], alpha = ialpha[i])
    expect_equal(class(torn), "tornado_plot")
    g <- plot(torn, plot = FALSE, xlabel = "MPG")
    g <- g + ggtitle(paste("Test:  Linear model ", itypes[i]))
    plot(g)
  }

  dict <- list(old = c("cyl", "wt", "hp"),
               new = c("Cylinders", "Weight", "Horsepower"))
  torn <- tornado(model_nofactors, type = "PercentChange", alpha = 0.10, dict = dict)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test:  Linear model PercentChange with dictionary")
  plot(g)

  # test a model with two factors
  for (i in 1:4)
  {
    torn <- tornado(model_twofactors, type = itypes[i], alpha = ialpha[i])
    expect_equal(class(torn), "tornado_plot")
    g <- plot(torn, plot = FALSE, xlabel = "MPG")
    g <- g + ggtitle(paste("Test:  Linear model ", itypes[i], " with 2 factors"))
    plot(g)
  }

  # test a variable with one factor
  for (i in 1:4)
  {
    torn <- tornado(model_onefactor, type = itypes[i], alpha = ialpha[i])
    expect_equal(class(torn), "tornado_plot")
    g <- plot(torn, plot = FALSE, xlabel = "MPG")
    g <- g + ggtitle(paste("Test:  Linear model ", itypes[i], " with one factor"))
    plot(g)
  }
})


test_that("Errors in tornado.lm", {
  expect_error(tornado(model_nofactors, type = "blah", alpha = 0.10))
})

test_that("linear model tornado works with weights", {
  gtest <- lm(mpg ~ cyl*wt*hp, data = mtcars, weights = rep(1:2, nrow(mtcars) / 2))
  torn <- tornado(gtest, type = "PercentChange", alpha = 0.10)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "MPG")
  g <- g + ggtitle("Test:  Linear model PercentChange weighted")
  plot(g)
})
