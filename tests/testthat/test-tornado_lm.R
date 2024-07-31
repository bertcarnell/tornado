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

  # test submitted by ghobro (https://github.com/bertcarnell/tornado/issues/13)
  set.seed(123)
  data <- data.frame(
    group = rep(letters[1:3], times = c(3, 5, 4)),
    x = rep(c(0, 5, 10), times = c(3, 5, 4)),
    y = c(rnorm(3, mean = 0), rnorm(5, mean = 5), rnorm(4, mean = 10))
  )

  model <- lm(y ~ x, data = data)

  grouped_data <- data.frame(
    group = c("A", "B", "C"),
    x = by(data$x, data$group, mean),
    y = by(data$y, data$group, mean),
    count = c(3, 4, 5))

  weighted_model <- lm(y ~ x, data = grouped_data, weights = count)

  expect_equal(coef(model), coef(weighted_model), tolerance = 1E-2)

  torn <- tornado(model, type = "percentiles", alpha = 0.10)
  torn_weighted <- tornado(weighted_model, type = "percentiles", alpha = 0.10)

  expect_equal(torn$data$pmeans, predict(model, newdata = data.frame(x = mean(data$x))))
  expect_equal(torn_weighted$data$pmeans, predict(weighted_model, newdata = data.frame(x = weighted.mean(grouped_data$x, grouped_data$count))))

  torn_weighted <- tornado(weighted_model, type = "PercentChange", alpha = 0.10)
  expect_equal(torn_weighted$data$pmeans, predict(weighted_model, newdata = data.frame(x = weighted.mean(grouped_data$x, grouped_data$count))))
  torn_weighted <- tornado(weighted_model, type = "ranges")
  expect_equal(torn_weighted$data$pmeans, predict(weighted_model, newdata = data.frame(x = weighted.mean(grouped_data$x, grouped_data$count))))
  torn_weighted <- tornado(weighted_model, type = "StdDev", alpha = 2)
  expect_equal(torn_weighted$data$pmeans, predict(weighted_model, newdata = data.frame(x = weighted.mean(grouped_data$x, grouped_data$count))))
})
