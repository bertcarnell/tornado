context("test-tornado_survreg")

test_that("survreg tornado works", {
  gtest <- survival::survreg(survival::Surv(futime, fustat) ~ ecog.ps + rx, survival::ovarian, dist = 'weibull', scale = 1)
  torn <- tornado(gtest, modeldata = survival::ovarian, type = "PercentChange", alpha = 0.10)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "Survival Time")
  g <- g + ggtitle("Test: survival regression")
  plot(g)

  mydat <- survival::ovarian
  mydat$resid.ds <- factor(mydat$resid.ds)
  gtest <- survival::survreg(survival::Surv(futime, fustat) ~ ecog.ps + rx + resid.ds, mydat, dist = 'weibull', scale = 1)
  torn <- tornado(gtest, modeldata = mydat, type = "PercentChange", alpha = 0.10)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "Survival Time")
  g <- g + ggtitle("Test: survival regression with factor")
  plot(g)
})

test_that("survreg tornado works with weights", {
  gtest <- survival::survreg(survival::Surv(futime, fustat) ~ ecog.ps + rx,
                             survival::ovarian, dist = 'weibull', scale = 1,
                             weights = rep(1:2, nrow(survival::ovarian) / 2))
  torn <- tornado(gtest, modeldata = survival::ovarian, type = "PercentChange", alpha = 0.10)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "Survival Time")
  g <- g + ggtitle("Test: survival regression with weights")
  plot(g)
})
