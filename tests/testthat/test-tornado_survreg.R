context("test-tornado_survreg")

test_that("survreg tornado works", {
  gtest <- survival::survreg(survival::Surv(futime, fustat) ~ ecog.ps + rx, survival::ovarian, dist = 'weibull', scale = 1)
  g <- tornado(gtest, survival::ovarian, type = "PercentChange", alpha = 0.10, xlabel = "Survival Time")
  g <- g + ggtitle("Test: survival regression")
  expect_equal(class(g), c("gg","ggplot"))
  plot(g)

  mydat <- survival::ovarian
  mydat$resid.ds <- factor(mydat$resid.ds)
  gtest <- survival::survreg(survival::Surv(futime, fustat) ~ ecog.ps + rx + resid.ds, mydat, dist = 'weibull', scale = 1)
  g <- tornado(gtest, mydat, type = "PercentChange", alpha = 0.10, xlabel = "Survival Time")
  g <- g + ggtitle("Test: survival regression with factor")
  expect_equal(class(g), c("gg","ggplot"))
  plot(g)
})
