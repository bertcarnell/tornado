context("test-tornado_survreg")

test_that("survreg tornado works", {
  data("ovarian", package = "survival")
  gtest <- survreg(Surv(futime, fustat) ~ ecog.ps + rx, ovarian, dist = 'weibull', scale = 1)
  g <- tornado(gtest, ovarian, type = "PercentChange", alpha = 0.10, xlabel = "futime")
  expect_equal(class(g), c("gg","ggplot"))
})
