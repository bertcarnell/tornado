context("test-tornado_coxph")

test_that("tornado.coxph works", {
  gtest <- survival::coxph(survival::Surv(stop, event) ~ rx + size + number,
                           survival::bladder)
  torn <- tornado(gtest, modeldata = survival::bladder, type = "PercentChange",
               alpha = 0.10)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "Risk")
  g <- g + ggtitle("Test: CoxPH")
  plot(g)
})

test_that("tornado.coxph works with weights", {
  gtest <- survival::coxph(survival::Surv(stop, event) ~ rx + size + number,
                           survival::bladder, weights = rep(1:2, nrow(survival::bladder) / 2))
  torn <- tornado(gtest, modeldata = survival::bladder, type = "PercentChange",
                  alpha = 0.10)
  expect_equal(class(torn), "tornado_plot")
  g <- plot(torn, plot = FALSE, xlabel = "Risk")
  g <- g + ggtitle("Test: CoxPH Weighted")
  plot(g)
})
