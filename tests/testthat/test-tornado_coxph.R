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
