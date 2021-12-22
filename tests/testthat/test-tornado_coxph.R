context("test-tornado_coxph")

test_that("tornado.coxph works", {
  gtest <- survival::coxph(survival::Surv(stop, event) ~ rx + size + number,
                           survival::bladder)
  g <- tornado(gtest, modeldata = survival::bladder, type = "PercentChange",
               alpha = 0.10, xlabel = "Risk")
  g$plot <- g$plot + ggtitle("Test: CoxPH")
  expect_equal(class(g), "tornado_plot")
  expect_equal(class(g$plot), c("gg","ggplot"))
  plot(g)
})
