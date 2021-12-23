context("test-importance_survreg")

test_that("importance.survreg works", {
  model_final <- survival::survreg(survival::Surv(futime, fustat) ~ ecog.ps*rx + age,
                                   survival::ovarian,
                                   dist = "weibull")
  imp <- importance(model_final, survival::ovarian, nperm = 100)
  expect_equal(class(imp), "importance_plot")
  plot(imp)
})
