context("test-importance_survreg")

test_that("importance.survreg works", {
  model_final <- survival::survreg(survival::Surv(futime, fustat) ~ ecog.ps*rx + age,
                                   survival::ovarian,
                                   dist = "weibull")
  g <- importance(model_final, survival::ovarian, 100)
  expect_equal(class(g), c("gg","ggplot"))
})
