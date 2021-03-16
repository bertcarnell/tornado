context("test-importance_survreg")

test_that("importance.survreg works", {
  require(survival)
  model_final <- survreg(Surv(futime, fustat) ~ ecog.ps*rx + age, ovarian,
                          dist = "weibull")
  g <- importance(model_final, ovarian, 100)
  expect_equal(class(g), c("gg","ggplot"))
})
