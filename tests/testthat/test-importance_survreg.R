context("test-importance_survreg")

test_that("importance.survreg works", {
  imp <- importance(survreg_model, survival::ovarian, nperm = 25)
  expect_equal(class(imp), "importance_plot")
  plot(imp)
})

test_that("importance.survreg works with weights", {
  imp <- importance(survreg_model_weighted, survival::ovarian, nperm = 25)
  expect_equal(class(imp), "importance_plot")
  plot(imp)
})
