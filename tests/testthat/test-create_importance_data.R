context("test-create_importance_data")

test_that("create importance data works", {
  model_final <- lm(mpg ~ cyl + wt + hp + gear + carb, data=mtcars)
  model_null <- lm(mpg ~ 1, data=mtcars)

  tab_summary <- .create_tab_summary(model_final, model_null, dict=NA, isDeviance=FALSE)
  expect_equal("data.frame", class(tab_summary))
  expect_equal(1, tab_summary$cum_contr[nrow(tab_summary)])

  dat2 <- .create_common_importance_data(tab_summary)
  expect_equal(class(dat2), "data.frame")
})

