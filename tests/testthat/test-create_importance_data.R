context("test-create_importance_data")

test_that("create importance data works", {
  model_final <- lm(mpg ~ cyl + wt + hp + gear + carb, data=mtcars)
  model_null <- lm(mpg ~ 1, data=mtcars)

  tab_summary <- .create_tab_summary(model_final, model_null, dict=NA, isDeviance=FALSE)
  expect_equal("data.frame", class(tab_summary))
  expect_equal(1, tab_summary$cum_contr[nrow(tab_summary)])

  imp <- .create_common_importance_plot(tab_summary, isDeviance=FALSE,
                                               col_imp_alone = "#69BE28",
                                               col_imp_cumulative = "#427730")
  expect_true(all(c("gtable", "gTree", "grob", "gDesc") %in% class(imp)))
})

