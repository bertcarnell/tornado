context("test-quantile_ordered")

test_that("quantile.ordered works", {
  x <- quantile(ordered(rep(c("C","B","A"), each = 30), levels = c("C","B","A")),
                probs <- seq(0, 1, 0.25))
  expect_equal(as.character(x), c("C","C","B","A","A"))
  expect_true(is.ordered(x))
  expect_equal(levels(x), c("C","B","A"))
})
