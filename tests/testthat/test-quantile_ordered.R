context("test-quantile_ordered")

test_that("quantile.ordered works", {
  x <- quantile(ordered(rep(c("C","B","A"), each = 30), levels = c("C","B","A")),
                probs = seq(0, 1, 0.25))
  expect_equal(as.character(x), c("C","C","B","A","A"))
  expect_true(is.ordered(x))
  expect_equal(levels(x), c("C","B","A"))

  x <- quantile(ordered("A"), probs = c(0, 0.5, 1))
  expect_equal(rep("A", 3), as.character(x))

  x <- quantile(ordered(c("A","B"), levels = c("B", "A")), probs = 0.5)
  expect_equal("B", as.character(x))

  x <- quantile(ordered(c("A","B"), levels = c("B", "A")), probs = 0.51)
  expect_equal("A", as.character(x))
})
