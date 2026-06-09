test_that("compute_dynamic_bewrs returns probabilities", {
  x <- compute_dynamic_bewrs(
    up = c(0.2, 0.5, 0.8),
    persistence = c(0.1, 0.4, 0.7),
    deterioration = c(0.0, 0.1, 0.2)
  )

  expect_type(x, "double")
  expect_length(x, 3)
  expect_true(all(x >= 0 & x <= 1))
})

test_that("compute_dynamic_bewrs handles extreme probabilities", {
  x <- compute_dynamic_bewrs(
    up = c(0, 1),
    persistence = c(0, 0),
    deterioration = c(0, 0)
  )

  expect_true(all(is.finite(x)))
  expect_true(all(x > 0 & x < 1))
})
