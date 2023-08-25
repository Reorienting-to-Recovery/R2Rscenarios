two_acres <- 8093.72

test_that("add partial contrallability works", {
  amount <- add_parital_controllability(two_acres)
  expect_lte(amount, 1.5*two_acres)
  expect_gte(amount, 0.5*two_acres)

  expect_equal(length(add_parital_controllability(two_acres, 2)), 2)
})
