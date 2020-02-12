test_that("lifetime_stats returns correct mean lifetime", {
  a = 1
  b = 1
  horizon = 2
  input_fit = list(a=a, b=b)

  expected = horizon*beta(a,b+horizon) + beta(a+1, b) + horizon*beta(a+1,b+1)
  expected = expected / beta(a,b)
  result = lifetime_stats(input_fit, horizon)
  expect_equal(expected, result$mean_lifetime)
})
