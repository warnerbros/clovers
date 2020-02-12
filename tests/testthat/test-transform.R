test_that("transform_individual works", {
  individual = data.frame(t         = c(1,1,2),
                          is_active = c(0,1,1))
  result = transform_individual(individual)
  expected = data.frame(t         = c(1,2),
                        n_churn   = as.integer(c(1,0)),
                        n_survive = as.integer(c(1,1)))
  expect_equal(result, expected)

  individual = data.frame(t         = c(1,1,2,2,3),
                          is_active = c(0,1,1,1,0))
  result = transform_individual(individual)
  expected = data.frame(t         = c(1,2,3),
                        n_churn   = as.integer(c(1,0,1)),
                        n_survive = as.integer(c(1,2,0)))
  expect_equal(result, expected)
})
