test_that("fit matches Fader's example", {
  # Use Fader's numbers from his Appendix B
  inputs = data.frame(t = c(1,2,3,4,5,6,7),
                      n_churn = c(131,
                                  126,
                                  90,
                                  60,
                                  42,
                                  34,
                                  26
                      ),
                      n_survive = c(0,0,0,0,0,0,491))
  result = fit_sbg(inputs, init_params = c(1,10))
  expect_equal(result$a, 0.668, tolerance = 0.01)
  expect_equal(result$b, 3.806, tolerance = 0.01)
})

test_that("loglik_sbg matches Fader's example", {
  # Use Fader's numbers from his Appendix B
  inputs = data.frame(t = c(1,2,3,4,5,6,7),
                      n_churn = c(131,
                                  126,
                                  90,
                                  60,
                                  42,
                                  34,
                                  26
                      ),
                      n_survive = c(0,0,0,0,0,0,491))
  result = loglik_sbg(log(1), log(1), inputs)
  expect_equal(result, -2115.5, tolerance = 0.01)
})
