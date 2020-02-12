# Utility function for testing the return type from plotting functions
check_plot_return_type = function(plot_result){
  expect_type(plot_result, "list")
  expect_equal(names(plot_result), c("plot","data"))
  expect_s3_class(plot_result$plot, "gg")
  expect_s3_class(plot_result$data, "data.frame")
}

test_that("survival_estimate returns correct values", {
  input = data.frame(t=c(1,2,3),
                     n_churn = c(1,2,3),
                     n_survive = c(6,5,4))

  # Manually compute the Kaplan-Meier estimate
  n_total = sum(input$n_churn) + sum(input$n_survive)
  p1 = 1 - input$n_churn[1]/n_total
  n_remain = n_total - input$n_churn[1] - input$n_survive[1]
  p2 = p1 * (1 - input$n_churn[2]/n_remain)
  n_remain = n_remain - input$n_churn[2] - input$n_survive[2]
  p3 = p2 * (1 - input$n_churn[3]/n_remain)
  expected = data.frame(t = c(1,2,3),
                        frac_surv = c(p1, p2, p3) )

  result = survival_estimate(input)
  expect_equal(result, expected)
})

test_that("plotting functions return correct types", {

  ####################################
  # plot_churn_distribution
  fit = list(a=1, b=1)
  result = plot_churn_distribution(fit, type="density",
                                   sample_size=1000, n_bins=10)
  check_plot_return_type(result)

  result = plot_churn_distribution(fit, type="percent",
                                   sample_size=1000, n_bins=10)
  check_plot_return_type(result)

  result = plot_churn_distribution(fit, type="count",
                                   sample_size=1000, n_bins=10)
  check_plot_return_type(result)

  ####################################
  # plot_survival_estimate
  data(customer_lifetimes)
  data = transform_individual(customer_lifetimes)
  result = plot_survival_estimate(data)
  check_plot_return_type(result)

  ####################################
  # plot_model_fit
  result = plot_model_fit(fit, data)
  check_plot_return_type(result)
})
