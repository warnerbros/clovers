#' Mean and standard deviation of modeled lifetime.
#'
#' Compute mean and SD of lifetime, limited to a time horizon,
#' for a fitted model. Uses the \code{a} and \code{b} parameters of the
#' beta distribution (as returned by, e.g., the function
#' \code{fit_sbg}).
#'
#' The lifetime stats are computed assuming that we limit the data
#' to a finite number of periods, as specified by the horizon parameter.
#' Thus one can compute, for example, a 12-month or 36-month LTV.
#'
#' @inheritParams plot_churn_distribution
#' @param horizon Integer, the number of periods defining the horizon.
#'
#' @return A list with the following elements:
#'     \item{\code{mean_lifetime}}{Numeric, the mean lifetime predicted
#'          by the model.}
#'     \item{\code{sd_lifetime}}{Numeric, the standard deviation of the lifetime
#'         predicted by the model.}
#' @export
#'
#' @examples
#' data(customer_lifetimes)
#' data = transform_individual(customer_lifetimes)
#' fit = fit_sbg(data)
#' lifetime_stats(fit)
lifetime_stats = function(fit, horizon=36){
  a = fit$a
  b = fit$b
  # Compute the values by directly summing the series
  tvals = c(1:horizon)
  summands = tvals*exp(lbeta(a+1,b+tvals-1) - lbeta(a,b))
  summands = c(summands, horizon*exp(lbeta(a,b+horizon)-lbeta(a,b)))
  summands2 = tvals^2*exp(lbeta(a+1,b+tvals-1) - lbeta(a,b))
  summands2 = c(summands2, horizon^2*exp(lbeta(a,b+horizon-1)-lbeta(a,b)))
  meant = sum(summands)
  sdt = sqrt(sum(summands2)-meant^2)
  return(list(mean_lifetime = meant,
              sd_lifetime = sdt))
}

# Compute mean and SD lifetime with manually-tweaked churn rate
# Given a churn-rate distribution with parameters in the object "fit",
# sample individual churn rates. Then apply the adjustment to each
# of the sampled churn rates. Finally, sample an actual lifetime
# for each individual.
lifetime_stats_adj = function(fit, horizon=36,
                              p_reduction=0, p_knot = 0.05,
                              p_mult = NULL, nsamps=1e5, random_seed=1){
  a = fit$a
  b = fit$b

  # Sample churn rates for nsamps individuals
  set.seed(random_seed)
  p_samp_floor = 1e-5
  psamps = rbeta(nsamps, a, b)

  # Modify churn rates
  if(is.null(p_mult)){
    if(p_reduction > p_knot) stop("p_reduction must be less than or equal to p_knot")
    psamps_reduced = ifelse(psamps >= p_knot,
                            psamps - p_reduction,
                            pmax((p_knot-p_reduction)*psamps/p_knot, p_samp_floor))
  } else {
    psamps_reduced = p_mult*psamps
  }

  # Sample actual lifetimes using modified churn rates
  ysamps = rgeom(nsamps, prob=psamps_reduced) + 1

  # Limit time range to horizon
  ysamps_cutoff = ysamps
  ysamps_cutoff[ysamps_cutoff > horizon] = horizon
  return(list(mean_lifetime = mean(ysamps_cutoff, na.rm=TRUE),
              sd_lifetime = sd(ysamps_cutoff, na.rm=TRUE)
              )
         )
}
