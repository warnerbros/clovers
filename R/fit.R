#' Fit Fader's Shifted Beta Geometric (sBG) model
#'
#' @param data Data frame with the following columns:
#'     \itemize{
#'         \item \code{t}: integer, the period number (\code{t} = 1,2,3,...).
#'         \item \code{n_churn}: integer, the number of customers who
#'             churned after \code{t} periods.
#'         \item \code{n_survive}: integer, the number of customers who
#'             have been subscribed for exactly \code{t} periods and
#'             are still subscribed.
#'     }
#' @param init_params A list containing initial values of the
#'        sBG parameters \code{a} and \code{b}.
#'
#' @return A list with the following elements
#'     \item{a}{ Numeric, the \code{a} parameter of the sBG model.}
#'     \item{b}{ Numeric, the \code{b} parameter of the sBG model.}
#' @export
#' @examples
#' data(customer_lifetimes)
#' data = transform_individual(customer_lifetimes)
#' fit_sbg(data)
fit_sbg = function(data, init_params = c(1,10)){
  init_params = log(init_params)
  result = stats::optim(init_params,
                        function(x) -loglik_sbg(x[1],x[2], data),
                        method="Nelder-Mead")
  return(list(a = exp(result$par[1]),
              b = exp(result$par[2])))
}

#' Log-likelihood function for the sBG model
#'
#' @param loga Numeric, the natural log of the \code{a} parameter
#'     of the sBG model.
#' @param logb Numeric, the natural log of the \code{b} parameter
#'     of the sBG model.
#' @inheritParams fit_sbg
#'
#' @return The log-likelihood for the sBG model.
#' @export
loglik_sbg = function(loga, logb, data){
  a = exp(loga)
  b = exp(logb)
  n_periods = max(data$t)
  n_churn = data$n_churn
  churn_betas = lbeta(a+1,b+data$t-1) - lbeta(a,b)
  n_survive = data$n_survive
  survive_betas = lbeta(a,b+data$t) - lbeta(a,b)
  ll = sum(n_churn * churn_betas + n_survive * survive_betas)
  return(ll)
}
