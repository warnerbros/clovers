#' Transform individual-level data
#'
#' Aggregate individual-level data into aggregate format
#' required by the fit_sbg function.
#'
#' @param individual A data frame with one row per individual, and columns
#'  \itemize{
#'    \item \code{t}: Integer, the number of periods for which the customer
#'        has been active.
#'    \item \code{is_active}: Integer, a flag indicating whether the customer
#'        is still active (\code{1} if still active, \code{0} if churned).
#'  }
#'
#' @return Data frame with the following columns:
#'     \itemize{
#'         \item \code{t}: integer, the period number (\code{t} = 1,2,3,...).
#'         \item \code{n_churn}: integer, the number of customers who
#'             churned after \code{t} periods.
#'         \item \code{n_survive}: integer, the number of customers who
#'             have been subscribed for exactly \code{t} periods and
#'             are still subscribed.
#'     }
#' @export
#'
#' @examples
#' data(customer_lifetimes)
#' transform_individual(customer_lifetimes)
transform_individual = function(individual){
  n_survive = with(individual,
                   tapply(is_active==1, t, sum))
  n_churn   = with(individual,
                   tapply(is_active==0, t, sum))

  df_survive = data.frame(t = as.integer(names(n_survive)),
                          n_survive = as.vector(n_survive))
  df_churn   = data.frame(t = as.integer(names(n_churn)),
                          n_churn = as.vector(n_churn))
  agg = merge(df_churn, df_survive, by="t")

  return(agg)
}
