#' Plot distribution of churn rates
#'
#' Plots the distribution of churn rates found by the sBG model.
#'
#' With \code{type="density"}, the probability density function is plotted.
#'
#' With \code{type="count"}, a set of customers numbering \code{sample_size}
#' is assumed, the churn rates are binned into \code{n_bins} bins, and the
#' count of customers in each bin is plotted as a bar chart.
#'
#' With \code{type="percent"}, the plot is similar to the plot produced by
#' \code{type="count"}, except that the bar heights represent the percentages
#' of customers in each bin instead of the counts.
#'
#' @param fit List with components \code{a} and \code{b},
#'        the parameters of the sBG model
#' @param type Type of plot to draw: \code{"density"},
#'        \code{"count"}, or \code{"percent"}.
#' @param sample_size Integer. If \code{type="count"}, total number
#'        of customers.
#' @param n_bins Integer. If \code{type="count"} or \code{type="percent"},
#'        number of bins.
#'
#' @return A list containing the following components:
#'     \item{plot}{A ggplot2 object containing the plot.}
#'     \item{data}{A data frame containing the plot data.}
#' @export
#'
#' @examples
#' data(customer_lifetimes)
#' data = transform_individual(customer_lifetimes)
#' fit = fit_sbg(data)
#' plot_churn_distribution(fit)
#' plot_churn_distribution(list(a=1.5, b=1.5), type="count")
plot_churn_distribution = function(fit, type="density",
                                   sample_size=1000, n_bins=10){
  if(! type %in% c("density", "count", "percent")){
    stop("Argument 'type' must be 'density', 'count', or 'percent'")
  }
  a = fit$a
  b = fit$b
  if(type=="density"){
    n_points = 1000
    churn_rates = seq(0,1,by=1/(n_points-1))
    plot_df = data.frame(churn_rate=churn_rates,
                         y=stats::dbeta(churn_rates, a, b))
    if(any(is.infinite(plot_df$y))){
      inf_indices = which(is.infinite(plot_df$y))
      plot_df = plot_df[-inf_indices,]
    }
    p = ggplot2::ggplot(plot_df, ggplot2::aes_string(x="churn_rate", y="y")) +
      ggplot2::geom_line() +
      ggplot2::ylim(0,max(plot_df$y)) +
      ggplot2::xlab("Churn rate") +
      ggplot2::ylab("Probability density") +
      ggplot2::theme(axis.title.y =
                       ggplot2::element_text(angle = 0, hjust = 1)) +
      ggplot2::scale_x_continuous(labels = scales::percent, limits=c(0,1.0))
  }
  else if(type=="count" | type=="percent"){
    nsamps = 1e5
    set.seed(1)
    samps = stats::rbeta(nsamps,a,b)
    breaks = seq(0,1.0,1.0/n_bins)
    binned = cut(samps, breaks=breaks)
    bin_counts = with(data.frame(bin=binned),
                      tapply(bin, bin, length))
    plot_df = data.frame(bin=names(bin_counts),
                         y=bin_counts,
                         row.names = NULL)
    plot_df$bin = breaks[1:(length(breaks)-1)]

    if(type=="percent"){
      plot_df$y = plot_df$y/nsamps
    } else if(type=="count"){
      plot_df$y = plot_df$y * (sample_size/nsamps)
    }

    p = ggplot2::ggplot(plot_df,
                        ggplot2::aes_string(x="`bin`+0.5/`n_bins`",y="y")) +
      ggplot2::geom_bar(stat="identity", width=0.95*1.0/n_bins) +
      ggplot2::xlab("Churn rate") +
      ggplot2::theme(axis.title.y =
                       ggplot2::element_text(angle = 0, hjust = 1)) +
      ggplot2::scale_x_continuous(labels = scales::percent, limits=c(0,1.0),
                                  breaks = breaks)

    if(type=="percent"){
      p = p +
        ggplot2::ylab("% of customers") +
        ggplot2::scale_y_continuous(
          labels = scales::percent_format(accuracy = 1))
    } else if(type=="count"){
      p = p +
        ggplot2::ylab("# of customers") +
        ggplot2::scale_y_continuous(labels = scales::comma)
    }
  }
  return(list(plot=p,
              data=plot_df))
}

#' Compute empirical survival function
#'
#' Computes the Kaplan-Meier estimate of the survival function.
#'
#' @inheritParams fit_sbg
#'
#' @return A data frame with columns
#' \itemize{
#'    \item \code{t}: Integer, the number of periods.
#'    \item \code{frac_surv}: Numeric, the fraction of customers still active
#'        after \code{t} periods.
#'  }
#' @export
#'
#' @examples
#' data(customer_lifetimes)
#' data = transform_individual(customer_lifetimes)
#' survival_estimate(data)
survival_estimate = function(data){
  df_churn = data[,c("t","n_churn")]
  names(df_churn) = c("t","n")
  df_churn$variable = "n_churn"
  df_survive = data[,c("t","n_survive")]
  names(df_survive) = c("t","n")
  df_survive$variable = "n_survive"
  data = rbind(df_churn, df_survive)
  data = data[,c("t","variable","n")]

  data$variable = ifelse(data$variable=="n_churn",
                             1,
                             ifelse(data$variable=="n_survive", 0, NA))

  fit <- with(data,
              survival::survfit(survival::Surv(time=t, event=variable,
                                               type='right') ~1,
                                weights=n, type="kaplan-meier")
  )
  n_total = sum(c(fit$n.event,fit$n.censor))
  df_out = data.frame(t=fit$time,
                      surv = fit$surv)

  names(df_out) = c("t","frac_surv")
  return(df_out)
}

#' Plot empirical survival function
#'
#' Plots the Kaplan-Meier estimate of the survival function.
#'
#' @inheritParams fit_sbg
#'
#' @return A list containing the following components:
#'     \item{plot}{A ggplot2 object containing the plot.}
#'     \item{data}{A data frame containing the plot data.}
#' @export
#'
#' @examples
#' data(customer_lifetimes)
#' data = transform_individual(customer_lifetimes)
#' plot_survival_estimate(data)
plot_survival_estimate = function(data){
  surv_data = survival_estimate(data)
  p = ggplot2::ggplot(surv_data, ggplot2::aes_string(x="t",y="frac_surv")) +
    ggplot2::geom_line() +
    ggplot2::xlim(0,max(surv_data$t)) +
    ggplot2::geom_point()
  p = p +
    ggplot2::theme(axis.title.y =
                     ggplot2::element_text(angle = 0, hjust = 1)) +
    ggplot2::scale_y_continuous(labels = scales::percent, limits=c(0,1.0)) +
    ggplot2::ylab("Percent remaining") +
    ggplot2::xlab("# Periods") +
    ggplot2::theme(legend.title=ggplot2::element_blank())
  return(list(plot=p,
              data=surv_data))
}

#' Plot empirical survival and modeled survival
#'
#' Draws a plot with two curves: the empirical survival
#' function computed with the Kaplan-Meier estimator, and
#' the survival function computed from the fitted sBG model.
#'
#' @inheritParams fit_sbg
#' @inheritParams plot_churn_distribution
#'
#' @return A list containing the following components:
#'     \item{plot}{A ggplot2 object containing the plot.}
#'     \item{data}{A data frame containing the plot data.}
#' @export
#'
#' @examples
#' data(customer_lifetimes)
#' data = transform_individual(customer_lifetimes)
#' fit = fit_sbg(data)
#' plot_model_fit(fit, data)
plot_model_fit = function(fit, data){
  a = fit$a
  b = fit$b
  surv_data = survival_estimate(data)
  surv_data$group = "Data"
  # names(surv_data) = c("t","frac_surv","group")
  surv_data = rbind(data.frame(t=0,frac_surv=1.0,group="Data"),
                    surv_data)

  survfit = beta(a, b+surv_data$t) / beta(a, b)
  surv_fit = data.frame(t=surv_data$t,
                        frac_surv=survfit,
                        group="Model")
  # surv_fit = rbind(data.frame(t=0,frac_surv=1.0,group="Model"),
  #                  surv_fit)
  plot_df = rbind(surv_data,
                  surv_fit)
  p = ggplot2::ggplot(plot_df, ggplot2::aes_string(x="t",y="frac_surv",
                                            group="group", color="group")) +
    ggplot2::geom_line() +
    ggplot2::xlim(0,max(surv_data$t)) +
    ggplot2::geom_point() +
    ggplot2::scale_color_manual(values=c("black","red"))
  p = p + ggplot2::theme(axis.title.y =
                           ggplot2::element_text(angle = 0, hjust = 1)) +
    ggplot2::scale_y_continuous(labels = scales::percent, limits=c(0,1.0)) +
    ggplot2::ylab("Percent remaining") +
    ggplot2::xlab("# Periods") +
    ggplot2::theme(legend.title=ggplot2::element_blank())
  return(list(plot=p,
              data=plot_df))
}
