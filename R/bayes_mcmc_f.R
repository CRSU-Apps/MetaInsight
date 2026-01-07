#' @title bayes_mcmc
#' @description Produces Markov chain Monte Carlo plots
#' @param model Output from `baseline_model()`, `bayes_model()` or `covariate_model()`
#' @inheritParams common_params
#' @return list containing:
#' \item{gelman_plots}{Gelman plots}
#' \item{trace_plots}{Gelman plots}
#' \item{density_plots}{Gelman plots}
#' \item{n_rows}{The number of rows for each plot}
#' @export
bayes_mcmc <- function(model, async = FALSE){

  if (!inherits(model, "bayes_model") && !inherits(model, "baseline_model")){
    async |> asyncLog(type = "error", "model must be an object created by baseline_model(), bayes_model() or covariate_model()")
  }

  if (inherits(model, "bayes_model")){
    parameters <- model$mtcResults$model$monitors$enabled
  } else {
    parameters <- GetBnmaParameters(all_parameters = attr(model$mtcResults$samples[[1]], "dimnames")[[2]],
                                    effects_type = model$mtcResults$network$type,
                                    cov_parameters = model$regressor)
  }

  if (inherits(model, "baseline_model") || model$mtcResults$model$type == "regression"){
    n_cols <- 4
  } else {
    n_cols <- 2
  }

  # this uses the internal function from coda with :::
  gelman_data <- lapply(parameters,
                        function(parameter) {
                          coda:::gelman.preplot(
                            coda::as.mcmc.list(model$mtcResults$samples[, parameter]), 10, 50, 0.95, FALSE, TRUE)
                        })

  list(
    parameters = parameters,
    gelman_data = gelman_data,
    n_cols = n_cols,
    n_rows = ceiling(length(parameters) / n_cols),
    n_rows_rmd = ceiling(length(parameters) / 2)
    )
}

#' @rdname bayes_mcmc
#' @param ... Parameters passed to `bayes_mcmc()`
#' @export
covariate_mcmc <- function(...){
  bayes_mcmc(...)
}

#' @rdname bayes_mcmc
#' @param ... Parameters passed to `bayes_mcmc()`
#' @export
baseline_mcmc <- function(...){
  bayes_mcmc(...)
}


#' Get the parameters that are to be displayed in Gelman plots.
#'
#' @param all_parameters Vector of monitored parameters from a bnma model.
#' @param effects_type "fixed" or "random".
#' @param cov_parameters "shared", "exchangeable", or "unrelated".
#' @return Vector of treatment effect and covariate parameter names, plus random effects sd and/or exchangeable covariate sd.
GetBnmaParameters <- function(all_parameters, effects_type, cov_parameters) {
  #Extract parameters which begin with "d[" or "b_bl[", except d[1] and b_bl[1]
  parameters <- grep(
    "(d|b_bl)\\[([0-9][0-9]+|[2-9])\\]",
    all_parameters,
    value = TRUE
  )
  if (effects_type == "random") {
    parameters <- c(parameters, "sd")
  } else if (effects_type != "fixed") {
    stop("effects_type must be 'fixed' or 'random'")
  }
  if (cov_parameters == "exchangeable") {
    parameters <- c(parameters, "sdB")
  } else if (!cov_parameters %in% c("shared", "unrelated")) {
    stop("cov_parameters must be 'shared', 'exchangeable' or 'unrelated'")
  }
  return(parameters)
}


#' Creates a Gelman plot for a gemtc or bnma model.
#'
#' @param gelman_data Output from `coda:::gelman.preplot()`
#' @param parameter The parameter from the previous argument, used as the title.
#' @return A function that reproduces the Gelman plot mentioned in @param gelman_plot as a plot that can be put in a grid.
#' @import ggplot2
GelmanPlot <- function(gelman_data, parameter) {
  y_vals_median <- gelman_data$shrink[, , "median"]
  y_vals_975 <- gelman_data$shrink[, , "97.5%"]
  x_vals <- gelman_data$last.iter

  # Create a data frame for plotting
  plot_data <- data.frame(
    x = rep(x_vals, 2),
    y = c(y_vals_median, y_vals_975),
    type = factor(rep(c("median", "97.5%"), each = length(x_vals)),
                  levels = c("median", "97.5%"))
  )

  ggplot(plot_data, aes(x = .data$x, y = .data$y, color = .data$type, linetype = .data$type)) +
    geom_line() +
    geom_hline(yintercept = 1, color = "black", linetype = 1) +
    scale_color_manual(values = c("median" = "black", "97.5%" = "red")) +
    scale_linetype_manual(values = c("median" = 1, "97.5%" = 2)) +
    labs(x = "Last iteration in chain",
         y = "Shrink factor",
         title = parameter) +
    theme_classic() +
    theme(axis.title = element_text(size = 15),
          axis.text = element_text(size = 12),
          plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
          legend.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.position = c(1, 1),
          legend.justification = c(1, 1),
          legend.background = element_rect(fill = "white", color = "black"),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1))
}

#' Creates Gelman plots for a gemtc or bnma model.
#'
#' @param gelman_data List of outputs from `coda:::gelman.preplot`
#' @param parameters Vector of parameters mentioned in the previous argument.
#' @return List of ggplot Gelman plots
#' @export
gelman_plots <- function(gelman_data, parameters) {
  lapply(seq_along(parameters), function(i) {
    GelmanPlot(
      gelman_data = gelman_data[[i]],
      parameter = parameters[i]
    )
  })
}

#' Creates trace plots of MCMC samples.
#'
#' @param model Model output.
#' @param parameters Vector of parameters to create trace plots for.
#' @return List of ggplot trace plots.
#' @export
trace_plots <- function(model, parameters) {
  lapply(seq_along(parameters), function(i) {
    bayesplot::mcmc_trace(x = model$samples, pars = parameters[i]) +
      ggplot2::ggtitle(parameters[i]) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  })
}

#' Creates posterior density plots of MCMC samples.
#'
#' @param model Model output.
#' @param parameters Vector of parameters to create density plots for.
#' @return List of ggplot density plots.
#' @export
density_plots <- function(model, parameters) {
  lapply(seq_along(parameters), function(i) {
    bayesplot::mcmc_hist(x = model$samples, pars = parameters[i])
  })
}
