#' @title Markov chain Monte Carlo plots
#' @description Produce Markov chain Monte Carlo plots for Bayesian models
#'
#' @param model Output from `baseline_model()`, `bayes_model()` or `covariate_model()`
#' @inheritParams common_params
#' @return list containing:
#' \item{gelman_plots}{Gelman plots}
#' \item{trace_plots}{Trace plots}
#' \item{density_plots}{Density plots}
#' \item{n_rows}{The number of rows for each plot}
#' @examples
#' configured_data_path <- system.file("extdata", "configured_data.Rds", package = "metainsight")
#' configured_data <- readRDS(configured_data_path)
#'
#' # n_adapt and n_iter are set low to run quickly, but should be left as the
#' # default values in real use
#'
#' fitted_bayes_model <- bayes_model(configured_data = configured_data,
#'                                   n_adapt = 100,
#'                                   n_iter = 100)
#'
#' bayes_mcmc(model = fitted_bayes_model)
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

  gelman_data <- lapply(parameters,
                        function(parameter) {
                          gelman_preplot(
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
#' @return Vector of treatment effect and covariate parameter names,
#' plus random effects sd and/or exchangeable covariate sd.
#' @noRd
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

#' Prepare the data for the gelman plot. Adapted from coda here:
#' https://github.com/cran/coda/blob/d0c2354522fbc3ce22b4efe1b396ea8d8385f250/R/gelman.R#L200
#'
#' @param x mcmc.list. Samples to plot
#' @param bin.width numeric. The width of bins
#' @param max.bins numeric. The maximum number of bins
#' @param confidence numeric. The confidence interval to use
#' @param transform logical. Whether to transform
#' @param autoburnin logical. Whether to use autoburnin
#' @noRd
gelman_preplot <- function(x, bin.width = bin.width, max.bins = max.bins,
            confidence = confidence, transform = transform,
            autoburnin = autoburnin){

  ###############################################################################
  ## R code from the 'coda' package for MCMC diagnostics, specifically the ##
  ## Gelman-Rubin diagnostic. used under the terms of the GNU General Public ##
  ## License version 2.0 or later (GNU GPL-2+) #
  ## See https://github.com/cran/coda/blob/d0c2354522fbc3ce22b4efe1b396ea8d8385f250/README#L1
  ##############################################################################

  x <- coda::as.mcmc.list(x)
  nbin <- min(floor((coda::niter(x) - 50)/coda::thin(x)), max.bins)
  if (nbin < 1) {
    stop("Insufficient iterations to produce Gelman-Rubin plot")
  }
  binw <- floor((coda::niter(x) - 50)/nbin)
  last.iter <- c(seq(from = stats::start(x) + 50 * coda::thin(x), by = binw *
                       coda::thin(x), length = nbin), stats::end(x))
  shrink <- array(dim = c(nbin + 1, coda::nvar(x), 2))
  dimnames(shrink) <- list(last.iter, coda::varnames(x),
                           c("median", paste(50 * (confidence + 1), "%",
                                             sep = ""))
  )
  for (i in 1:(nbin + 1)) {
    shrink[i, , ] <- coda::gelman.diag(stats::window(x, end = last.iter[i]),
                                 confidence = confidence,
                                 transform = transform,
                                 autoburnin = autoburnin,
                                 multivariate = FALSE)$psrf
  }
  all.na <- apply(is.na(shrink[, , 1, drop = FALSE]), 2, all)
  if (any(all.na)) {
    message("\n******* Error: *******\n")
    message("Cannot compute Gelman & Rubin's diagnostic for any chain \n")
    message("segments for variables", coda::varnames(x)[all.na], "\n")
    message("This indicates convergence failure\n")
  }
  return(list(shrink = shrink, last.iter = last.iter))
}


#' Creates a Gelman plot for a gemtc or bnma model.
#'
#' @param gelman_data Output from `gelman_preplot`
#' @param parameter The parameter from the previous argument, used as the title.
#' @return A function that reproduces the Gelman plot mentioned in @param gelman_plot as a plot that can be put in a grid.
#' @import ggplot2
#' @noRd
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
#' @param gelman_data List of outputs from `gelman_preplot`
#' @param parameters Vector of parameters mentioned in the previous argument.
#' @return List of ggplot Gelman plots
#' @examples
#' configured_data_path <- system.file("extdata", "configured_data.Rds", package = "metainsight")
#' configured_data <- readRDS(configured_data_path)
#'
#' # n_adapt and n_iter are set low to run quickly, but should be left as the
#' # default values in real use
#'
#' fitted_bayes_model <- bayes_model(configured_data = configured_data,
#'                                   n_adapt = 100,
#'                                   n_iter = 100)
#'
#' mcmc <- bayes_mcmc(model = fitted_bayes_model)
#'
#' gelman_plots(mcmc$gelman_data, mcmc$parameters)
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
#' @examples
#' configured_data_path <- system.file("extdata", "configured_data.Rds", package = "metainsight")
#' configured_data <- readRDS(configured_data_path)
#'
#' # n_adapt and n_iter are set low to run quickly, but should be left as the
#' # default values in real use
#'
#' fitted_bayes_model <- bayes_model(configured_data = configured_data,
#'                                   n_adapt = 100,
#'                                   n_iter = 100)
#'
#' mcmc <- bayes_mcmc(model = fitted_bayes_model)
#'
#' trace_plots(fitted_bayes_model$mtcResults, mcmc$parameters)
#'
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
#' @examples
#' configured_data_path <- system.file("extdata", "configured_data.Rds", package = "metainsight")
#' configured_data <- readRDS(configured_data_path)
#'
#' # n_adapt and n_iter are set low to run quickly, but should be left as the
#' # default values in real use
#'
#' fitted_bayes_model <- bayes_model(configured_data = configured_data,
#'                                   n_adapt = 100,
#'                                   n_iter = 100)
#'
#' mcmc <- bayes_mcmc(model = fitted_bayes_model)
#'
#' density_plots(fitted_bayes_model$mtcResults, mcmc$parameters)
#'
#' @export
density_plots <- function(model, parameters) {
  lapply(seq_along(parameters), function(i) {
    bayesplot::mcmc_hist(x = model$samples, pars = parameters[i])
  })
}
