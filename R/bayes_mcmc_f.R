#' @title bayes_mcmc
#' @description Produces Markov chain Monte Carlo plots
#' @param model Output from `bayes_model()`
#' @param async Whether or not the function is being used asynchronously. Default `FALSE`
#' @return list containing:
#' \item{gelman_plots}{Gelman plots}
#' \item{trace_plots}{Gelman plots}
#' \item{density_plots}{Gelman plots}
#' \item{n_rows}{The number of rows for each plot}
#' @export
bayes_mcmc <- function(model, async = FALSE){

  if (!inherits(model, "bayes_model")){
    return(async |> asyncLog(type = "error", "model must be an object created by bayes_model()"))
  }

  # prevent plots being produced
  pdf(file = NULL)
  on.exit(dev.off())

  parameters <- model$mtcResults$model$monitors$enabled
  gelman_data <- lapply(parameters,
                        function(parameter) {
                          return(coda::gelman.plot(model$mtcResults$samples[, parameter]))
                        })

  plots <- list()
  plots$gelman_plots <- GelmanPlots(gelman_data, parameters)
  plots$trace_plots <- TracePlots(model$mtcResults, parameters)
  plots$density_plots <- DensityPlots(model$mtcResults, parameters)
  plots$n_rows <- ceiling(length(parameters) / 2)
  plots
}

#' Creates a Gelman plot for a gemtc or bnma model.
#'
#' @param gelman_data Output from `coda::gelman.plot(model$samples[, parm])`, where `parm` is a parameter from the model.
#' @param parameter The parameter from the previous argument, used as the title.
#' @return A function that reproduces the Gelman plot mentioned in @param gelman_plot as a plot that can be put in a grid.
GelmanPlot <- function(gelman_data, parameter) {
  function(){
    y_vals_median <- gelman_data$shrink[, , "median"]
    y_vals_975 <- gelman_data$shrink[, , "97.5%"]
    x_vals <- gelman_data$last.iter
    plot(x_vals, y_vals_975, type = "l", col = "red", lty = 2, ylab = "shrink factor",
         xlab = "last iteration in chain", cex.lab = 1.5, cex.main = 1.5, main = parameter)
    lines(x_vals, y_vals_median, type = "l")
    lines(c(-max(x_vals)/5, max(x_vals)), c(1, 1))
    legend("topright", legend = c("median", "97.5%"), lty = c(1, 2), col = c("black", "red"))
  }
}


#' Creates Gelman plots for a gemtc or bnma model.
#'
#' @param gelman_plots List of outputs from `coda::gelman.plot(model$samples[, parm])`, where parm is a parameter from the model.
#' @param parameters Vector of parameters mentioned in the previous argument.
#' @return A list of functions that produce Gelman plots mentioned in @param gelman_plots.
GelmanPlots <- function(gelman_data, parameters) {
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
TracePlots <- function(model, parameters) {
  trace_plots <- list()
  for (i in 1:length(parameters)) {
    trace_plots[[i]] <- bayesplot::mcmc_trace(x = model$samples, pars = parameters[i]) + ggplot2::ggtitle(parameters[i]) + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  }
  return(trace_plots)
}


#' Creates posterior density plots of MCMC samples.
#'
#' @param model Model output.
#' @param parameters Vector of parameters to create density plots for.
#' @return List of ggplot density plots.
DensityPlots <- function(model, parameters) {
  density_plots <- list()
  for (i in 1:length(parameters)) {
    density_plots[[i]] <- bayesplot::mcmc_hist(x = model$samples, pars = parameters[i])
  }
  return(density_plots)
}
