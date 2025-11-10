#' Produces a summary of a Bayesian model
#'
#' @param model list. Output produced by `baseline_model()`, `bayes_model()` or `covariate_model()`.
#' @inheritParams common_params
#' @return HTML summary of the model
#' @export
bayes_details <- function(model, logger = NULL){

  if (!inherits(model, "bayes_model") && !inherits(model, "baseline_model")){
    logger |> writeLog(type = "error", "model must be an object created by baseline_model(), bayes_model() or covariate_model()")
  }

  if (inherits(model, "bayes_model")){
    mcmc <- GetGemtcMcmcCharacteristics(model$mtcResults)
    priors <- GetGemtcPriors(model$mtcResults)
  } else {
    mcmc <- GetBnmaMcmcCharacteristics(model$mtcResults)
    priors <- GetBnmaPriors(model$mtcResults)
  }

  list(mcmc = mcmc,
       priors = priors)
}

#' @rdname bayes_details
#' @param ... Parameters passed to `bayes_details()`
#' @export
covariate_details <- function(...){
  bayes_details(...)
}

#' @rdname bayes_details
#' @param ... Parameters passed to `bayes_details()`
#' @export
baseline_details <- function(...){
  bayes_details(...)
}



#' MCMC characteristics from a GEMTC model.
#'
#' @param model GEMTC model output.
#' @return Data frame with four MCMC characteristics.
GetGemtcMcmcCharacteristics <- function(model) {
  return(
    data.frame(
      characteristic = c(
        "Chains",
        "Burn-in iterations",
        "Sample iterations",
        "Thinning factor"),
      value = c(
        model$model$n.chain,
        attr(model$samples[[1]], "mcpar")[1] - 1,
        length(model$samples[[1]][, 1]),
        summary(model)$summaries$thin
      )
    )
  )
}

#' Prior distributions from a GEMTC model.
#'
#' @param model GEMTC model output.
#' @return Data frame with prior distribution information.
GetGemtcPriors <- function(model) {
  treatment_effect_var <- round(model$model$data$re.prior.sd^2, digits = 1)
  prior_table <- data.frame(
    parameter = c(
      "Relative treatment effects",
      "Intercepts"
    ),
    value = c(
      paste0(" ~ N (0, ", treatment_effect_var, ")"),
      paste0(" ~ N (0, ", treatment_effect_var, ")")
    )
  )

  #If the model is random effects, add the heterogenity SD
  if (model$model$linearModel == "random") {
    heterogeneity_sd_upper <- round(model$model$data$om.scale, digits = 1)
    prior_table <- rbind(
      prior_table,
      data.frame(
        parameter = "Heterogeneity standard deviation",
        value = paste0(" ~ Unif (0, ", heterogeneity_sd_upper, ")")
      )
    )
  }

  if (!is.null(model$model$regressor$coefficient)) {
    #If the model is NMR shared, add the covariate parameter
    if (model$model$regressor$coefficient == "shared") {
      shared_var <- RoundForDisplay(model$model$data$om.scale^2)
      prior_table <- rbind(
        prior_table,
        data.frame(
          parameter = "Shared covariate parameter",
          value = paste0(" ~ Scaled t-distribution (0, ", shared_var, ", 1)")
        )
      )
      #If the model is NMR exchangeable, add the covariate mean and variance parameters
    } else if (model$model$regressor$coefficient == "exchangeable") {
      exchangeable_mean_var <- round(model$model$data$om.scale^2, digits = 1)
      exchangeable_sd_upper <- round(model$model$data$om.scale, digits = 1)
      prior_table <- rbind(
        prior_table,
        data.frame(
          parameter = c(
            "Covariate mean",
            "Covariate standard deviation"
          ),
          value = c(
            paste0(" ~ Scaled t-distribution (0, ", exchangeable_mean_var, ", 1)"),
            paste0(" ~ Unif (0, ", exchangeable_sd_upper, ")")
          )
        )
      )
      #If the model is NMR unrelated, add the covariate parameters
    } else if (model$model$regressor$coefficient == "unrelated") {
      unrelated_var <- RoundForDisplay(model$model$data$om.scale^2)
      prior_table <- rbind(
        prior_table,
        data.frame(
          parameter = "Covariate parameters",
          value = paste0(" ~ Scaled t-distribution (0, ", unrelated_var, ", 1)")
        )
      )
    }
  }
  return(prior_table)
}

#' MCMC characteristics from a BNMA model.
#'
#' @param model BNMA model output.
#' @return Data frame with four MCMC characteristics.
GetBnmaMcmcCharacteristics <- function(model) {
  return(
    data.frame(
      characteristic = c("Chains",
                         "Burn-in iterations",
                         "Sample iterations",
                         "Thinning factor"),
      value = c(length(model$samples),
                model$burnin,
                length(model$samples[[1]][, 1]),
                model$n.thin)
    )
  )
}


#' Prior distributions from a BNMA model.
#'
#' @param model BNMA model output.
#' @return Data frame with prior distribution information.
GetBnmaPriors <- function(model) {
  treatment_effect_mean <- model$network$prior.data$mean.d
  treatment_effect_var <- round(1 / model$network$prior.data$prec.d, digits = 1)
  eta_mean <- model$network$prior.data$mean.Eta
  eta_var <- round(1 / model$network$prior.data$prec.Eta, digits = 1)
  prior_table <- data.frame(
    parameter = c("Relative treatment effects",
                  "Intercepts"),
    value = c(paste0(" ~ N (", treatment_effect_mean, ", ", treatment_effect_var, ")"),
              paste0(" ~ N (", eta_mean, ", ", eta_var, ")"))
  )

  #If the model is random effects, add the heterogenity SD
  if (model$network$type == "random") {
    heterogeneity_sd_lower <- round(model$network$prior.data$hy.prior.1, digits = 1)
    heterogeneity_sd_upper <- round(model$network$prior.data$hy.prior.2, digits = 1)
    prior_table <- rbind(
      prior_table,
      data.frame(
        parameter = "Heterogeneity standard deviation",
        value = paste0(" ~ Unif (",heterogeneity_sd_lower, ", ", heterogeneity_sd_upper, ")")
      )
    )
  }

  #If the model is NMR shared, add the covariate parameter
  if (model$network$baseline == "common") {
    shared_mean <- model$network$prior.data$mean.bl
    shared_var <- round(1 / model$network$prior.data$prec.bl, digits = 1)
    prior_table <- rbind(
      prior_table,
      data.frame(
        parameter = "Shared covariate parameter",
        value = paste0(" ~ N (", shared_mean, ", ", shared_var, ")")
      )
    )
    #If the model is NMR exchangeable, add the covariate mean and variance parameters
  } else if (model$network$baseline == "exchangeable") {
    exchangeable_mean_mean <- model$network$prior.data$mean.bl
    exchangeable_mean_var <- round(1 / model$network$prior.data$prec.bl, digits = 1)
    exchangeable_sd_lower <- model$network$prior.data$hy.prior.bl.1
    exchangeable_sd_upper <- model$network$prior.data$hy.prior.bl.2
    prior_table <- rbind(
      prior_table,
      data.frame(
        parameter = c("Covariate mean",
                      "Covariate standard deviation"),
        value = c(paste0(" ~ N (", exchangeable_mean_mean, ", ", exchangeable_mean_var, ")"),
                  paste0(" ~ Unif (", exchangeable_sd_lower, ", ", exchangeable_sd_upper, ")"))
      )
    )
    #If the model is NMR unrelated, add the covariate parameters
  } else if (model$network$baseline == "independent") {
    unrelated_mean <- model$network$prior.data$mean.bl
    unrelated_var <- round(1 / model$network$prior.data$prec.bl, digits = 1)
    prior_table <- rbind(
      prior_table,
      data.frame(
        parameter = "Covariate parameters",
        value = paste0(" ~ N (", unrelated_mean, ", ", unrelated_var, ")")
      )
    )
  }
  return(prior_table)
}

#' Returns a number rounded in a nice manner.
#'
#' @param number Any number.
#' @return 'number' with 1 decimal place or 1 significant figure.
RoundForDisplay <- function(number) {
  rounded <- round(number, digits = 1)
  if (rounded == 0) {
    return(signif(number, digits = 1))
  } else {
    return(rounded)
  }
}
