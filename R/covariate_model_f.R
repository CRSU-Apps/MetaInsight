#' @title covariate_model
#' @description Fits a covariate regression model using \CRANpkg{gemtc}
#' @param covariate_value numeric. The value at which to fit the model. Must be greater
#' than or equal to the minimum value and less than or equal to the maximum
#' value in `connected_data`
#' @param regressor_type character. Type of regression coefficient, either `shared`, `unrelated`, or `exchangeable`
#' @param covariate_type character. Whether the covariate values are `Continuous` or `Binary`
#' @param covariate_model_output list. The output of the function. Default `NULL`.
#' When supplied, only the output is recalculated for a given covariate value,
#' rather than refitting the model.
#' @inheritParams common_params
#' @return List of \CRANpkg{gemtc} related output:
#'  \item{mtcResults}{model object from `gemtc::mtc.run()` carried through (needed to match existing code)}
#'  \item{mtcRelEffects}{data relating to presenting relative effects}
#'  \item{rel_eff_tbl}{table of relative effects for each comparison}
#'  \item{covariate_value}{The covariate value originally passed into this function}
#'  \item{reference_treatment}{The name of the reference treatment}
#'  \item{comparator_names}{Vector containing the names of the comparators}
#'  \item{a}{text output stating whether fixed or random effects}
#'  \item{sumresults}{summary output of relative effects}
#'  \item{dic}{data frame of model fit statistics}
#'  \item{cov_value_sentence}{text output stating the value for which the covariate has been set to for producing output}
#'  \item{slopes}{named list of slopes for the regression equations (unstandardised - equal to one 'increment')}
#'  \item{intercepts}{named list of intercepts for the regression equations at covariate_value}
#'  \item{outcome}{The outcome type for the analysis eg. "MD" or "OR"}
#'  \item{mtcNetwork}{The network object from GEMTC}
#'  \item{model}{The type of linear model, either "fixed" or "random"}
#'  \item{covariate_min}{Vector of minimum covariate values directly contributing to the regression}
#'  \item{covariate_max}{Vector of maximum covariate values directly contributing to the regression}
#' @export
covariate_model <- function(connected_data,
                            treatment_df,
                            outcome,
                            outcome_measure,
                            covariate_value,
                            model_type,
                            regressor_type,
                            covariate_type,
                            reference_treatment,
                            seed,
                            covariate_model_output = NULL,
                            async = FALSE){

  if (!async){ # only an issue if run outside the app
    if (check_param_classes(c("connected_data", "treatment_df", "outcome", "outcome_measure", "covariate_value",
                              "model_type",  "reference_treatment", "regressor_type", "covariate_type", "seed"),
                            c("data.frame", "data.frame", "character", "character", "numeric",
                              "character", "character", "character", "character", "numeric"), NULL)){
      return()
    }
  }

  if (!any(grepl("covar\\.", names(connected_data)))){
    return(async |> asyncLog(type = "error", "connected_data does not contain a covariate column"))
  }

  if (!outcome %in% c("Binary", "Continuous")){
    return(async |> asyncLog(type = "error", "outcome must be 'Binary' or 'Continuous'"))
  }

  if (!covariate_type %in% c("Binary", "Continuous")){
    return(async |> asyncLog(type = "error", "covariate_type must be 'Binary' or 'Continuous'"))
  }

  if (!model_type %in% c("fixed", "random")){
    return(async |> asyncLog(type = "error", "model_type must be 'fixed' or 'random'"))
  }

  if (!outcome_measure %in% c("OR", "RR", "MD")){
    return(async |> asyncLog(type = "error", "outcome_measure must be 'OR', 'RR' or 'MD'"))
  }

  covariate <- FindCovariateNames(connected_data)

  if (covariate_value < min(connected_data[[covariate]], na.rm = TRUE)){
    return(async |> asyncLog(type = "error", "covariate_value must not be lower than the minimum covariate value in connected_data"))
  }

  if (covariate_value > max(connected_data[[covariate]], na.rm = TRUE)){
    return(async |> asyncLog(type = "error", "covariate_value must not be higher than the maximum covariate value in connected_data"))
  }

  if (!regressor_type %in% c("shared", "unrelated", "exchangeable")){
    return(async |> asyncLog(type = "error", "regressor_type must be 'shared', 'unrelated', or 'exchangeable'"))
  }

  if (!reference_treatment %in% treatment_df$Label){
    return(async |> asyncLog(type = "error", "reference_treatment must be one of the treatments in treatment_df"))
  }

  cov_friendly <- GetFriendlyCovariateName(covariate)

  # only run these parts if it hasn't run before, or if the regressor_type has changed
  if (is.null(covariate_model_output) || covariate_model_output$mtcResults$model$regressor$coefficient != regressor_type){
    prepped_data <- PrepDataGemtc(connected_data,
                                  treatment_df,
                                  outcome,
                                  covariate,
                                  cov_friendly)

    gemtc_model <- CreateGemtcModel(prepped_data,
                                    model_type,
                                    outcome_measure,
                                    regressor_type,
                                    reference_treatment,
                                    seed)

    model_output <- suppress_jags_output(gemtc::mtc.run(gemtc_model))

    model_info <- CovariateModelOutput(connected_data,
                                       treatment_df,
                                       model_output,
                                       covariate,
                                       covariate_value,
                                       outcome_measure,
                                       covariate_type)
  } else {
    model_info <- CovariateModelOutput(connected_data,
                                       treatment_df,
                                       covariate_model_output$mtcResults,
                                       covariate,
                                       covariate_value,
                                       outcome_measure,
                                       covariate_type)
  }

  class(model_info) <- c("bayes_model", "covariate_model")

  return(model_info)
}

#' Function for formatting standard app user data ready for start of \CRANpkg{gemtc} chain of commands
#'
#' @param data Uploaded data post-processing
#' @param treatment_df Data frame containing treatment IDs and names in columns named 'Number' and 'Label' respectively
#' @param outcome Indicator of whether data is 'Binary' or 'Continuous'
#' @param covariate Chosen covariate name as per uploaded data
#' @param cov_friendly Friendly name of chosen covariate
#' @return list containing two dataframes: armData containing the core data; studyData containing covariate data
PrepDataGemtc <- function(data, treatment_df, outcome, covariate, cov_friendly){
  # ensure data is in long format
  if (FindDataShape(data) == "wide") {
    long_data <- WideToLong(data, outcome)
  } else {
    long_data <- data
  }
  # specify arm level data
  if (outcome == "Continuous") {
    armData <- data.frame(study = long_data$Study,
                          treatment = treatment_df$Label[match(long_data$T, treatment_df$Number)],
                          mean = long_data$Mean,
                          std.dev = long_data$SD,
                          sampleSize = long_data$N)
  } else if (outcome == "Binary") {
    armData <- data.frame(study = long_data$Study,
                          treatment = treatment_df$Label[match(long_data$T, treatment_df$Number)],
                          responders = long_data$R,
                          sampleSize = long_data$N)
  } else {
    paste0("Outcome has to be 'Continuous' or 'Binary'")
  }
  # specify study level data
  studyData <- unique(data.frame(study = long_data$Study,
                                 covariate = long_data[, covariate]))
  names(studyData)[2] <- cov_friendly
  rownames(studyData) <- NULL

  return(list(armData = armData, studyData = studyData))
}

#' Function for setting up covariate \CRANpkg{gemtc} model object
#'
#' @param data list containing armData and studyData (as created by PrepDataGemtc)
#' @param model_type Whether the model is 'fixed' or 'random'
#' @param outcome_measure Type of outcome measure ('OR', 'RR', or 'MD')
#' @param regressor_type Type of regression coefficient, either "shared", "unrelated", or "exchangeable"
#' @param ref_choice The choice of reference treatment as selected by user
#' @param seed Seed value to use when fitting model
#' @return An object of class mtc.model
CreateGemtcModel <- function(data, model_type, outcome_measure, regressor_type, ref_choice, seed) {
  # Create 'network' object
  network_object <- gemtc::mtc.network(data.ab = data$armData,
                                       studies = data$studyData)
  # Define regression coefficient
  regressor <- list(coefficient=regressor_type,
                    variable=colnames(data$studyData[2]),
                    control=ref_choice)

  if (outcome_measure == "MD") {
    like <- "normal"
    link <- "identity"
  } else if (outcome_measure %in% c('OR', 'RR')) {
    like <- "binom"
    if (outcome_measure == "OR") {
      link <- "logit"
    } else if (outcome_measure == "RR") {
      link <- "log"
    }
  } else {
    paste0("Outcome can only be OR, RR, or MD")
  }

  # use same RNG inside and outside of mirai
  RNGkind("L'Ecuyer-CMRG")
  # needs to be set before mtc.model
  set.seed(seed)

  # Create 'model' object
  model_object <- gemtc::mtc.model(network_object,
                                   type = "regression",
                                   likelihood=like,
                                   link = link,
                                   linearModel = model_type,
                                   regressor = regressor)
  # Settings for JAGS seeds and generator types for reproducible results (code taken from mtc.model manual)
  seeds <- sample.int(4, n = .Machine$integer.max) # 4 chains
  model_object$inits <- mapply(c, model_object$inits, list(
    list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = seeds[1]),
    list(.RNG.name = "base::Marsaglia-Multicarry", .RNG.seed = seeds[2]),
    list(.RNG.name = "base::Super-Duper", .RNG.seed = seeds[3]),
    list(.RNG.name = "base::Mersenne-Twister", .RNG.seed = seeds[4])),
    SIMPLIFY = FALSE)

  return(model_object)
}

#' Function to collate all the model output to be used in other existing functions
#'
#' @param connected_data Data frame from which model was calculated
#' @param treatment_df Data frame containing treatment IDs (Number) and names (Label)
#' @param model Completed model object after running RunCovariateRegression()
#' @param covariate_title Covariate name as per uploaded data
#' @param covariate_value Value of covariate for which to give output (default value the mean of study covariates)
#' @param outcome_measure The outcome measure for the analysis: One of: "OR", "RR", "MD"
#' @param covariate_type character. Whether the covariate values are `Continuous` or `Binary`
#' @return List of gemtc related output:
#'  mtcResults = model object itself carried through (needed to match existing code)
#'  mtcRelEffects = data relating to presenting relative effects;
#'  rel_eff_tbl = table of relative effects for each comparison;
#'  covariate_value = The covariate value originally passed into this function
#'  reference_treatment = The name of the reference treatment
#'  comparator_names = Vector containing the names of the comparators.
#'  a = text output stating whether fixed or random effects;
#'  sumresults = summary output of relative effects
#'  dic = data frame of model fit statistics
#'  cov_value_sentence = text output stating the value for which the covariate has been set to for producing output
#'  slopes = named list of slopes for the regression equations (unstandardised - equal to one 'increment')
#'  intercepts = named list of intercepts for the regression equations at covariate_value
#'  outcome = The outcome type for the analysis eg. "MD" or "OR"
#'  mtcNetwork = The network object from GEMTC
#'  model = The type of linear model, either "fixed" or "random"
#'  covariate_min = Vector of minimum covariate values directly contributing to the regression.
#'  covariate_max = Vector of maximum covariate values directly contributing to the regression.
CovariateModelOutput <- function(connected_data, treatment_df, model, covariate_title, covariate_value, outcome_measure, covariate_type) {

  model_levels <- levels(model$model$data$reg.control)
  reference_treatment <- model_levels[model_levels %in% model$model$data$reg.control]
  comparator_names <- model_levels[!model_levels %in% model$model$data$reg.control]

  # If the covariate type has been selected as continuous and gemtc has inferred it as binary, overwrite it
  if (covariate_type == "Continuous" & model$model$regressor$type == "binary") {
    model$model$regressor$type <- "continuous"
  }

  # Create text for random/fixed effect
  model_text <- paste(model$model$linearModel, "effect", sep = " ")

  # Relative Effects raw data
  rel_eff <- gemtc::relative.effect(model, as.character(model$model$regressor$control), covariate = covariate_value)

  # Relative Effects table of all comparisons
  rel_eff_tbl <- gemtc::relative.effect.table(model, covariate = covariate_value)

  # Table of Model fit stats
  fit_stats <- as.data.frame(summary(model)$DIC)

  # Summary sentence of where covariate value has been set for results
  cov_value_sentence <- paste0("Value for covariate ", model$model$regressor$variable, " set at ", covariate_value)

  # Obtain slope(s)
  slope_indices <- grep(ifelse(model$model$regressor$coefficient == "shared", "^B$", "^beta\\[[0-9]+\\]$"), model$model$monitors$enabled)
  model_summary <- summary(model)
  slopes <- model_summary$summaries$quantiles[slope_indices, "50%"] / model$model$regressor$scale

  # Duplicate slope for each comparator when "shared" type
  if (model$model$regressor$coefficient == "shared") {
    slopes <- rep(slopes[1], length(comparator_names))
  }

  # Intercepts (regression)
  rel_eff_zero <- gemtc::relative.effect(model, as.character(model$model$regressor$control), covariate = 0)
  rel_eff_zero_summary <- summary(rel_eff_zero)
  intercepts <- rel_eff_zero_summary$summaries$quantiles[startsWith(rownames(rel_eff_zero_summary$summaries$quantiles), "d."), "50%"]

  # Rename items for intercepts and slopes
  names(slopes) <- comparator_names
  names(intercepts) <- comparator_names

  min_max <- FindCovariateRanges(
    connected_data = connected_data,
    treatment_df = treatment_df,
    reference_treatment = as.character(model$model$regressor$control),
    covariate_title = covariate_title
  )

  # Summary of relative effects
  rel_eff_summary <- summary(rel_eff)

  # Find indices of beta parameters
  beta_indices <- grep("^B$|^beta\\[[0-9]+\\]$",
                       model$model$monitors$enabled,
                       value = TRUE)
  # Add betas to relative effect summary
  beta_statistics <- model_summary$summaries$statistics[beta_indices, ]
  beta_quantiles <- model_summary$summaries$quantiles[beta_indices, ]
  rel_eff_summary$summaries$statistics <- rbind(rel_eff_summary$summaries$statistics, beta_statistics)
  rel_eff_summary$summaries$quantiles <- rbind(rel_eff_summary$summaries$quantiles, beta_quantiles)
  # Correct parameter name in shared case, when only one row has been appended
  if (model$model$regressor$coefficient == "shared") {
    rownames(rel_eff_summary$summaries$statistics)[rownames(rel_eff_summary$summaries$statistics) == "beta_statistics"] <- "B"
    rownames(rel_eff_summary$summaries$quantiles)[rownames(rel_eff_summary$summaries$quantiles) == "beta_quantiles"] <- "B"
  }

  # Add text about the scaling of covariate values
  rel_eff_summary$covariate <- paste0(rel_eff_summary$covariate, "\nThe covariate values have been scaled (divided by ", round(model$model$regressor$scale, digits = 2), ") in order to have standard deviation 0.5. \n - The interpretation of covariate parameters should change accordingly.")

  # naming conventions to match current Bayesian functions
  return(
    list(
      mtcResults = model,
      mtcRelEffects = rel_eff,
      rel_eff_tbl = rel_eff_tbl,
      covariate_value = covariate_value,
      reference_treatment = reference_treatment,
      comparator_names = comparator_names,
      a = model_text,
      sumresults = rel_eff_summary,
      dic = fit_stats,
      cov_value_sentence = cov_value_sentence,
      slopes = slopes,
      intercepts = intercepts,
      outcome = outcome,
      outcome_measure = outcome_measure,
      mtcNetwork = model$model$network, # why duplicate mtcResults?
      model_type = model$model$linearModel,
      covariate_min = min_max$min,
      covariate_max = min_max$max
    )
  )
}

#' Find the lowest and highest covariate values given by a study comparing the reference and comparator treatments.
#' This does noes not include studies which do not directly compare treatments to the reference.
#'
#' @param connected_data Data frame from which to find covariate ranges
#' @param treatment_df data frame containing treatment names ("Label") and IDs ("Number")
#' @param reference_treatment Name of reference treatment.
#' @param covariate_title Title of covariate column in data. Only required when @param baseline_risk == FALSE.
#' @param baseline_risk TRUE if the covariate is baseline risk. Defaults to FALSE.
#' @param outcome "Binary" or "Continuous". Only required when @param baseline_risk == TRUE. Defaults to NULL.
#' @param model Model created by bnma::network.run(). Only required when @param baseline_risk == TRUE. Defaults to NULL.
#'
#' @return The lowest and highest covariate values of relevant studies. This is structured as a list containing 2 items:
#' - "min" a named vector of the lowest values, where the names are the treatment names.
#' - "max" a named vector of the highest values, where the names are the treatment names.
#' @export
FindCovariateRanges <- function(connected_data, treatment_df, reference_treatment, covariate_title, baseline_risk = FALSE, outcome = NULL, model = NULL) {

  studies <- unique(connected_data$Study)

  study_treatments <- sapply(
    studies,
    function(study) {
      FindAllTreatments(connected_data, treatment_df, study)
    }
  )

  # Turn list into matrix
  # This is only needed when there are different numbers of treatment arms between studies
  if (is.list(study_treatments)) {
    max_treatments <- max(lengths(study_treatments))

    temp_matrix <- matrix(
      nrow = max_treatments,
      ncol = length(studies)
    )
    colnames(temp_matrix) <- studies

    sapply(
      studies,
      function(study) {
        treatments <- study_treatments[[study]]
        temp_matrix[1:length(treatments), study] <<- treatments
      }
    )

    study_treatments <- temp_matrix
  }

  colnames(study_treatments) <- studies

  non_reference_treatment_names <- treatment_df$Label[treatment_df$Label != reference_treatment]

  if (baseline_risk) {
    baseline_risk_covariate <- GetReferenceOutcome(connected_data = connected_data,
                                                   treatment_df = treatment_df,
                                                   outcome = outcome,
                                                   observed = "Imputed",
                                                   model = model)
  }

  treatment_min <- c()
  treatment_max <- c()
  for (treatment_name in non_reference_treatment_names) {
    min <- NA
    max <- NA
    for (study in studies) {

      if (treatment_name %in% study_treatments[, study] && reference_treatment %in% study_treatments[, study]) {
        if (!baseline_risk) {
          covariate_value <- connected_data[[covariate_title]][connected_data$Study == study][1]
        } else {
          covariate_value <- baseline_risk_covariate[study]
        }

        if (is.na(min) || min > covariate_value) {
          min <- covariate_value
        }

        if (is.na(max) || max < covariate_value) {
          max <- covariate_value
        }
      }
    }
    treatment_min[[treatment_name]] <- unname(min)
    treatment_max[[treatment_name]] <- unname(max)
  }

  return(
    list(
      min = unlist(treatment_min),
      max = unlist(treatment_max)
    )
  )
}

#' Get the outcome in the reference arm.
#'
#' @param connected_data Data in long format.
#' @param treatment_df Data frame containing treatment IDs and names in columns named 'Number' and 'Label' respectively.
#' @param outcome "Binary" or "Continuous".
#' @param observed "Observed" or "Imputed". See @return.
#' @param model Model created by bnma::network.run(). Only required when @param observed == "Imputed". Defaults to NULL.
#'
#' @return Vector of reference arm outcomes, named by study.
#'   If a study contains the reference treatment then the value returned is the observed outcome in the reference arm.
#'   If a study does not contain the reference treatment then the value returned is:
#'     - NA if @param observed == "Observed";
#'     - The median of the study-specific intercept parameter if @param observed == "Imputed".
GetReferenceOutcome <- function(connected_data, treatment_df, outcome, observed, model = NULL){

  if (!(observed %in% c("Observed", "Imputed"))) {
    stop("'observed' must be 'Observed' or 'Imputed'")
  }

  if (observed == "Imputed" && is.null(model)) {
    stop("A model must be provided when 'observed' == 'Imputed'")
  }

  treatments <- treatment_df$Label
  connected_data$Treatment <- treatments[match(connected_data$T, treatment_df$Number)]

  #Data with only control treatment rows kept
  data_control <- KeepOrDeleteControlTreatment(data = connected_data, treatments = treatments, keep_delete = "keep")
  if (outcome == "Binary"){
    #Add NAs as required by metafor::escalc()
    data_control$R[data_control$Treatment != treatments[1]] <- NA
    effect_sizes <- metafor::escalc(measure = "PLO",
                                    xi = data_control$R,
                                    ni = data_control$N)
  } else if (outcome == "Continuous"){
    #Add NAs as required by metafor::escalc()
    data_control$Mean[data_control$Treatment != treatments[1]] <- NA
    effect_sizes <- metafor::escalc(measure = "MN",
                                    mi = data_control$Mean,
                                    sdi = data_control$SD,
                                    ni = data_control$N)
  } else{
    stop("'outcome' must be 'Continuous' or 'Binary'")
  }
  outcomes <- as.numeric(effect_sizes$yi)
  names(outcomes) <- unique(data_control$Study)

  #If imputed values are requested and there are any missing outcomes then use the imputed outcomes
  if (observed == "Imputed" && any(is.na(outcomes))) {
    #Eta is the study-specific intercept parameter, which is often called mu in the literature. When a study does not contain the reference arm, {bnma} adds the reference arm to the data with missing values. Eta is then imputed for that study.
    imputed_outcomes <- MCMCvis::MCMCsummary(object = model$samples, params = "Eta")["50%"][[1]]
    names(imputed_outcomes) <- model$network$Study.order

    #The studies with missing outcomes
    na_studies <- names(outcomes[is.na(outcomes)])

    outcomes[na_studies] <- imputed_outcomes[na_studies]
  }

  return(outcomes)
}

