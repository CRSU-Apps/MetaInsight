#' Run Bayesian models
#'
#' @inheritParams common_params
#' @return List containing:
#'  \item{mtcResults}{mtc.result. Output from `gemtc::mtc.run()`}
#'  \item{mtcRelEffects}{mtc.result. Output from `gemtc::relative.effect()`}
#'  \item{rel_eff_tbl}{mtc.relative.effect.table. Output from `gemtc::relative.effect.table()`}
#'  \item{sumresults}{summary.mtc.result. Output from `summary(mtcRelEffects)`}
#'  \item{mtcNetwork}{mtc.network. Output from `gemtc::mtc.network()`}
#'  \item{dic}{dataframe. Containing the statistics 'Dbar', 'pD', 'DIC', and 'data points'}
#'  \item{outcome_measure}{character. The input `outcome_measure`}
#'  \item{model_type}{dataframe. The input `model_type`}
#' @export

bayes_model <- function(configured_data, async = FALSE){

  if (!async){ # only an issue if run outside the app
    if (check_param_classes(c("configured_data"), c("configured_data"), NULL)){
      return()
    }
  }

  if (!configured_data$outcome_measure %in% c("OR", "RR", "MD")){
    return(async |> asyncLog(type = "error", "configured data must have an outcome_measure of 'OR', 'RR' or 'MD'"))
  }

  # use same RNG inside and outside of mirai
  RNGkind("L'Ecuyer-CMRG")
  set.seed(configured_data$seed)

  longsort <- dataform.df(configured_data$connected_data, configured_data$treatments, configured_data$outcome)

  # Create arm level data set for gemtc
  if (configured_data$outcome == "continuous") {
    armData <- data.frame(study = longsort$Study,
                          treatment = longsort$T,
                          mean = longsort$Mean,
                          std.err = longsort$se)
  } else if (configured_data$outcome == "binary") {
    armData <- data.frame(study = longsort$Study,
                          treatment = longsort$T,
                          responders = longsort$R,
                          sampleSize = longsort$N)
  }
  # Gemtc network object
  mtcNetwork <- gemtc::mtc.network(data.ab = armData, description = "Network")

  if (configured_data$outcome_measure == "MD") {
    like <- "normal"
    link <- "identity"
  } else if (configured_data$outcome_measure == "OR" || configured_data$outcome_measure == "RR") {
    like <- "binom"
    link <- ifelse(configured_data$outcome_measure == "OR", "logit", "log")
  }

  mtcModel <- gemtc::mtc.model(
    network = mtcNetwork,
    type = "consistency",
    linearModel = configured_data$effects,
    likelihood = like,
    link = link,
    dic = TRUE
  )

  # Settings for JAGS seeds and generator types for reproducible results (code taken from mtc.model manual)
  seeds <- sample.int(4, n = .Machine$integer.max) # 4 chains
  mtcModel$inits <- mapply(c, mtcModel$inits, list(
    list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = seeds[1]),
    list(.RNG.name = "base::Marsaglia-Multicarry", .RNG.seed = seeds[2]),
    list(.RNG.name = "base::Super-Duper", .RNG.seed = seeds[3]),
    list(.RNG.name = "base::Mersenne-Twister", .RNG.seed = seeds[4])),
    SIMPLIFY = FALSE)

  # Run gemtc model object for analysis
  mtcResults <- suppress_jags_output(gemtc::mtc.run(mtcModel))

  mtcRelEffects <- gemtc::relative.effect(mtcResults, t1 = configured_data$reference_treatment)  #Set reference treatment
  rel_eff_tbl <- gemtc::relative.effect.table(mtcResults)
  sumresults <- summary(mtcRelEffects)
  # a <- paste(model, "effect", sep = " ")   #Create text for random/fixed effect
  sumoverall <- summary(mtcResults)
  dic <- as.data.frame(sumoverall$DIC) # The statistics 'Dbar', 'pD', 'DIC', and 'data points'

  results <- list(
                mtcResults = mtcResults,
                mtcRelEffects = mtcRelEffects,
                rel_eff_tbl = rel_eff_tbl,
                sumresults = sumresults,
                mtcNetwork = mtcNetwork,
                dic = dic,
                # these are stored so that only the model object can be passed to other functions
                outcome = configured_data$outcome,
                outcome_measure = configured_data$outcome_measure,
                reference_treatment = configured_data$reference_treatment,
                effects = configured_data$effects,
                seed = configured_data$seed
             )

  class(results) <- "bayes_model"

  results
}

