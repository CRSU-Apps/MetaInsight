#' Run the nodesplit model
#'
#' @inheritParams common_params
#'
#' @return `mtc.nodesplit` object containing an mtc.result object for each node
#' @export
bayes_nodesplit <- function(connected_data, treatment_df, outcome, outcome_measure, model_type, async = FALSE) {

  if (!async){ # only an issue if run outside the app
    if (check_param_classes(c("connected_data", "treatment_df", "outcome", "outcome_measure", "model_type"),
                            c("data.frame", "data.frame", "character", "character", "character"), NULL)){
      return()
    }
  }

  if (!outcome %in% c("Binary", "Continuous")){
    return(async |> asyncLog(type = "error", "outcome must be either Binary or Continuous"))
  }
  if (!model_type %in% c("fixed", "random")){
    return(async |> asyncLog(type = "error", "model_type must be 'fixed' or 'random'"))
  }
  if (outcome_measure == "SMD" ) {
    return(async |> asyncLog(type = "error", "Standardised mean difference currently cannot be analysed in Bayesian analysis"))
  }
  else if (outcome_measure == "RD") {
    return(async |> asyncLog(type = "error", "Bayesian analysis of risk differences is not currently implemented in MetaInsight"))
  }
  if (!outcome_measure %in% c('OR', 'RR', 'MD')) {
    return(async |> asyncLog(type = "error", glue::glue("Outcome_measure type '{outcome_measure}' is not supported. Please use one of: 'MD', 'OR', 'RR'")))
  }

  data <- dataform.df(connected_data, treatment_df, outcome)

  if (outcome == "Continuous") {
    armData <- data.frame(
      study = data$Study,
      treatment = data$T,
      mean = data$Mean,
      std.err = data$se
    )
  } else {
    armData <- data.frame(
      study = data$Study,
      treatment = data$T,
      responders = data$R,
      sampleSize = data$N
    )
  }
  mtcNetwork <- suppress_jags_output(
    gemtc::mtc.network(
      data.ab = armData,
      description = "Network",
      treatments = data.frame(
        id = treatment_df$Label,
        description = treatment_df$Label
        )
      )
    )

  if (outcome_measure == "MD") {
    like <- "normal"
    link <- "identity"
  } else  {
    like <- "binom"
    link <- ifelse (outcome_measure == "OR", "logit", "log")
  }

  check_nodesplit <- IsNodesplittable(
    data = data,
    treatments = treatment_df$Label
  )

  if (!check_nodesplit$is_nodesplittable) {
    return(
      async |> asyncLog(
        type = "error",
        check_nodesplit$reason
      )
    )
  } else {
    return(
      suppress_jags_output(
        gemtc::mtc.nodesplit(
          network = mtcNetwork,
          linearModel = model_type,
          likelihood = like,
          link = link
        )
      )
    )
  }
}

#' Produce a forest plot from nodesplitting results
#'
#' @param nodesplit `mtc.nodesplit` object produced by `bayes_nodesplit()`
#' @param main_analysis logical. Whether the analysis is the main or sensitivity analysis. Default `TRUE`.
#' @inheritParams common_params
#' @inherit return-svg return
#' @export
bayes_nodesplit_plot <- function(nodesplit, main_analysis = TRUE, logger = NULL){

  if (!inherits(nodesplit, "mtc.nodesplit")){
    logger |> writeLog(type = "error", "nodesplit must be a 'mtc.nodesplit' object")
  }

  if (!inherits(main_analysis, "logical")){
    logger |> writeLog(type = "error", "main_analysis must be either 'TRUE' or 'FALSE'")
  }

  plot_height <- max(400, length(nodesplit) * 80) / 72
  plot_title = paste0("Inconsistency test with nodesplitting \nmodel",
                      ifelse(main_analysis, " for all studies", " with selected studies excluded"))

  svglite::xmlSVG({
    plot(summary(nodesplit), digits = 3)
    title(main = plot_title)
  },
  width = 8,
  height = plot_height
  ) |> crop_svg()

}

