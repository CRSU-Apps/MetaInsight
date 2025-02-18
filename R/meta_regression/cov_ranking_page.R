
#' Module UI for the covariate ranking page.
#' 
#' @param id ID of the module.
#' @return Div for the page.
covariate_ranking_page_ui <- function(id) {
  ns <- NS(id)
  div(
    helpText(
      tags$strong("Please note it may take up to 5 minutes to load the results.", style = "color:#FF0000"),
      tags$br(),
      tags$strong(
        "IMPORTANT: If you export and include the Litmus Rank-O-Gram or the Radial SUCRA plot in your work, please cite it as:",
        style = "color:#4863A0"
      ),
      tags$a(
        href = "https://doi.org/10.1016/j.jclinepi.2023.02.016",
        "Nevill CR, Cooper NJ, Sutton AJ, A multifaceted graphical display, including treatment ranking, was developed to aid interpretation of network meta-analysis,
        Journal of Clinical Epidemiology (2023)"
      )
    ),
    fluidRow(
      ranking_panel_ui(id = ns("rank_all"), title = "Ranking panel for all studies", table_label = "Ranking probabilities and SUCRA values for all treatments")
    )
  )
}


#' Module server for the covariate ranking page.
#' 
#' @param id ID of the module.
#' @param model Reactive containing bayesian meta-analysis for all studies.
#' @param data Reactive containing data to analyse.
#' @param treatment_df Reactive containing data frame containing treatment IDs (Number) and names (Label).
#' @param metaoutcome Reactive containing meta analysis outcome: "Continuous" or "Binary".
#' @param outcome_measure Reactive containing meta analysis outcome measure: "MD", "SMD", "OR, "RR", or "RD".
#' @param model_effects Reactive containing model effects: either "random" or "fixed".
#' @param rank_option Reactive containing ranking option: "good" or "bad" depending on whether small values are desirable or not.
#' @param freq_all Reactive containing frequentist meta-analysis.
#' @param bugsnetdt Reactive containing bugsnet meta-analysis.
#' @param model_valid Reactive containing whether the model is valid.
#' @param cov_value Value of covariate for regression analysis.
#' @param package "gemtc" or "bnma". Defaults to "gemtc".
#' regression_text Annotation text for regression model plots.
covariate_ranking_page_server <- function(
    id,
    model,
    data,
    treatment_df,
    metaoutcome,
    outcome_measure,
    model_effects,
    rank_option,
    freq_all,
    bugsnetdt,
    model_valid,
    cov_value = reactive({NA}),
    package = "gemtc"
    ) {
  
  moduleServer(id, function(input, output, session) {
    
    if (package == "gemtc"){
      ranking_panel_server(
        id = "rank_all",
        data = data,
        treatment_df = treatment_df,
        model = model,
        metaoutcome = metaoutcome,
        rank_option = rank_option,
        frequentist = freq_all,
        bugsnetdt = bugsnetdt,
        model_valid = model_valid,
        filename_prefix = "regression_",
        title_prefix = "Regression analysis",
        cov_value = cov_value
      )
    } else if(package == "bnma"){
      ranking_panel_server(
        id = "rank_all",
        data = data,
        treatment_df = treatment_df,
        model = model,
        metaoutcome = metaoutcome,
        rank_option = rank_option,
        frequentist = freq_all,
        bugsnetdt = bugsnetdt,
        model_valid = model_valid,
        filename_prefix = "baseline_risk_",
        title_prefix = "Baseline risk",
        cov_value = cov_value,
        package = package
      )
    } else{
      stop("package must be 'gemtc' or 'bnma'")
    }
  })
}

