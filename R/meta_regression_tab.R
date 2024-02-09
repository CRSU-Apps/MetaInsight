#' Create the meta-regression panel.
#'
#' @param id ID of the module
#' @return Div containing the module
meta_regression_tab_ui <- function(id, page_numbering) {
  ns <- NS(id)
  
  page_numbering$DiveLevel()
  
  ui = div(
    fluidPage(
      tabsetPanel(
        id = "regression_tabs",
        tabPanel(
          title = paste0(page_numbering$AddChild(), " Summary"),
          metaregression_summary_panel_ui(id = ns("metaregression_summary_panel"))
        ),
        tabPanel(
          title = paste0(page_numbering$AddChild(), " Baseline Risk Analysis"),
          informed_conditional_panel_ui(
            id = ns("baseline_risk_outcome_dependent"),
            inner_ui_expression = { baseline_risk_analysis_panel_ui(id = ns("baseline_risk_analysis"), page_numbering) },
            args = list(page_numbering = page_numbering)
          )
        ),
        tabPanel(
          title = paste0(page_numbering$AddChild(), " Covariate Analysis"),
          informed_conditional_panel_ui(
            id = ns("covariate_presence_dependent"),
            inner_ui_expression = {
              informed_conditional_panel_ui(
                id = ns("covariate_outcome_dependent"),
                inner_ui_expression = { covariate_analysis_panel_ui(id = ns("covariate_analysis"), page_numbering) },
                args = list(page_numbering = page_numbering)
              )
            },
            args = list(page_numbering = page_numbering)
          )
        )
      )
    )
  )
  
  page_numbering$FloatLevel()
  
  return(ui)
}

#' Build the text to inform the user that the outcome measure is not supported.
#'
#' @param supported_measures Vector of outcome measure which are supported.
#'
#' @return Built text.
.BuildUnsupportedOutcomeMeasureErrorMessageText <- function(supported_measures) {
  return(
    paste0(
      "Outcome measure is not supported for this analysis. Supported types are: ",
      paste0(supported_measures, collapse = ", ")
    )
  )
}

#' Build a div containing bold, italic red text informing the user that no covariate data has been loaded.
#'
#' @return Built div.
.BuildMissingCovariateErrorMessageUi <- function() {
  return(
    div(
      h3(
        "No covariate data. To add covariate data, add a column titled",
        code("covar.*"),
        "where the",
        code("*"),
        "is replaced by the covariate name. eg. ",
        code("covar.age")
      ),
      style = "color: red; font-style: italic; font-weight: bold;"
    )
  )
}

#' Create the meta-regression server.
#'
#' @param id ID of the module
#' @param all_data Study data including covariate columns, in wide or long format
#' @param treatment_df Reactive containing data frame containing treatment IDs (Number) and names (Label)
#' @param reference_treatment Reactive containing the sanitised name of reference treatment
#' @param metaoutcome Reactive containing meta analysis outcome: "Continuous" or "Binary"
#' @param outcome_measure Reactive containing meta analysis outcome measure: "MD", "SMD", "OR, "RR", or "RD"
#' @param model_effects Reactive containing model effects: either "random" or "fixed"
#' @param rank_option Reactive containing ranking option: "good" or "bad" depending on whether small values are desirable or not
#' @param freq_all Reactive containing frequentist meta-analysis
#' @param bugsnetdt Reactive containing bugsnet meta-analysis
meta_regression_tab_server <- function(
    id, 
    all_data,
    treatment_df,
    reference_treatment,
    metaoutcome,
    outcome_measure,
    model_effects,
    rank_option,
    freq_all,
    bugsnetdt
    ) {
  shiny::moduleServer(id, function(input, output, session) {
    
    metaregression_summary_panel_server(id = "metaregression_summary_panel", 
                                        all_data = all_data, 
                                        metaoutcome = metaoutcome, 
                                        treatment_df = treatment_df)
    
    basline_risk_outcomes <- c("MD", "OR")
    baseline_risk_supported = reactive({ outcome_measure() %in% basline_risk_outcomes })
    # Baseline risk analysis
    informed_conditional_panel_server(
      id = "baseline_risk_outcome_dependent",
      condition = baseline_risk_supported,
      error_message_text_expression = { .BuildUnsupportedOutcomeMeasureErrorMessageText(basline_risk_outcomes) },
      inner_server_expression = {
        baseline_risk_analysis_panel_server(
          id = "baseline_risk_analysis",
          all_data = all_data,
          treatment_df = treatment_df,
          reference_treatment = reference_treatment,
          metaoutcome = metaoutcome,
          outcome_measure = outcome_measure,
          model_effects = model_effects,
          bugsnetdt = bugsnetdt
        )
      }
    )
    
    covariate_data_present = reactive({ length(FindCovariateNames(all_data())) > 0 })
    covariate_outcomes <- c("MD", "OR", "RR")
    covariate_supported = reactive({ outcome_measure() %in% covariate_outcomes })
    # Covariate analysis
    informed_conditional_panel_server(
      id = "covariate_presence_dependent",
      condition = covariate_data_present,
      error_message_ui_expression = { .BuildMissingCovariateErrorMessageUi() },
      inner_server_expression = {
        
        informed_conditional_panel_server(
          id = "covariate_outcome_dependent",
          condition = covariate_supported,
          error_message_text_expression = { .BuildUnsupportedOutcomeMeasureErrorMessageText(covariate_outcomes) },
          inner_server_expression = {
            covariate_analysis_panel_server(
              id = "covariate_analysis",
              all_data = all_data,
              treatment_df = treatment_df,
              reference_treatment = reference_treatment,
              metaoutcome = metaoutcome,
              outcome_measure = outcome_measure,
              model_effects = model_effects,
              rank_option = rank_option,
              freq_all = freq_all,
              bugsnetdt = bugsnetdt
            )
          }
        )
        
      }
    )
  })
}
