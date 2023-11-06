#' Create the meta-regression panel.
#'
#' @param id ID of the module
#' @return Div containing the module
meta_regression_tab_ui <- function(id) {
  ns <- NS(id)
  div(
    fluidPage(
      tabsetPanel(
        id = "regression_tabs",
        tabPanel(
          title = "4a. Summary",
          # Add a new module here for the summary panel
        ),
        tabPanel(
          title = "4b. Baseline Risk Analysis",
          informed_conditional_panel_ui(
            id = ns("baseline_risk_outcome_dependent"),
            inner_ui_expression = { baseline_risk_analysis_panel_ui(id = ns("baseline_risk_analysis")) }
          )
        ),
        tabPanel(
          title = "4c. Covariate Analysis",
          informed_conditional_panel_ui(
            id = ns("covariate_presence_dependent"),
            informed_conditional_panel_ui(
              id = ns("covariate_outcome_dependent"),
              inner_ui_expression = { covariate_analysis_panel_ui(id = ns("covariate_analysis")) }
            )
          )
        )
      )
    )
  )
}

#' Create the meta-regression server.
#'
#' @param id ID of the module
#' @param all_data Study data including covariate columns, in wide or long format
#' @param outcome_measure Reactive containing the outcome measure of the analysis.
meta_regression_tab_server <- function(id, all_data, outcome_measure) {
  shiny::moduleServer(id, function(input, output, session) {
    
    basline_risk_outcomes <- c("MD", "OR")
    # Baseline risk analysis
    informed_conditional_panel_server(
      id = "baseline_risk_outcome_dependent",
      inner_server_expression = {
        baseline_risk_analysis_panel_server(
          id = "baseline_risk_analysis"
        )
      },
      condition = reactive({
        outcome_measure() %in% basline_risk_outcomes
      }),
      error_message_text_expression = {
        paste0(
          "Outcome measure is not supported for this analysis. Supported types are: ",
          paste0(basline_risk_outcomes, collapse = ", ")
        )
      }
    )
    
    covariate_outcomes <- c("MD", "OR", "RR")
    # Covariate analysis
    informed_conditional_panel_server(
      id = "covariate_presence_dependent",
      inner_server_expression = {
        informed_conditional_panel_server(
          id = "covariate_outcome_dependent",
          inner_server_expression = {
            covariate_analysis_panel_server(
              id = "covariate_analysis",
              all_data = all_data
            )
          },
          condition = reactive({
            outcome_measure() %in% covariate_outcomes
          }),
          error_message_text_expression = {
            paste0(
              "Outcome measure is not supported for this analysis. Supported types are: ",
              paste0(covariate_outcomes, collapse = ", ")
            )
          }
        )
      },
      condition = reactive({
        length(FindCovariateNames(all_data())) > 0
      }),
      error_message_ui_expression = {
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
      }
    )
  })
}
