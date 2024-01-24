
#' Module UI for the deviance report page
#' 
#' @param id ID of the module
#' @return Div for the page
deviance_report_page_ui <- function(id) {
  ns <- NS(id)
  
  # Gather deviance report divs to be inserted into the main UI
  all_divs <- deviance_report_panel_ui(id = ns("all"), item_name = "all studies")
  sub_divs <- deviance_report_panel_ui(id = ns("sub"), item_name = "the sensitivity analysis")
  
  # Main UI
  div(
    helpText(
      "Please note: if you change the selections on the sidebar,
      you will need to re-run the primary and/or sensitivity analysis from the 'Forest Plot' page."
    ),
    p(tags$strong("Deviance report for all studies and the sensitivity analysis")),
    fluidRow(
      column(
        width = 6,
        all_divs$residual
      ),
      column(
        width = 6,
        sub_divs$residual
      )
    ),
    p(
      "This plot represents each data points' contribution to the residual deviance for the
      NMA with consistency (horizontal axis) and the unrelated mean effect (ume) inconsistency models
      (vertical axis) along with the line of equality. The points on the equality line means there is no
      improvement in model fit when using the inconsistency model, suggesting that there is no evidence of inconsistency.
      Points above the equality line means they have a smaller residual deviance for the consistency model indicating a
      better fit in the NMA consistency model and points below the equality line
      means they have a better fit in the ume inconsistency model. Please note that the unrelated mean effects model
      may not handle multi-arm trials correctly. (Further reading: Dias S, Ades AE, Welton NJ, Jansen JP, Sutton AJ. Network meta-anlaysis for
      decision-making. Chapter 3 Model fit, model comparison and outlier detection. @2018 John Wiley & Sons Ltd.)"
    ),
    br(),
    br(),
    br(),
    fluidRow(
      column(
        width = 6,
        all_divs$per_arm
      ),
      column(
        width = 6,
        sub_divs$per_arm
      ),
      br(),
      p(
        "This stem plot represents the posterior residual deviance per study arm. The total number of stems equals
        the total number of data points in the network meta analysis. Going from left to right, the alternating symbols
        on the stems indicate the different studies. Each stem corresponds to the residual deviance ($dev.ab) associated with each
        arm in each study. The smaller residual deviance (the shorter stem), dev.ab, the better model fit for each
        data point. You can identify which stem corresponds to which study arm by hovering on the stem symbols.
        (Further reading: Dias S, Ades AE, Welton NJ, Jansen JP, Sutton AJ. Network meta-anlaysis for
        decision-making. Chapter 3 Model fit, model comparison and outlier detection. @2018 John Wiley & Sons Ltd.)"
      ),
      br(),
      br(),
      br(),
      column(
        width = 6,
        all_divs$leverage
      ),
      column(
        width = 6,
        sub_divs$leverage
      )
    ),
    br(),
    p(
      "This leverage plot shows the average leverage across the arms for each study ({sum($lev.ab)}/{number of arms}
      for each study) versus the square root of the average residual deviance across the arms for each study
      (sqrt({sum($dev.ab)}/{number of arms}) for each study).
      The leverage for each data point, is calculated as the posterior mean of the residual
      deviance, minus the deviance at the posterior mean of the fitted values. The leverage plot may be used to
      identify influential and/or poorly fitting studies and can be used to check how each study is affecting
      the overall model fit and DIC. Curves of the form x2 + y = c, c = 1, 2, 3, ., where x represents square root
      of residual deviance, and y represents the leverage, are marked on the plot. Points lying on such parabolas
      each contribute an amount c to the DIC (Spiegelhalter et al., 2002). Points that lie outside the line with
      c = 3 can generally be identified as contributing to the model's poor fit. Points with a high leverage are
      influential, which means that they have a strong influence on the model parameters that generate their fitted
      values. (Further reading: Dias S, Ades AE, Welton NJ, Jansen JP, Sutton AJ. Network meta-anlaysis for
      decision-making. Chapter 3 Model fit, model comparison and outlier detection. @2018 John Wiley & Sons Ltd.
      Spiegelhalter et al. (2002) Bayesian measures of model complexity and fit. J. R. Statist. Soc.B 64, Part4,
      pp.583-639)"
    ),
    br(),
    br()
  )
}


#' Module server for the deviance report page.
#' 
#' @param id ID of the module
#' @param model Reactive containing bayesian meta-analysis for all studies
#' @param model_sub Reactive containing meta-analysis with studies excluded
deviance_report_page_server <- function(id, model, model_sub) {
  moduleServer(id, function(input, output, session) {
    deviance_report_panel_server(id = "all", model = model)
    deviance_report_panel_server(id = "sub", model = model_sub)
  })
}