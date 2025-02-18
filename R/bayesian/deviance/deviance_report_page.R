
#' Module UI for the deviance report page.
#' 
#' @param id ID of the module.
#' @param item_names Vector of analysis titles to be shown side-by-side in the page.
#' @return Div for the page
deviance_report_page_ui <- function(id, item_names) {
  ns <- NS(id)
  
  # Matrix containing ui items:
  # - Deviance plots
  # - Invalid model notification
  divs <- sapply(
    1:length(item_names),
    function(index) {
      return(deviance_report_panel_ui(id = ns(glue::glue("deviance_{index}")), item_name = item_names[index]))
    }
  )
  
  # Main UI
  div(
    conditionalPanel(
      condition = "output.model_type != 'consistency'",
      ns = ns,
      h4(tags$strong("PLEASE NOTE: the", textOutput(ns("package"), inline = TRUE), "package does not currently include unrelated-mean-effects meta-regression models, therefore the"), tags$strong(tags$em(" consistency vs UME ")), tags$strong("graph that is displayed in the ", tags$code("deviance report"), " tab under ", tags$code("Bayesian network meta-analysis"), " is not available here."))
    ),
    
    conditionalPanel(
      condition = "output.model_type == 'consistency'",
      ns = ns,
      p(tags$strong("Deviance report for all studies and the sensitivity analysis")),
      
      # This is the way to get a dynamic number of columns rendered into the row
      do.call(
        fluidRow,
        lapply(
          1:length(item_names),
          function(index) {
            column(
              width = 12 / length(item_names),
              divs["model_invalid", index],
              divs["residual", index]
            )
          }
        )
      ),
      
      p(
        "This plot represents each data point's contribution to the residual deviance for the
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
      br()
    ),
    
    # This is the way to get a dynamic number of columns rendered into the row
    do.call(
      fluidRow,
      lapply(
        1:length(item_names),
        function(index) {
          column(
            width = 12 / length(item_names),
            divs["per_arm", index]
          )
        }
      )
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
    
    # This is the way to get a dynamic number of columns rendered into the row
    do.call(
      fluidRow,
      lapply(
        1:length(item_names),
        function(index) {
          column(
            width = 12 / length(item_names),
            divs["leverage", index]
          )
        }
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
#' @param id ID of the module.
#' @param models Vector of reactives containing bayesian meta-analyses.
#' @param models_valid Vector of reactives containing whether each model is valid.
#' @param package "gemtc" (default) or "bnma".
deviance_report_page_server <- function(id, models, models_valid, package = "gemtc") {
  moduleServer(id, function(input, output, session) {

    output$package <- reactive(paste0("{", package, "}"))
    outputOptions(x = output, name = "package", suspendWhenHidden = FALSE)
    
    output$model_type <- reactive({
      if (package == "gemtc") {
        return(models[[1]]()$mtcResults$model$type)
      } else if (package == "bnma") {
        return("baseline risk")
      }
    })
    outputOptions(x = output, name = "model_type", suspendWhenHidden = FALSE)
    
    # Create server for each model
    sapply(
      1:length(models),
      function(index) {
        deviance_report_panel_server(glue::glue("deviance_{index}"), model = models[[index]], model_valid = models_valid[[index]], package = package)
      }
    )
  })
}

