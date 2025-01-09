
#' Module UI for the result details panel
#' 
#' @param id ID of the module
#' @param item_name Name of this deviance report item.
#' @return Div for the panel
result_details_panel_ui <- function(id, item_name) {
  ns <- NS(id)
  div(
    p(tags$strong(glue::glue("Results details for {item_name}"))),
    shinycssloaders::withSpinner(
      verbatimTextOutput(outputId = ns("gemtc_results")),
      type = 6
    ),
    p(tags$strong(glue::glue("Gelman convergence assessment plot for {item_name}"))),
    shinycssloaders::withSpinner(
      plotOutput(outputId = ns("gemtc_gelman"), inline = TRUE),
      type = 6
    )
  )
}



#' Module server for the result details panel.
#' 
#' @param id ID of the module.
#' @param model Reactive containing bayesian meta-analysis.
#' @param package "gemtc" (default) or "bnma".
#' @param model_valid Reactive containing whether the model is valid.
result_details_panel_server <- function(id, model, model_valid, package = "gemtc") {
  moduleServer(id, function(input, output, session) {
    
    # Results details
    output$gemtc_results <- renderPrint({
      if (!model_valid()) {
        return()
      }
      if (package == "gemtc") {
        return(model()$sumresults)
      } else if (package == "bnma") {
        return(summary(model()))
      }
    })
    
    #Need to use bnma terminology for covariate parameters below
    cov_parameters <- reactive({
      if (model()$network$baseline == "common"){
        return("shared")
      } else if (model()$network$baseline == "independent"){
        return("unrelated")
      } else {
        return(model()$network$baseline)
      }
    })
    
    #The parameters to display in Gelman plots
    parameters <- reactive({
      if (package == "gemtc") {
        return(model()$mtcResults$model$monitors$enabled)
      } else if (package == "bnma") {
        return(GetBnmaParameters(all_parameters = attr(model()$samples[[1]], "dimnames")[[2]],
                                 effects_type = model()$network$type,
                                 cov_parameters = cov_parameters()))
      }
    })
    
    #For baseline risk, create a Gelman plot for each parameter
    gelman_plots <- reactive({
      if (package == "gemtc") {
        return(NULL)
      } else if (package == "bnma") {
        return(
          lapply(parameters(),
                 function(parameter){
                   return(coda::gelman.plot(model()$samples[, parameter]))
                 }
          )
        )
      }
    })
    
    #The number of rows, to determine the dimensions of the grid in bnma, and the height of the plot in bnma and gemtc
    n_rows <- reactive({
      ceiling(length(parameters()) / 2)
    })
    
    # Gelman plots
    output$gemtc_gelman <- renderPlot(
      {
        if (!model_valid()) {
          return()
        }
        if (package == "gemtc") {
          return(gelman.plot(model()$mtcResults))
        } else if (package == "bnma") {
          par(mfrow = c(n_rows(), 2))
          return(BnmaGelmanPlots(gelman_plots = gelman_plots(), parameters = parameters()))
        }
      },
      height = function() {
        n_rows() * 300
      }
    )
  })
}