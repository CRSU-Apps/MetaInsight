
#' Module UI for the MCMC panel
#' 
#' @param id ID of the module
#' @param item_name Name of this deviance report item.
#' @return Div for the panel
mcmc_panel_ui <- function(id, item_name) {
  ns <- NS(id)
  div(
    invalid_model_panel_ui(id = ns("model_invalid")),

    h4(tags$strong(glue::glue("Gelman convergence assessment plots for {item_name}"))),
    shinycssloaders::withSpinner(
      plotOutput(outputId = ns("gemtc_gelman"), width = "100%", height = "100%"),
      type = 6
    ),
    br(),
    h4(tags$strong(glue::glue("Trace plots for {item_name}"))),
    shinycssloaders::withSpinner(
      plotOutput(outputId = ns("trace_plots"), width = "100%", height = "100%"),
      type = 6
    ),
    br(),
    h4(tags$strong(glue::glue("Posterior density plots for {item_name}"))),
    shinycssloaders::withSpinner(
      plotOutput(outputId = ns("density_plots"), width = "100%", height = "100%"),
      type = 6
    )
  )
}



#' Module server for the MCMC panel.
#' 
#' @param id ID of the module.
#' @param model Reactive containing bayesian meta-analysis.
#' @param package "gemtc" (default) or "bnma".
#' @param model_valid Reactive containing whether the model is valid.
mcmc_panel_server <- function(id, model, model_valid, package = "gemtc") {
  moduleServer(id, function(input, output, session) {
    
    invalid_model_panel_server(id = "model_invalid", model_valid = model_valid)
    
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
        return(
          lapply(parameters(),
                 function(parameter) {
                   return(coda::gelman.plot(model()$mtcResults$samples[, parameter]))
                 }
          )
        )
      } else if (package == "bnma") {
        return(
          lapply(parameters(),
                 function(parameter) {
                   return(coda::gelman.plot(model()$samples[, parameter]))
                 }
          )
        )
      }
    })
    
    #TRUE if the model is a regression
    is_nmr <- reactive({
      return(package == "bnma" || model()$mtcResults$model$type == "regression")
    })
    
    #The number of columns in the grid
    n_cols <- reactive({
      return(2 + 2 * is_nmr())
    })
    
    #The number of rows in the grid
    n_rows <- reactive({
      return(ceiling(length(parameters()) / n_cols()))
    })
    
    # Gelman plots
    output$gemtc_gelman <- renderPlot({
        if (is.null(model_valid()) || !model_valid()) {
          return()
        }
        par(mfrow = c(n_rows(), n_cols()))
        return(GelmanPlots(gelman_plots = gelman_plots(), parameters = parameters()))
      },
      height = function() {
        n_rows() * 250
      }
    )
    
    #Trace plots
    output$trace_plots <- renderPlot({
        if (is.null(model_valid()) || !model_valid()) {
          return()
        }
        
        plotlist <- switch(
          package,
          "gemtc" = TracePlots(model = model()$mtcResults, parameters = parameters()),
          "bnma" = TracePlots(model = model(), parameters = parameters())
          )
        
        return(
          cowplot::plot_grid(
            plotlist = plotlist,
            ncol = n_cols()
          )
        )
      },
      height = function() {
        n_rows() * 200
      }
    )
    
    #Posterior density plots
    output$density_plots <- renderPlot({
        if (is.null(model_valid()) || !model_valid()) {
          return()
        }
        
        plotlist <- switch(
          package,
          "gemtc" = DensityPlots(model = model()$mtcResults, parameters = parameters()),
          "bnma" = DensityPlots(model = model(), parameters = parameters())
          )
      
        return(
          cowplot::plot_grid(
            plotlist = plotlist,
            ncol = n_cols()
          )
        )
      },
      height = function() {
        n_rows() * 200
      }
    )
  })
}