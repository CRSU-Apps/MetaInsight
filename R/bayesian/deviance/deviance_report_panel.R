
#' Module UI for the deviance report panel
#' 
#' @param id ID of the module
#' @param item_name Name of this deviance report item.
#' @return List of divs containing the plots. Named "residual", "per_arm", and "leverage".
deviance_report_panel_ui <- function(id, item_name) {
  ns <- NS(id)
  return(
    list(
      residual = div(
        p(tags$strong(glue::glue("Residual deviance from NMA model and UME inconsistency model for {item_name}"))),
        plotlyOutput(outputId = ns("dev_scat"))
      ),
      per_arm = div(
        p(tags$strong(glue::glue("Per-arm residual deviance for {item_name}"))),
        plotlyOutput(outputId = ns("per_arm"))
      ),
      leverage = div(
        p(tags$strong(glue::glue("Leverage plot for {item_name}"))),
        plotlyOutput(outputId = ns("leverage"))
      )
    )
  )
}


#' Module server for the deviance report panel.
#' 
#' @param id ID of the module.
#' @param model Reactive containing bayesian meta-analysis.
#' @param package "gemtc" (default) or "bnma".
#' @param model_valid Reactive containing whether the model is valid.
deviance_report_panel_server <- function(id, model, model_valid, package = "gemtc") {
  moduleServer(id, function(input, output, session) {

    # Residual deviance from NMA model and UME inconsistency model
    output$dev_scat <- renderPlotly({
      if (!model_valid()) {
        return()
      }
      if (package == "gemtc") {
        return(scat_plot(model())$p)
      } else if (package == "bnma") {
        return(NULL)
      }
    })

    # Per-arm residual deviance
    output$per_arm <- renderPlotly({
      if (!model_valid()) {
        return()
      }
      stemplot(model(), package = package)
    })

    # Leverage plot
    output$leverage <- renderPlotly({
      if (!model_valid()) {
        return()
      }
      levplot(model(), package = package)
    })
  })
}

