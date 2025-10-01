covariate_forest_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("run"), "Generate plot", icon = icon("arrow-turn-down")),
    downloadButton(ns("download"), "Download plot")
  )
}

covariate_forest_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    shinyjs::hide("download")

    observeEvent(input$run, {
      if (is.null(common$covariate_model)){
        common$logger |> writeLog(type = "error", "Please fit the covariate model first")
        return()
      } else {
        common$meta$covariate_forest$used <- TRUE
        shinyjs::show("download")
        trigger("covariate_forest")
      }
    })

    svg <- reactive({
      watch("covariate_model_fit")
      req(watch("covariate_forest") > 0)
      covariate_forest(common$covariate_model,
                       common$treatment_df,
                       common$reference_treatment_all,
                       "Covariate regression analysis")
    })

    output$plot <- renderUI({
      req(svg())
      div(class = "svg_container", style = "max-width: 800px;",
          HTML(svg()$svg)
      )
    })

    output$download <- downloadHandler(
      filename = function() {
        "MetaInsight_covariate_forest_plot.{common$download_format}"
      },
      content = function(file) {
        write_svg_plot(file, common$download_format, svg())
      }
    )

})
}


covariate_forest_module_result <- function(id) {
  ns <- NS(id)
  div(align = "center",
      uiOutput(ns("plot"))
  )
}


covariate_forest_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  # Populate using metadata()
}

