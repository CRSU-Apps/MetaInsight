metaregression_forest_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("run"), "Generate plot", icon = icon("arrow-turn-down")),
    downloadButton(ns("download"), "Download plot")
  )
}

covariate_forest_module_ui <- function(id) {
  ns <- NS(id)
  metaregression_forest_module_ui(ns("covariate"))
}


metaregression_forest_module_server <- function(id, common) {
  moduleServer(id, function(input, output, session) {

    shinyjs::hide("download")

    module <- glue::glue("{id}_forest")
    model <- glue::glue("{id}_model")

    observeEvent(input$run, {
      if (is.null(common[[model]])){
        common$logger |> writeLog(type = "error", glue::glue("Please fit the {id} model first"))
        return()
      } else {
        common$meta[[module]]$used <- TRUE
        shinyjs::show("download")
        trigger(module)
      }
    })

    svg <- reactive({
      watch(glue::glue("{model}_fit"))
      req(watch(module) > 0)
      do.call(module, list(common[[model]],
                           common$treatment_df,
                           common$reference_treatment_all,
                           glue::glue("{stringr::str_to_title(id)} regression analysis")
                           ))
    })

    output$plot <- renderUI({
      req(svg())
      div(class = "svg_container", style = "max-width: 800px;",
          HTML(svg()$svg)
      )
    })

    output$download <- downloadHandler(
      filename = function() {
        glue::glue("MetaInsight_{id}_forest_plot.{common$download_format}")
      },
      content = function(file) {
        write_svg_plot(file, common$download_format, svg())
      }
    )

  })
}

covariate_forest_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {
    metaregression_forest_module_server("covariate", common)
})
}


metaregression_forest_module_result <- function(id) {
  ns <- NS(id)
  div(align = "center",
      uiOutput(ns("plot"))
  )
}

covariate_forest_module_result <- function(id) {
  ns <- NS(id)
  metaregression_forest_module_result(ns("covariate"))
}


covariate_forest_module_rmd <- function(common) {
  list(covariate_forest_knit = !is.null(common$meta$covariate_forest$used))
}

