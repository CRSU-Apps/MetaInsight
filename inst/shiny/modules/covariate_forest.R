metaregression_forest_module_ui <- function(id, parent_id) {
  ns <- NS(id)
  div(class = glue("{parent_id}_div download_buttons"),
      downloadButton(ns("download"), "Download table")
  )
}

covariate_forest_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("run"), "Generate plot", icon = icon("arrow-turn-down")),
    metaregression_forest_module_ui(ns("covariate"), id)
  )
}

metaregression_forest_module_server <- function(id, common, run) {
  moduleServer(id, function(input, output, session) {

    module_id <- glue("{id}_forest")
    model <- glue("{id}_model")

    hide_and_show(module_id)

    observeEvent(run(), {
      if (is.null(common[[model]])){
        common$logger |> writeLog(type = "error", go_to = model, glue("Please fit the {id} model first"))
        return()
      } else {
        common$meta[[module_id]]$used <- TRUE
        trigger(module_id)
      }
    })

    svg <- reactive({
      watch(glue("{model}_fit"))
      req(watch(module_id) > 0)

      if (id == "covariate"){
        plot_title  <- "Covariate regression analysis"
      } else {
        plot_title  <- "Baseline risk regression analysis"
      }

      do.call(module_id, list(common[[model]],
                           common$treatment_df,
                           common$reference_treatment_all,
                           plot_title
                           ))
    })

    output$plot <- renderUI({
      req(svg())
      div(class = "svg_container", style = "max-width: 800px;",
          svg()
      )
    })

    output$download <- downloadHandler(
      filename = function() {
        glue("MetaInsight_{id}_forest_plot.{common$download_format}")
      },
      content = function(file) {
        write_plot(svg(), file, common$download_format)
      }
    )

  })
}

covariate_forest_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {
    metaregression_forest_module_server("covariate", common, reactive(input$run))
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

