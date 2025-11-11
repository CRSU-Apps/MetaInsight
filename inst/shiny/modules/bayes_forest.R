bayes_forest_submodule_ui <- function(id, download_label) {
  ns <- NS(id)
  downloadButton(ns("download"), download_label)
}

bayes_forest_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    actionButton(ns("run"), "Generate plots", icon = icon("arrow-turn-down")),
    layout_columns(
      bayes_forest_submodule_ui(ns("all"), "All studies"),
      bayes_forest_submodule_ui(ns("sub"), "Selected studies excluded")
    )
  )
}

bayes_forest_submodule_server <- function(id, common, model, run, title){
  moduleServer(id, function(input, output, session) {

    shinyjs::hide("download")

    svg <- reactive({
      tdf <- ifelse(id == "all", "treatment_df", "subsetted_treatment_df")

      bayes_forest(common[[paste0("bayes_", id)]],
                   common[[tdf]],
                   common[[paste0("reference_treatment_", id)]],
                   title)
    }) |> bindEvent(run())

    # this enables the plot to always fit in the column width
    output$plot <- renderUI({
      shinyjs::show("download")
      req(svg())

      div(class = "svg_container",
          HTML(svg()$svg)
      )
    })

    output$download <- downloadHandler(
      filename = function() {
        glue::glue("MetaInsight_bayesian_forest_plot_{id}.{common$download_format}")
      },
      content = function(file) {

        write_svg_plot(file,
                       common$download_format,
                       svg()
                       )

      }
    )

  })
}

bayes_forest_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    # check that a fitted model exists and error if not
    observeEvent(input$run, {

      # add check for a running model

      if (is.null(common$bayes_all)){
        common$logger |> writeLog(type = "error", go_to = "bayes_model", "Please fit the Bayesian models first")
        return()
      } else {
        common$meta$bayes_forest$used <- TRUE
        trigger("bayes_forest")
      }
    })

    # trigger for the main analysis - when run is clicked, but only if there is a valid model
    all_trigger <- reactive({
      if (watch("bayes_forest") > 0){
        return(list(watch("bayes_forest"), watch("bayes_model_all")))
      }
    })

    # trigger for the sub analysis - when run is clicked or the model reruns, but only if there is a valid model
    sub_trigger <- reactive({
      if (watch("bayes_forest") > 0){
        return(list(watch("bayes_forest"), watch("bayes_model_sub")))
      }
    })

    bayes_forest_submodule_server("all", common, "bayes_all", all_trigger, "Results for all studies")
    bayes_forest_submodule_server("sub", common, "bayes_sub", sub_trigger, "Results with selected studies excluded")

  })
}


bayes_forest_submodule_result <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("plot"))
  )
}

bayes_forest_module_result <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 6,
        align = "center",
        bayes_forest_submodule_result(ns("all"))
      ),
      column(
        width = 6,
        align = "center",
        bayes_forest_submodule_result(ns("sub"))
      )
    )
  )
}


bayes_forest_module_rmd <- function(common) {
  list(bayes_forest_knit = !is.null(common$meta$bayes_forest$used)
      )
}







