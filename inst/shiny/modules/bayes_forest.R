bayes_forest_submodule_ui <- function(id, run_label, download_label) {
  ns <- NS(id)
  fixedRow(
    actionButton(ns("run"), run_label),
    downloadButton(ns("download"), download_label)
  )
}

bayes_forest_module_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    fixedRow(
      column(
        width = 6,
        bayes_forest_submodule_ui(ns("all"), "Run for all studies", "All studies")
      ),
      column(
        width = 6,
        bayes_forest_submodule_ui(ns("sub"), "Run with selected studies excluded", "With selected studies excluded")
      )
    )
  )

}


bayes_forest_submodule_server <- function(id, common, model, trigger){
  moduleServer(id, function(input, output, session) {

    init(trigger)

    observeEvent(input$run, {
      trigger(trigger)
    })

    output$plot <- renderPlot({
      watch(trigger)
      req(common[[model]])
      if (model == "bayes_all"){
        title <- "All studies:"
      } else if (model == "bayes_sub"){
        title <- "With selected studies excluded:"
      }

      bayes_forest(common[[model]])
      title(main = title)

    })

    output$plot_wrap <- renderUI({
      if (model == "bayes_all"){
        n_trt <- nrow(common$treatment_df)
      } else{
        n_trt <- nrow(common$subsetted_treatment_df)
      }
      plotOutput(session$ns("plot"), height = forest_height_pixels(n_trt, title = TRUE))
    })

    output$table <- renderTable ({
      watch(trigger)
      req(common[[model]])
      common[[model]]$dic
    }, digits = 3, rownames = TRUE, colnames = FALSE)

    output$text <-renderText({
      watch(trigger)
      req(common[[model]])
      CreateTauSentence(common[[model]], common$outcome_measure, common$model_type)
    })

    output$download <- downloadHandler(
      filename = function() {
        if (model == "bayes_all"){
          name <- "MetaInsight_bayesian_forest_plot_all."
        } else {
          name <- "MetaInsight_bayesian_forest_plot_sub."
        }
        paste0(name, common$download_format)
      },
      content = function(file) {

        if (model == "bayes_all"){
          title <- "All studies:"
          n_trt <- nrow(common$treatment_df)
        } else if (model == "bayes_sub"){
          title <- "With selected studies excluded:"
          n_trt <- nrow(common$subsetted_treatment_df)
        }

        plot_func <- function(){
          bayes_forest(common[[model]])
          title(main = title)
        }

        write_plot(file, common$download_format, plot_func, width = 9, height = as.integer(forest_height_pixels(n_trt, title = TRUE) / 72) + 0.5)

      }
    )


  })
}



bayes_forest_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {


    bayes_forest_submodule_server("all", common, "bayes_all", "bayes_forest_all")
    bayes_forest_submodule_server("sub", common, "bayes_sub", "bayes_forest_sub")

  return(list(
    save = function() {
      # Save any values that should be saved when the current session is saved
      # Populate using save_and_load()
    },
    load = function(state) {
      # Load
      # Populate using save_and_load()
    }
  ))

})
}


bayes_forest_submodule_result <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("plot_wrap")),
    p("Model fit:"),
    tableOutput(ns("table")),
    textOutput(ns("text"))
  )
}

bayes_forest_module_result <- function(id) {
  ns <- NS(id)

  tagList(
    fixedRow(
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
  # Variables used in the module's Rmd code
  # Populate using metadata()
}







