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
      bayes_forest_submodule_ui(ns("sub"), "With selected studies excluded")
    )
  )
}

bayes_forest_submodule_server <- function(id, common, model, run, title){
  moduleServer(id, function(input, output, session) {

    # for some reason width doesn't adjust to screen size if this is a bindEvent
    output$plot <- renderPlot({
      run()
      req(common[[model]], run())
      bayes_forest(common[[model]])
      title(main = title)
    })

    n_trt <- reactive({
      if (model == "bayes_all"){
        return(nrow(common$treatment_df))
      } else {
        return(nrow(common$subsetted_treatment_df))
      }
    }) %>% bindEvent(run())

    output$plot_wrap <- renderUI({
      req(n_trt())
      plot_height = forest_height_pixels(n_trt(), title = TRUE)
      common$meta$bayes_forest[[paste0("plot_height_", id)]] <- plot_height / 72
      tagList(
        plotOutput(session$ns("plot"), height = plot_height),
        # stop it appearing without running
        p("Model fit:")
      )
    })

    output$table <- renderTable({
      common[[model]]$dic
    }, digits = 3, rownames = TRUE, colnames = FALSE, label = "Test") %>% bindEvent(run())

    output$text <- renderText({
      req(common[[model]])
      CreateTauSentence(common[[model]], common$outcome_measure, common$model_type)
    }) %>% bindEvent(run())

    output$download <- downloadHandler(
      filename = function() {
        glue::glue("MetaInsight_bayesian_forest_plot{id}.{common$download_format}")
      },
      content = function(file) {

        plot_func <- function(){
          bayes_forest(common[[model]])
          title(main = title)
        }

        write_plot(file,
                   common$download_format,
                   plot_func,
                   width = 9,
                   height = as.integer(forest_height_pixels(n_trt(), title = TRUE) / 72) + 0.5)

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
        common$logger %>% writeLog(type = "error", "Please fit the Bayesian models first")
        return()
      } else {
        trigger("bayes_forest")
      }
    })

    # listen for the _sub model being refitted and trigger again, but only if the module has already been used
    on("bayes_model_sub", {
      if (watch("bayes_forest") > 0){
        shinyjs::runjs("Shiny.setInputValue('bayes_forest-rerun', new Date().getTime());")
      }
    })

    # trigger for the main analysis - when run is clicked, but only if there is a valid model
    all_trigger <- reactive({
      if (watch("bayes_forest") > 0){
        common$meta$bayes_forest$used <- TRUE
        return(watch("bayes_forest"))
        }
    })

    # trigger for the sub analysis - when run is clicked or the model reruns, but only if there is a valid model
    sub_trigger <- reactive({
      if (watch("bayes_forest") > 0){
        return(list(watch("bayes_forest"), input$rerun))
      }
    })

    bayes_forest_submodule_server("all", common, "bayes_all", all_trigger, "All studies:")
    bayes_forest_submodule_server("sub", common, "bayes_sub", sub_trigger, "With selected studies excluded:")

  })
}


bayes_forest_submodule_result <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("plot_wrap")),
    tableOutput(ns("table")),
    textOutput(ns("text"))
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
  list(bayes_forest_knit = !is.null(common$meta$bayes_forest$used),
       bayes_forest_plot_height_all = common$meta$bayes_forest$plot_height_all,
       bayes_forest_plot_height_sub = common$meta$bayes_forest$plot_height_sub
      )
}







