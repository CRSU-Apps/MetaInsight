bayes_compare_submodule_ui <- function(id, download_label) {
  ns <- NS(id)
  downloadButton(ns("download"), download_label)
}

bayes_compare_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    actionButton(ns("run"), "Generate tables", icon = icon("arrow-turn-down")),
    layout_columns(
      bayes_compare_submodule_ui(ns("all"), "All studies"),
      bayes_compare_submodule_ui(ns("sub"), "With selected studies excluded")
    )
  )
}

bayes_compare_submodule_server <- function(id, common, model, run, text){
  moduleServer(id, function(input, output, session) {

    output$table <- renderTable({
      bayes_compare(common[[model]], common$outcome_measure)
    }) %>% bindEvent(run())

    output$text <- renderUI({
      req(common[[model]])
      p(tags$strong(text))
    }) %>% bindEvent(run())

    output$download <- downloadHandler(
      filename = function(){
         glue::glue("MetaInsight_bayesian_comparison_{id}.csv")
      },
      content = function(file) {
        write.csv(bayes_compare(common[[model]], common$outcome_measure), file)
      }
    )

  })
}

bayes_compare_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    # check that a fitted model exists and error if not
    observeEvent(input$run, {
      if (is.null(common$bayes_all)){
        common$logger %>% writeLog(type = "error", "Please fit the Bayesian models first")
        return()
      } else {
        trigger("bayes_compare")
      }
    })

    # listen for the _sub model being refitted and trigger again, but only if the module has already been used
    on("bayes_model_sub", {
      if (watch("bayes_compare") > 0){
        shinyjs::runjs("Shiny.setInputValue('bayes_compare-rerun', new Date().getTime());")
      }
    })

    # trigger for the main analysis - when run is clicked, but only if there is a valid model
    all_trigger <- reactive({
      if (watch("bayes_compare") > 0){
        common$meta$bayes_compare$used <- TRUE
        return(input$run)
      }
    })

    # trigger for the sub analysis - when run is clicked or the model reruns, but only if there is a valid model
    sub_trigger <- reactive({
      if (watch("bayes_compare") > 0){
        return(list(input$run, input$rerun))
      }
    })

    bayes_compare_submodule_server("all", common, "bayes_all", all_trigger, "Treatment effects for all studies: comparison of all treatment pairs.")
    bayes_compare_submodule_server("sub", common, "bayes_sub", sub_trigger, "Treatment effects with selected studies excluded: comparison of all treatment pairs.")

  })
}


bayes_compare_submodule_result <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("text")),
    tableOutput(ns("table"))
  )
}

bayes_compare_module_result <- function(id) {
  ns <- NS(id)
  tagList(
    p(
      tags$strong(
        "In contrast to the 'comparison of all treatment pairs' tab in the frequentist NMA results,
        this table only contains the estimates from the network meta analysis,
        i.e. does not contain estimates from pairwise meta-analysis which only contains direct evidence.
        If you would like to obtain the pairwise meta-analysis results, please use the Nodesplit model module"
      )
    ),
    layout_columns(
      bayes_compare_submodule_result(ns("all")),
      bayes_compare_submodule_result(ns("sub"))
    )
  )
}

bayes_compare_module_rmd <- function(common) {
  list(bayes_compare_knit = !is.null(common$meta$bayes_compare$used))
}
