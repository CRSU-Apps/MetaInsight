
bayes_anohe_submodule_ui <- function(id, download_label) {
  ns <- NS(id)
  downloadButton(ns("download"), download_label)
}

bayes_anohe_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    actionButton(ns("run"), "Run models", icon = icon("arrow-turn-down")),
    div(
      class = "bayes_anohe download_buttons",
      layout_columns(
        bayes_anohe_submodule_ui(ns("all"), "All studies"),
        bayes_anohe_submodule_ui(ns("sub"), "With selected studies excluded")
      )
    )
  )
}

bayes_anohe_submodule_server <- function(id, common, module_id, model, anohe, trigger){
  moduleServer(id, function(input, output, session) {

    output$table <- renderTable({
      watch(trigger)
      req(common[[anohe]])
      shinyjs::show(selector = ".bayes_anohe")
      on.exit(shinyjs::runjs(glue("Shiny.setInputValue('{session$ns(\"complete\")}', 'complete');")))
      common[[paste0("bayes_anohe_", id)]]
    }, rownames = TRUE)

    outputOptions(output, "table", suspendWhenHidden = FALSE)
    
    output$download <- downloadHandler(
      filename = function(){
        glue("MetaInsight_bayesian_heterogeneity_{id}.csv")
      },
      content = function(file) {
        write.csv(common[[paste0("bayes_anohe_", id)]], file, row.names = FALSE)
      }
    )
  })
}


bayes_anohe_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {
    
    init("bayes_anohe_all")
    init("bayes_anohe_sub")
    hide_and_show(id, show = FALSE)
    
    observeEvent(input$run, {
      if (is.null(common$bayes_model_all)){
        common$logger |> writeLog(type = "error", go_to = "bayes_model", "Please fit the Bayesian models first")
        return()
      } else {
        common$meta$bayes_anohe$used <- TRUE
        trigger("bayes_anohe")
      }
    })
    
    common$tasks$bayes_anohe_all <- ExtendedTask$new(
      function(...) mirai::mirai(run(...), run = bayes_anohe, .args = environment())
    ) |> bind_task_button("run")
    
    common$tasks$bayes_anohe_sub <- ExtendedTask$new(
      function(...) mirai::mirai(run(...), run = bayes_anohe, .args = environment())
    ) |> bind_task_button("run")
    
    observeEvent(list(watch("bayes_anohe"), watch("bayes_model_all")), {
      req(watch("bayes_anohe") > 0)
      common$logger |> writeLog(type = "starting", "Running heterogeneity models")
      common$tasks$bayes_anohe_all$invoke(common$bayes_model_all)
      result_all$resume()
    })
    
    observeEvent(list(watch("bayes_anohe"), watch("bayes_model_sub")), {
      req(watch("bayes_anohe") > 0)
      
      # prevent showing on first run
      if (!is.null(common$bayes_anohe_sub)){
        common$logger |> writeLog(type = "complete", "Updating heterogeneity models")
      }
      
      common$tasks$bayes_anohe_sub$invoke(common$bayes_model_sub)
      result_sub$resume()
    })
    
    result_all <- observe({
      result <- common$tasks$bayes_anohe_all$result()
      result_all$suspend()
      common$bayes_anohe_all <- result
      common$logger |> writeLog(type = "complete", "Heterogeneity models have been run")
      trigger("bayes_anohe_all")
    })
    
    result_sub <- observe({
      result <- common$tasks$bayes_anohe_sub$result()
      result_sub$suspend()
      if (!is.null(common$bayes_anohe_sub)){
        common$logger |> writeLog(type = "complete", "Heterogeneity models have been run")
      }
      common$bayes_anohe_sub <- result
      trigger("bayes_anohe_sub")
    })

    bayes_anohe_submodule_server("all", common, "bayes_anohe", "bayes_model_all", "bayes_anohe_all", "bayes_anohe_all")
    bayes_anohe_submodule_server("sub", common, "bayes_anohe", "bayes_model_sub", "bayes_anohe_sub", "bayes_anohe_sub")
    
  })
}

bayes_anohe_submodule_result <- function(id, text) {
  ns <- NS(id)
  tagList(
    p(tags$strong(text)),
    tableOutput(ns("table"))
  )
}

bayes_anohe_module_result <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "bayes_anohe",
      layout_columns(
        bayes_anohe_submodule_result(ns("all"), "Heterogeneity statistics for all studies."),
        bayes_anohe_submodule_result(ns("sub"), "Heterogeneity statistics with selected studies excluded.")
      )
    )
  )
}

bayes_anohe_module_rmd <- function(common) {
  list(bayes_anohe_knit = !is.null(common$meta$bayes_anohe$used))
}
