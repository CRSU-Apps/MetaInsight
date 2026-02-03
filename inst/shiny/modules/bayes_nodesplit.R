bayes_nodesplit_submodule_ui <- function(id, download_label) {
  ns <- NS(id)
  div(class = "bayes_nodesplit_div",
      downloadButton(ns("download"), download_label)
  )
}

bayes_nodesplit_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    input_task_button(ns("run"), "Run nodesplitting models", icon = icon("arrow-turn-down"), type = "default"),
    layout_columns(
      bayes_nodesplit_submodule_ui(ns("all"), "All studies"),
      bayes_nodesplit_submodule_ui(ns("sub"), "With selected studies excluded")
    )
  )
}

bayes_nodesplit_submodule_server <- function(id, common, nodesplit, run){
  moduleServer(id, function(input, output, session) {

    svg <- reactive({
      watch(run)
      req(common[[nodesplit]])
      shinyjs::show(selector = ".bayes_nodesplit_div")
      on.exit(shinyjs::runjs(paste0("Shiny.setInputValue('bayes_nodesplit-",id ,"-complete', 'complete');")))
      bayes_nodesplit_plot(common[[nodesplit]], id == "all")
    })

    output$plot <- renderUI({
      req(svg())
      svg_container(
        svg()
      )
    })

    output$download <- downloadHandler(
      filename = function(){
          glue("MetaInsight_nodesplit_{id}.{common$download_format}")
      },
      content = function(file) {
        write_plot(svg(),
                   file,
                   common$download_format)
      }
    )

  })
}

bayes_nodesplit_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    init("bayes_nodesplit_all")
    init("bayes_nodesplit_sub")

    hide_and_show(id, show = FALSE)

    observeEvent(input$run, {
      if (is.null(common$main_connected_data)){
        common$logger |> writeLog(type = "error", go_to = "setup_configure",
                                  "Please configure the analysis in the Setup component first.")
      }
      trigger("bayes_nodesplit")
    })

    common$tasks$bayes_nodesplit_all <- ExtendedTask$new(
      function(...) mirai::mirai(run(...), run = bayes_nodesplit, .args = environment())
    ) |> bind_task_button("run")

    # needed to cancel in progress
    sub_nodesplit <- NULL
    common$tasks$bayes_nodesplit_sub <- ExtendedTask$new(
      function(...) sub_nodesplit <<- mirai::mirai(run(...), run = bayes_nodesplit, .args = environment())
    ) |> bind_task_button("run")

    observeEvent(watch("bayes_nodesplit"), {
      req(watch("bayes_nodesplit") > 0)
      common$tasks$bayes_nodesplit_all$invoke(common$main_connected_data,
                                              common$treatment_df,
                                              common$outcome,
                                              common$outcome_measure,
                                              common$model_type,
                                              async = TRUE)

      result_all$resume()
    })

    observeEvent(list(watch("bayes_nodesplit"), watch("setup_exclude")), {
      req(watch("bayes_nodesplit") > 0)

      # cancel if the model is already updating
      if (common$tasks$bayes_nodesplit_sub$status() == "running"){
        mirai::stop_mirai(sub_nodesplit)
      }

      common$tasks$bayes_nodesplit_sub$invoke(common$subsetted_data,
                                              common$subsetted_treatment_df,
                                              common$outcome,
                                              common$outcome_measure,
                                              common$model_type,
                                              async = TRUE)
      result_sub$resume()
    })

    result_all <- observe({
      result <- common$tasks$bayes_nodesplit_all$result()
      result_all$suspend()
      if (inherits(result, "mtc.nodesplit")){
        common$nodesplit_all <- result
        # METADATA ####
        common$meta$bayes_nodesplit$used <- TRUE
        trigger("bayes_nodesplit_all")
      } else {
        common$logger |> writeLog(type = "error", result)
        shinyjs::runjs("Shiny.setInputValue('bayes_nodesplit-all-complete', 'complete');")
      }

    })

    result_sub <- observe({
      # prevent loading when the task is cancelled
      if (common$tasks$bayes_nodesplit_sub$status() == "success"){
        result <- common$tasks$bayes_nodesplit_sub$result()
        result_sub$suspend()
        if (inherits(result, "mtc.nodesplit")){
          common$nodesplit_sub <- result
          trigger("bayes_nodesplit_sub")
        } else {
          common$logger |> writeLog(type = "error", result)
          shinyjs::runjs("Shiny.setInputValue('bayes_nodesplit-sub-complete', 'complete');")
        }
      }
    })

    bayes_nodesplit_submodule_server("all", common, "nodesplit_all", "bayes_nodesplit_all")
    bayes_nodesplit_submodule_server("sub", common, "nodesplit_sub", "bayes_nodesplit_sub")

  })
}


bayes_nodesplit_submodule_result <- function(id) {
  ns <- NS(id)
  uiOutput(ns("plot"))
}

bayes_nodesplit_module_result <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      bayes_nodesplit_submodule_result(ns("all")),
      bayes_nodesplit_submodule_result(ns("sub"))
    )
  )
}


bayes_nodesplit_module_rmd <- function(common) {
  list(bayes_nodesplit_knit = !is.null(common$nodesplit_all),
       bayes_nodesplit_plot_height_all = common$meta$bayes_nodesplit$plot_height_all,
       bayes_nodesplit_plot_height_sub = common$meta$bayes_nodesplit$plot_height_sub
       )
}

