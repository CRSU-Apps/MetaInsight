bayes_results_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    actionButton(ns("run"), "Show results", icon = icon("arrow-turn-down")),
  )
}

bayes_results_submodule_server <- function(id, common, model, run){
  moduleServer(id, function(input, output, session) {

    output$text <- renderUI({
      req(common[[model]])
      bayes_results(common[[model]])
    }) |> bindEvent(run())

    outputOptions(output, "text", suspendWhenHidden = FALSE)

    output$statistics <- renderTable({
      req(common[[model]])
      common[[model]]$sumresults$summaries$statistics
    }, rownames = TRUE) |> bindEvent(run())

    output$quantiles <- renderTable({
      req(common[[model]])
      common[[model]]$sumresults$summaries$quantiles
    }, rownames = TRUE) |> bindEvent(run())
  })
}

bayes_results_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    hide_and_show(id)

    # check that a fitted model exists and error if not
    observeEvent(input$run, {

      if (is.null(common$bayes_all)){
        common$logger |> writeLog(type = "error", go_to = "bayes_model", "Please fit the Bayesian models first")
        return()
      } else {
        trigger("bayes_results")
        common$meta$bayes_results$used <- TRUE
      }
    })

    # trigger for the main analysis - when run is clicked, but only if there is a valid model
    all_trigger <- reactive({
      if (watch("bayes_results") > 0){
        return(list(watch("bayes_results"), watch("bayes_model_all")))
      }
    })

    # trigger for the sub analysis - when run is clicked or the model reruns, but only if there is a valid model
    sub_trigger <- reactive({
      if (watch("bayes_results") > 0){
        return(list(watch("bayes_results"), watch("bayes_model_sub")))
      }
    })

    bayes_results_submodule_server("all", common, "bayes_all", all_trigger)
    bayes_results_submodule_server("sub", common, "bayes_sub", sub_trigger)

  })
}


bayes_results_submodule_result <- function(id, label, class) {
  ns <- NS(id)
  tagList(
    div(class = class,
      tagList(
        br(),
        h5(glue::glue("Results details {label}")),
        uiOutput(ns("text")),
        br(),
        h5("Empirical mean and standard deviation for each variable,
            plus standard error of the mean:"),
        tableOutput(ns("statistics")),
        h5("Quantiles for each variable:"),
        tableOutput(ns("quantiles"))
      )
    )
  )
}

bayes_results_module_result <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 6,
        bayes_results_submodule_result(ns("all"), "for all studies", "bayes_results_div")
      ),
      column(
        width = 6,
        bayes_results_submodule_result(ns("sub"), "excluding selected studies", "bayes_results_div")
      )
    )
  )
}

bayes_results_module_rmd <- function(common) {
  list(bayes_results_knit = !is.null(common$meta$bayes_results$used))
}

