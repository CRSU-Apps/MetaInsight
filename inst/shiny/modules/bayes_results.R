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

      title <- glue::glue("Results on the {common[[model]]$sumresults$measure} scale")
      iterations <- glue::glue("Iterations = {common[[model]]$sumresults$summaries$start}:{common[[model]]$sumresults$summaries$end}")
      thinning <- glue::glue("Thinning interval = {common[[model]]$sumresults$summaries$thin}")
      chains <- glue::glue("Number of chains = {common[[model]]$sumresults$summaries$nchain}")
      sample <- glue::glue("Sample size per chain = {(common[[model]]$sumresults$summaries$end + 1) - common[[model]]$sumresults$summaries$start}")

      shinyjs::show(selector = ".bayes_results_div")
      HTML(paste(title, "", iterations, thinning, chains, sample, sep = "<br/>"))

    }) %>% bindEvent(run())

    output$statistics <- renderTable({
      req(common[[model]])
      common[[model]]$sumresults$summaries$statistics
    }, rownames = TRUE) %>% bindEvent(run())

    output$quantiles <- renderTable({
      req(common[[model]])
      common[[model]]$sumresults$summaries$quantiles
    }, rownames = TRUE) %>% bindEvent(run())
  })
}

bayes_results_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    shinyjs::hide(selector = ".bayes_results_div")

    # check that a fitted model exists and error if not
    observeEvent(input$run, {

      # add check for a running model

      if (is.null(common$bayes_all)){
        common$logger %>% writeLog(type = "error", "Please fit the Bayesian models first")
        return()
      } else {
        trigger("bayes_results")
      }
    })

    # listen for the _sub model being refitted and trigger again, but only if the module has already been used
    on("bayes_model_sub", {
      if (watch("bayes_results") > 0){
        shinyjs::runjs("Shiny.setInputValue('bayes_results-rerun', new Date().getTime());")
      }
    })

    # trigger for the main analysis - when run is clicked, but only if there is a valid model
    all_trigger <- reactive({
      if (watch("bayes_results") > 0){
        common$meta$bayes_results$used <- TRUE
        return(watch("bayes_results"))
      }
    })

    # trigger for the sub analysis - when run is clicked or the model reruns, but only if there is a valid model
    sub_trigger <- reactive({
      if (watch("bayes_results") > 0){
        return(list(watch("bayes_results"), input$rerun))
      }
    })

    bayes_results_submodule_server("all", common, "bayes_all", all_trigger)
    bayes_results_submodule_server("sub", common, "bayes_sub", sub_trigger)

  })
}


bayes_results_submodule_result <- function(id, label) {
  ns <- NS(id)
  tagList(
    tagList(
      div(class = "bayes_results_div",
          br(),
          h5(glue::glue("Results details {label}"))
      ),
      uiOutput(ns("text")),
      div(class = "bayes_results_div",
        br(),
        h5("Empirical mean and standard deviation for each variable,
            plus standard error of the mean:")
        ),
      tableOutput(ns("statistics")),
      div(class = "bayes_results_div",
          h5("Quantiles for each variable:")
          ),
      tableOutput(ns("quantiles"))
    )
  )
}

bayes_results_module_result <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 6,
        bayes_results_submodule_result(ns("all"), "for all studies")
      ),
      column(
        width = 6,
        bayes_results_submodule_result(ns("sub"), "excluding selected studies")
      )
    )
  )
}


bayes_results_module_rmd <- function(common) {
  list(bayes_results_knit = !is.null(common$meta$bayes_results$used))
}







