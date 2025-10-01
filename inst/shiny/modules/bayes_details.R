bayes_details_submodule_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("run"), "Generate details", icon = icon("arrow-turn-down"))
  )
}

bayes_details_module_ui <- function(id) {
  ns <- NS(id)
  bayes_details_submodule_ui(ns("bayes"))
}


bayes_details_submodule_server <- function(id, common, model, model_trigger, module_id, deviance, deviance_trigger, error_message){
  moduleServer(id, function(input, output, session) {

  shinyjs::hide(selector = glue::glue(".{module_id}_div"))

  observeEvent(input$run, {
    if (is.null(common[[model]])){
      common$logger |> writeLog(type = "error", error_message)
      return()
    } else {
      shinyjs::show(selector = glue::glue(".{module_id}_div"))
      common$meta[[module_id]]$used <- TRUE
      trigger(module_id)
    }
  })

  output$code <- renderPrint({
    watch(model_trigger)
    req(watch(module_id) > 0)
    req(common[[model]])
    cat(common[[model]]$mtcResults$model$code, fill = FALSE, labels = NULL, append = FALSE)
  })

  output$download_code <- downloadHandler(
    filename = "MetaInsight_bayesian_model_code.txt",
    content = function(file) {
      cat(
        common[[model]]$mtcResults$model$code,
        file = file,
        fill = FALSE,
        labels = NULL,
        append = FALSE
      )
    }
  )

  output$inits <- renderPrint({
    watch(model_trigger)
    req(watch(module_id) > 0)
    req(common[[model]])
    common[[model]]$mtcResults$model$inits
  })

  create_chain_initial_data_download_handler <- function(index) {
    filename <- paste0("MetaInsight_bayesian_initial_values_chain_", index, ".txt")

    downloadHandler(
      filename = filename,
      content = function(file) {
        lapply(
          common$bayes_all$mtcResults$model$inits,
          write,
          file,
          append = TRUE,
          ncolumns = 1000
        )
      }
    )
  }

  output$download_inits_1 <- create_chain_initial_data_download_handler(1)
  output$download_inits_2 <- create_chain_initial_data_download_handler(2)
  output$download_inits_3 <- create_chain_initial_data_download_handler(3)
  output$download_inits_4 <- create_chain_initial_data_download_handler(4)

  create_chain_data_download_handler <- function(index) {
    downloadHandler(
      filename = paste0("MetaInsight_bayesian_data_for_chain_", index, ".csv"),
      content = function(file) {
        data <- as.data.frame(samples()[[index]])
        write.csv(data, file)
      }
    )
  }

  output$download_data1 <- create_chain_data_download_handler(1)
  output$download_data2 <- create_chain_data_download_handler(2)
  output$download_data3 <- create_chain_data_download_handler(3)
  output$download_data4 <- create_chain_data_download_handler(4)

  output$dev_mtc <- renderPrint({
    watch(deviance_trigger)
    watch(model_trigger)
    req(watch(module_id) > 0)
    validate(need(common[[deviance]], "Please run the Deviance report module first"))
    common[[deviance]]$deviance_mtc
  })

  output$dev_ume <- renderPrint({
    watch(deviance_trigger)
    watch(model_trigger)
    req(watch(module_id) > 0)
    validate(need(common[[deviance]], "Please run the Deviance report module first"))
    common[[deviance]]$deviance_ume
  })

  })
}

bayes_details_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {
    bayes_details_submodule_server("bayes", common, "bayes_all", "bayes_model_all", "bayes_details", "bayes_deviance_all", "bayes_deviance",
                                   "Please fit the Bayesian models first")
})
}

bayes_details_submodule_result <- function(id, class) {
  ns <- NS(id)
  tagList(
    div(class = class,
        tabsetPanel(
          tabPanel(
            title = "Model codes",
            p(tags$strong("Model codes for analysis of all studies")),
            downloadButton(ns("download_code")),
            verbatimTextOutput(ns("code"))
          ),
          tabPanel(
            title = "Initial values",
            p(tags$strong("Initial values")),
            downloadButton(ns('download_inits_1'), "Download initial values for chain 1"),
            downloadButton(ns('download_inits_2'), "Download initial values for chain 2"),
            downloadButton(ns('download_inits_3'), "Download initial values for chain 3"),
            downloadButton(ns('download_inits_4'), "Download initial values for chain 4"),
            verbatimTextOutput(ns("inits"))
          ),
          tabPanel(
            title = "Download simulations",
            p(tags$strong("Download simulated data")),
            downloadButton(ns('download_data1'), "Download data from chain 1"),
            downloadButton(ns('download_data2'), "Download data from chain 2"),
            downloadButton(ns('download_data3'), "Download data from chain 3"),
            downloadButton(ns('download_data4'), "Download data from chain 4")
          ),
          tabPanel(
            title = "Deviance details",
            p("NMA (consistency) model"),
            verbatimTextOutput(ns("dev_mtc")),
            p("UME (inconsistency) model"),
            verbatimTextOutput(ns("dev_ume"))
          )
        )
    )
  )
}

bayes_details_module_result <- function(id) {
  ns <- NS(id)
  bayes_details_submodule_result(ns("bayes"), "bayes_details_div")
}


bayes_details_module_rmd <- function(common) {
  list(bayes_details_knit = !is.null(common$meta$bayes_details$used),
       bayes_deviance_knit = !is.null(common$meta$bayes_deviance$used))
}

