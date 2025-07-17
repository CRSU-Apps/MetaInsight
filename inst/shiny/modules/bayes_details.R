bayes_details_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    actionButton(ns("run"), "Generate details", icon = icon("arrow-turn-down"))
  )
}

bayes_details_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    shinyjs::hide(selector = ".bayes_details_div")

    observeEvent(input$run, {
      if (is.null(common$bayes_all)){
        common$logger |> writeLog(type = "error", "Please fit the Bayesian models first")
        return()
      } else {
      shinyjs::show(selector = ".bayes_details_div")
      trigger("bayes_details")
      }
    })

    output$code <- renderPrint({
      watch("bayes_model_all")
      req(watch("bayes_details") > 0)
      req(common$bayes_all)
      cat(common$bayes_all$mtcResults$model$code, fill = FALSE, labels = NULL, append = FALSE)
    })

    output$download_code <- downloadHandler(
      filename = "MetaInsight_bayesian_model_code.txt",
      content = function(file) {
        cat(
          common$bayes_all$mtcResults$model$code,
          file = file,
          fill = FALSE,
          labels = NULL,
          append = FALSE
        )
      }
    )

    output$inits <- renderPrint({
      watch("bayes_model_all")
      req(watch("bayes_details") > 0)
      req(common$bayes_all)
      common$bayes_all$mtcResults$model$inits
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
      watch("bayes_deviance")
      watch("bayes_model_all")
      req(watch("bayes_details") > 0)
      validate(need(common$bayes_deviance_all, "Please run the Deviance report module first"))
      common$bayes_deviance_all$deviance_mtc
    })

    output$dev_ume <- renderPrint({
      watch("bayes_deviance")
      watch("bayes_model_all")
      req(watch("bayes_details") > 0)
      validate(need(common$bayes_deviance_all, "Please run the Deviance report module first"))
      common$bayes_deviance_all$deviance_ume
    })

})
}


bayes_details_module_result <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "bayes_details_div",
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


bayes_details_module_rmd <- function(common) {
  list(bayes_details_knit = FALSE)
}

