bayes_details_module_ui <- function(id) {
  ns <- NS(id)
  actionButton(ns("run"), "Generate details", icon = icon("arrow-turn-down"))
}

bayes_details_submodule_server <- function(id, common, model, model_trigger, module_id, deviance, deviance_trigger, error_message, run){
  moduleServer(id, function(input, output, session) {

  hide_and_show(module_id, show = FALSE)

  # for file names
  model_type <- switch(id,
                       "all" = "Bayesian",
                       "baseline" = "baseline_risk",
                       "covariate" = "covariate")

  observeEvent(run(), {
    if (is.null(common[[model]])){
      common$logger |> writeLog(type = "error",
                                go_to = glue::glue("{id}_model"),
                                error_message)
      return()
    } else {
      common$meta[[module_id]]$used <- TRUE
      trigger(module_id)
    }
  })

  details <- reactive({
    watch(model_trigger)
    req(watch(module_id) > 0)
    req(common[[model]])
    shinyjs::show(selector = glue::glue(".{module_id}_div"))
    bayes_details(common[[model]])
  })

  output$mcmc <- renderTable({
    details()$mcmc
  }, digits = 0, colnames = FALSE)

  outputOptions(output, "mcmc", suspendWhenHidden = FALSE)

  output$download_mcmc <- downloadHandler(
    filename = glue::glue("MetaInsight_{model_type}_mcmc_characteristics.csv"),
    content = function(file) {
      write.csv(details()$mcmc, file, row.names = FALSE, col.names = FALSE)
    }
  )

  output$priors <- renderTable({
    details()$priors
  }, colnames = FALSE)

  output$download_priors <- downloadHandler(
    filename = glue::glue("MetaInsight_{model_type}_prior_distributions.csv"),
    content = function(file) {
      write.csv(details()$priors, file, row.names = FALSE, col.names = FALSE)
    }
  )

  output$code <- renderPrint({
    watch(model_trigger)
    req(watch(module_id) > 0)
    req(common[[model]])
    location <- ifelse(model == "baseline_model", "network", "model")
    cat(common[[model]]$mtcResults[[location]]$code, fill = FALSE, labels = NULL, append = FALSE)
  })

  output$download_code <- downloadHandler(
    filename = glue::glue("MetaInsight_{model_type}_model_code.txt"),
    content = function(file) {
      location <- ifelse(model == "baseline_model", "network", "model")
      cat(
        common[[model]]$mtcResults[[location]]$code,
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
    if (model == "baseline_model"){
      common[[model]]$mtcResults$inits
    } else {
      common[[model]]$mtcResults$model$inits
    }
  })

  create_chain_initial_data_download_handler <- function(index) {
    downloadHandler(
      filename = glue::glue("MetaInsight_{model_type}_initial_values_chain_{index}.txt"),
      content = function(file) {
        # this should be able to be more elegant, but I can't get it to work.
        if (model == "baseline_model") {
          lapply(
            paste(names(common[[model]]$mtcResults$inits[[index]]),
                  common[[model]]$mtcResults$inits[[index]],
                  "\n"),
            write,
            file,
            append = TRUE,
            ncolumns = 1000
          )
        } else {
          lapply(
            paste(names(common[[model]]$mtcResults$model$inits[[index]]),
                  common[[model]]$mtcResults$model$inits[[index]],
                  "\n"),
            write,
            file,
            append = TRUE,
            ncolumns = 1000
          )
        }
      }
    )
  }

  output$download_inits_1 <- create_chain_initial_data_download_handler(1)
  output$download_inits_2 <- create_chain_initial_data_download_handler(2)
  output$download_inits_3 <- create_chain_initial_data_download_handler(3)
  output$download_inits_4 <- create_chain_initial_data_download_handler(4)

  create_chain_data_download_handler <- function(index) {
    downloadHandler(
      filename = glue::glue("MetaInsight_{model_type}_data_for_chain_{index}.csv"),
      content = function(file) {
        data <- as.data.frame(common[[model]]$mtcResults$samples[[index]])
        write.csv(data, file)
      }
    )
  }

  output$download_data_1 <- create_chain_data_download_handler(1)
  output$download_data_2 <- create_chain_data_download_handler(2)
  output$download_data_3 <- create_chain_data_download_handler(3)
  output$download_data_4 <- create_chain_data_download_handler(4)

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
    if (model == "bayes_all"){
      validate(need(common[[deviance]], "Please run the Deviance report module first"))
      common[[deviance]]$deviance_ume
    } else {
      NULL
    }
  })

  # this is a bit messy, but the simplest way to display the _sub deviance
  output$dev_mtc_sub <- renderPrint({
    req(model == "bayes_all")
    watch(gsub("all", "sub", deviance_trigger))
    watch(gsub("all", "sub", model_trigger))
    req(watch(module_id) > 0)
    validate(need(common[[deviance]], "Please run the Deviance report module first"))
    deviance <- gsub("all", "sub", deviance)
    common[[deviance]]$deviance_mtc
  })

  output$dev_ume_sub <- renderPrint({
    req(model == "bayes_all")
    watch(gsub("all", "sub", deviance_trigger))
    watch(gsub("all", "sub", model_trigger))
    req(watch(module_id) > 0)
    validate(need(common[[deviance]], "Please run the Deviance report module first"))
    common[[deviance]]$deviance_ume
  })

  output$deviance <- renderUI({
    ns <- session$ns
    if (model == "bayes_all"){
      tagList(
        layout_columns(
          div(
            p("NMA (consistency) model for all studies"),
            verbatimTextOutput(ns("dev_mtc")),
            p("UME (inconsistency) model for all studies"),
            verbatimTextOutput(ns("dev_ume"))
          ),
          div(
            p("NMA (consistency) model excluding selected studies"),
            verbatimTextOutput(ns("dev_mtc_sub")),
            p("UME (inconsistency) model excluding selected studies"),
            verbatimTextOutput(ns("dev_ume_sub"))
          )
        )
      )
    } else {
      tagList(p("NMA (consistency) model"),
              verbatimTextOutput(ns("dev_mtc"))
             )
    }
  })

  })
}

bayes_details_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    bayes_details_submodule_server("bayes", common, "bayes_all", "bayes_model_all", "bayes_details", "bayes_deviance_all", "bayes_deviance",
                                   "Please fit the Bayesian models first", reactive(input$run))
})
}

bayes_details_submodule_result <- function(id, class) {
  ns <- NS(id)
  tagList(
    div(class = paste(class, "details_download"),
        tabsetPanel(id = ns("tabs"),
          tabPanel(
            value = "mcmc",
            title = "MCMC characteristics and priors",
            downloadButton(ns('download_mcmc'), "Download MCMC"),
            downloadButton(ns('download_priors'), "Download priors"),
            h4(tags$strong("MCMC characteristics")),
            tableOutput(ns("mcmc")),
            h4(tags$strong("Prior distributions")),
            tableOutput(ns("priors")),
            br(),
            p("Note: Normal distributions are parameterized here as N(mean, variance) and in the JAGS code as N(mean, precision)."),
            p("Note: For help on the scaled t-distribution see the JAGS manual.")
          ),
          tabPanel(
            value = "code",
            title = "Model codes",
            p(tags$strong("Model codes for analysis of all studies")),
            downloadButton(ns("download_code")),
            verbatimTextOutput(ns("code"))
          ),
          tabPanel(
            value = "inits",
            title = "Initial values",
            p(tags$strong("Initial values")),
            downloadButton(ns('download_inits_1'), "Download initial values for chain 1"),
            downloadButton(ns('download_inits_2'), "Download initial values for chain 2"),
            downloadButton(ns('download_inits_3'), "Download initial values for chain 3"),
            downloadButton(ns('download_inits_4'), "Download initial values for chain 4"),
            verbatimTextOutput(ns("inits"))
          ),
          tabPanel(
            value = "sims",
            title = "Download simulations",
            p(tags$strong("Download simulated data")),
            downloadButton(ns('download_data_1'), "Download data from chain 1"),
            downloadButton(ns('download_data_2'), "Download data from chain 2"),
            downloadButton(ns('download_data_3'), "Download data from chain 3"),
            downloadButton(ns('download_data_4'), "Download data from chain 4")
          ),
          tabPanel(
            value = "dev",
            title = "Deviance details",
            uiOutput(ns("deviance"))
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

