
#' Module UI for the model details panel
#' 
#' @param id ID of the module
#' @param item_names Vector of analysis titles to be shown side-by-side in the page.
#' @param page_numbering PageNumbering object for giving each page a unique identifier in the UI
#' @return Div for the panel
model_details_panel_ui <- function(id, item_names, page_numbering) {
  ns <- NS(id)
  
  page_numbering$DiveLevel()
  
  # Matrix containing plots for named items
  index <- 1
  divs <- sapply(
    item_names,
    function(name) {
      divs <- list(
        deviance = div(
          p(tags$strong("Deviance data for", name)),
          p("NMA (consistency) model"),
          verbatimTextOutput(outputId = ns(glue::glue("dev_{index}")))
        ),
        inconsistency = div(
          p("UME (inconsistency) model"),
          verbatimTextOutput(outputId = ns(glue::glue("dev_ume_{index}")))
        )
      )
      index <<- index + 1
      return(divs)
    }
  )
  
  ui = div(
    tabsetPanel(
      tabPanel(
        title = paste0(page_numbering$AddChild(), " MCMC characteristics and priors"),
        invalid_model_panel_ui(id = ns("model_invalid_1")),
        downloadButton(outputId = ns('download_mcmc'),
                       label = "Download MCMC"),
        downloadButton(outputId = ns('download_priors'),
                       label = "Download priors"),
        h4(tags$strong("MCMC characteristics")),
        tableOutput(outputId = ns("mcmc_details")),
        h4(tags$strong("Prior distributions")),
        tableOutput(outputId = ns("priors")),
        br(),
        p("Note: Normal distributions are parameterized here as N(mean, variance) and in the JAGS code as N(mean, precision)."),
        p("Note: For help on the scaled t-distribution see the JAGS manual.")
      ),
      tabPanel(
        title = paste0(page_numbering$AddChild(), " Model codes"),
        invalid_model_panel_ui(id = ns("model_invalid_2")),
        p(tags$strong("Model codes for analysis of all studies")),
        downloadButton(outputId = ns('download_code')),
        verbatimTextOutput(outputId = ns("code"))
      ),
      tabPanel(
        title = paste0(page_numbering$AddChild(), " Initial values"),
        invalid_model_panel_ui(id = ns("model_invalid_3")),
        p(tags$strong("Initial values")),
        downloadButton(outputId = ns('download_inits_1'), "Download initial values for chain 1"),
        downloadButton(outputId = ns('download_inits_2'), "Download initial values for chain 2"),
        downloadButton(outputId = ns('download_inits_3'), "Download initial values for chain 3"),
        downloadButton(outputId = ns('download_inits_4'), "Download initial values for chain 4"),
        verbatimTextOutput(outputId = ns("inits"))
      ),
      tabPanel(
        title = paste0(page_numbering$AddChild(), " Download simulations"),
        invalid_model_panel_ui(id = ns("model_invalid_4")),
        p(tags$strong("Download simulated data")),
        downloadButton(outputId = ns('download_data1'), "Download data from chain 1"),
        br(),
        downloadButton(outputId = ns('download_data2'), "Download data from chain 2"),
        br(),
        downloadButton(outputId = ns('download_data3'), "Download data from chain 3"),
        br(),
        downloadButton(outputId = ns('download_data4'), "Download data from chain 4")
      ),
      tabPanel(
        title = paste0(page_numbering$AddChild(), " Deviance details"),
        
        # This is the way to get a dynamic number of columns rendered into the row
        do.call(
          fluidRow,
          lapply(
            1:length(item_names),
            function(index) {
              name <- item_names[[index]]
              column(
                width = 12 / length(item_names),
                invalid_model_panel_ui(id = ns(glue::glue("model_invalid_4_{index}"))),
                divs["deviance", name]
              )
            }
          )
        ),
      
        # This is the way to get a dynamic number of columns rendered into the row
        conditionalPanel(
          condition = "output.model_type == 'consistency'",
          ns = ns,
          do.call(
            fluidRow,
            lapply(
              item_names,
              function(name) {
                column(
                  width = 12 / length(item_names),
                  divs["inconsistency", name]
                )
              }
            )
          )
        )
      )
    )
  )
  
  page_numbering$FloatLevel()
  
  return(ui)
}


#' Module server for the model details panel.
#' 
#' @param id ID of the module
#' @param models Vector of reactives containing bayesian meta-analyses.
#' @param models_valid Vector of reactives containing whether each model is valid.
#' @param package "gemtc" (default) or "bnma".
model_details_panel_server <- function(id, models, models_valid, package = "gemtc") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    main_model <- models[[1]]
    main_model_valid <- models_valid[[1]]
    
    invalid_model_panel_server(id = "model_invalid_1", model_valid = main_model_valid)
    invalid_model_panel_server(id = "model_invalid_2", model_valid = main_model_valid)
    invalid_model_panel_server(id = "model_invalid_3", model_valid = main_model_valid)
    invalid_model_panel_server(id = "model_invalid_4", model_valid = main_model_valid)
    
    observe({
      # Get either enable or disable function to apply to relevant UI elements
      if (is.null(main_model_valid()) || !main_model_valid()) {
        fn <- shinyjs::disable
      } else {
        fn <- shinyjs::enable
      }
      
      # Apply enable or disable function
      fn(id = glue::glue("download_mcmc"))
      fn(id = glue::glue("download_priors"))
      fn(id = glue::glue("download_code"))

      sapply(
        1:4,
        function(index) {
          fn(id = glue::glue("download_inits_{index}"))
          fn(id = glue::glue("download_data{index}"))
        }
      )
    })
    
    #Tab 1: MCMC characteristics and prior distributions
    mcmc_details <- reactive({
      if (package == "gemtc") {
        return(GetGemtcMcmcCharacteristics(model = main_model()$mtcResults))
      } else if (package == "bnma") {
        return(GetBnmaMcmcCharacteristics(model = main_model()))
      }
    })
    
    priors <- reactive({
      if (package == "gemtc") {
        return(GetGemtcPriors(model = main_model()$mtcResults))
      } else if (package == "bnma") {
        return(GetBnmaPriors(model = main_model()))
      }
    })
    
    output$mcmc_details <- renderTable(
      {
        mcmc_details()
      },
      digits = 0,
      colnames = FALSE
    )
    
    output$priors <- renderTable(
      {
        priors()
      },
      colnames = FALSE
    )
    
    output$download_mcmc <- downloadHandler(
      filename = "mcmc_characteristics.csv",
      content = function(file) {
        data <- mcmc_details()
        write.csv(data, file, row.names = FALSE, col.names = FALSE)
      }
    )
    
    output$download_priors <- downloadHandler(
      filename = "prior_distributions.csv",
      content = function(file) {
        data <- priors()
        write.csv(data, file, row.names = FALSE, col.names = FALSE)
      }
    )
    
    #Tab 2: Model codes
    output$code <- renderPrint({
      if (is.null(main_model_valid()) || !main_model_valid()) {
        return()
      }
      if (package == "gemtc") {
        return(cat(main_model()$mtcResults$model$code, fill = FALSE, labels = NULL, append = FALSE))
      } else if (package == "bnma") {
        return(cat(main_model()$network$code, fill = FALSE, labels = NULL, append = FALSE))
      }
    })

    output$download_code <- downloadHandler(
      filename = "code.txt",
      content = function(file) {
        cat(
          main_model()$mtcResults$model$code,
          file = file,
          fill = FALSE,
          labels = NULL,
          append = FALSE
        )  # write the code into a file for download
      }
    )
    
    #Tab 3: Initial values
    inits <- reactive({
      if (is.null(main_model_valid()) || !main_model_valid()) {
        return()
      }
      if (package == "gemtc") {
        return(main_model()$mtcResults$model$inits)
      } else if (package == "bnma") {
        return(main_model()$inits)
      }
    })
    
    output$inits <- renderPrint({inits()})
    
    # Download handlers for download buttons
    sapply(
      1:4,
      function(index) {
        output[[glue::glue("download_inits_{index}")]] <- .create_chain_initial_data_download_handler(index, inits)
      }
    )

    #Tab 4: Chain data.

    samples <- reactive({
      if (is.null(main_model_valid()) || !main_model_valid()) {
        return()
      }
      if (package == "gemtc") {
        return(main_model()$mtcResults$samples)
      } else if (package == "bnma") {
        return(main_model()$samples)
      }
    })
    
    # Download handlers for download buttons
    sapply(
      1:4,
      function(index) {
        output[[glue::glue("download_data{index}")]] <- .create_chain_data_download_handler(index, samples)
      }
    )

    #Tab 5: Deviance
    
    model_type <- reactive({
      for (index in 1:length(models)) {
        if (is.null(models_valid[[index]]()) || !models_valid[[index]]()) {
          next
        }
        if (package == "gemtc") {
          return(models[[index]]()$mtcResults$model$type)
        } else if (package == "bnma") {
          return("baseline risk")
        }
      }
      
      return("invalid")
    })
    
    output$model_type <- reactive({
      model_type()
    })
    outputOptions(x = output, name = "model_type", suspendWhenHidden = FALSE)
    
    # Create server for each model
    sapply(
      1:length(models),
      function(index) {
        mod = models[[index]]
        mod_valid <- models_valid[[index]]
        
        invalid_model_panel_server(id = glue::glue("model_invalid_4_{index}"), model_valid = mod_valid)
        
        # NMA consistency model
        output[[glue::glue("dev_{index}")]] <- renderPrint({
          if (is.null(mod_valid()) || !mod_valid()) {
            return()
          }
          if (package == "gemtc") {
            return(mtc.deviance({mod()$mtcResults}))
          } else if (package == "bnma") {
            return(mod()$deviance)
          }
        })
        
        # UME inconsistency model
        output[[glue::glue("dev_ume_{index}")]] <- renderText({
          if (is.null(mod_valid()) || !mod_valid()) {
            return()
          }
          if (model_type() != "consistency") {
            return(NULL)
          } else {
            printed_output <- capture.output(scat_plot(mod())$y)
            
            # Strip out progress bars
            progress_bar_lines <- grep("^(\\s*\\|\\s+\\|(\\+|\\*)*?\\s*\\|\\s+[0-9]+%)+$", printed_output)
            printed_output <- printed_output[-progress_bar_lines]
            
            return(paste0(printed_output, collapse = "\n"))
          }
        })
      }
    )
  })
}

#' Create a download handler for the initial values for a given chain
#'
#' @param index the Index of the chain
#' @return The created download handler
.create_chain_initial_data_download_handler <- function(index, inits) {
  filename <- paste0("initialvalues_chain", index, ".txt")
  
  return(
    downloadHandler(
      filename = filename,
      content = function(file) {
        lapply(
          paste(names(inits()[[index]]),
                inits()[[index]],
                "\n"),
          write,
          file,
          append = TRUE,
          ncolumns = 1000
        )
      }
    )
  )
}

#' Create a download handler for the data for a given chain
#'
#' @param index the Index of the chain
#' @return The created download handler
.create_chain_data_download_handler <- function(index, samples) {
  return(
    downloadHandler(
      filename = paste0("data_for_chain_", index, ".csv"),
      content = function(file) {
        data <- as.data.frame(samples()[[index]])
        write.csv(data, file)
      }
    )
  )
}

