
#' Module UI for the model details panel
#' 
#' @param id ID of the module
#' @return Div for the panel
model_details_panel_ui <- function(id, item_names) {
  ns <- NS(id)
  
  # Matrix containing plots for named items
  index <- 0
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
  
  div(
    helpText(
      "Please note: if you change the selections on the sidebar,
      you will need to re-run the primary and/or sensitivity analysis from the 'Forest Plot' page."
    ),
    tabsetPanel(
      tabPanel(
        title = "3g-1. Model codes",
        p(tags$strong("Model codes for analysis of all studies")),
        downloadButton(outputId = ns('download_code')),
        verbatimTextOutput(outputId = ns("code"))
      ),
      tabPanel(
        title = "3g-2. Initial values",
        p(tags$strong("Initial values")),
        downloadButton(outputId = ns('download_inits_1'), "Download initial values for chain 1"),
        downloadButton(outputId = ns('download_inits_2'), "Download initial values for chain 2"),
        downloadButton(outputId = ns('download_inits_3'), "Download initial values for chain 3"),
        downloadButton(outputId = ns('download_inits_4'), "Download initial values for chain 4"),
        verbatimTextOutput(outputId = ns("inits"))
      ),
      tabPanel(
        title = "3g-3. Download simulations",
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
        title = "3g-4. Deviance details",
        
        # This is the way to get a dynamic number of columns rendered into the row
        do.call(
          fluidRow,
          lapply(
            item_names,
            function(name) {
              column(
                width = 12 / length(item_names),
                divs["deviance", name]
              )
            }
          )
        ),
      
        # This is the way to get a dynamic number of columns rendered into the row
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
}


#' Module server for the model details panel.
#' 
#' @param id ID of the module
#' @param model Reactive containing bayesian meta-analysis for all studies
#' @param model_sub Reactive containing meta-analysis with studies excluded
model_details_panel_server <- function(id, models) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    main_model <- models[[1]]

    # 3g-1 Model codes
    output$code <- renderPrint({
      cat(main_model()$mtcResults$model$code, fill = FALSE, labels = NULL, append = FALSE)
    })

    output$download_code <- downloadHandler(
      filename = "code.txt",
      content = function(file) {
        file.copy("./codes.txt", file)
      }
    )

    # 3g-2 Initial values
    output$inits <- renderPrint({
      main_model()$mtcResults$model$inits
    })

    #' Create a download handler for the initial values for a given chain
    #'
    #' @param index the Index of the chain
    #' @return The created download handler
    create_chain_initial_data_download_handler <- function(index) {
      filename <- paste0("initialvalues_chain", index, ".txt")
      return(
        downloadHandler(
          filename = filename,
          content = function(file) {
            lapply(
              main_model()$mtcResults$model$inits[[index]],
              write,
              file,
              append = TRUE,
              ncolumns = 1000
            )
          }
        )
      )
    }

    output$download_inits_1 <- create_chain_initial_data_download_handler(1)
    output$download_inits_2 <- create_chain_initial_data_download_handler(2)
    output$download_inits_3 <- create_chain_initial_data_download_handler(3)
    output$download_inits_4 <- create_chain_initial_data_download_handler(4)

    # 3g-3 Chain data.

    #' Create a download handler for the data for a given chain
    #'
    #' @param index the Index of the chain
    #' @return The created download handler
    create_chain_data_download_handler <- function(index) {
      return(
        downloadHandler(
          filename = paste0("data_for_chain_", index, ".csv"),
          content = function(file) {
            data <- as.data.frame(main_model()$mtcResults$samples[[index]])
            write.csv(data, file)
          }
        )
      )
    }

    output$download_data1 <- create_chain_data_download_handler(1)
    output$download_data2 <- create_chain_data_download_handler(2)
    output$download_data3 <- create_chain_data_download_handler(3)
    output$download_data4 <- create_chain_data_download_handler(4)

    # 3g-4 Output deviance
    
    # Create server for each model
    index <- 0
    sapply(
      models,
      function(mod) {
        # NMA consistency model
        output[[glue::glue("dev_{index}")]] <- renderPrint({
          mtc.deviance({mod()$mtcResults})
        })
        
        # UME inconsistency model
        output[[glue::glue("dev_ume_{index}")]] <- renderPrint({
          scat_plot(mod())$y
        })
        
        index <<- index + 1
      }
    )
  })
}