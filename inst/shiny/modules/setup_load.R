setup_load_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    div(
      h4(tags$strong("Select outcome type")),
      radioButtons(ns("metaoutcome"), NULL, choices = c(
          "Continuous (e.g. mean difference) " = "Continuous",
          "Binary (e.g. Odds Ratio)" = "Binary"
        ),
      ),
      h4(tags$strong("Select data format")),
      radioButtons(ns("format"), NULL, choices = c(
        "Long" = "long",
        "Wide" = "wide"
      ),
      ),
      h4(tags$strong("Select a data file (.csv or .xlsx) to upload")),
      p(tags$strong("Default maximum file size is 5MB.")),
      uiOutput(ns("data_out")),
      actionButton(ns("run"), "Load example data"),
      uiOutput(ns("reset_out")),
      br(),
      uiOutput(ns("download_out"))
    )
  )
}

setup_load_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  ns <- session$ns

  # Toggle visibility of guidance depending on selection
  observe({
    selection <- paste0(tolower(input$metaoutcome),"_guidance")
      shinyjs::runjs(sprintf("
                     document.querySelectorAll('.continuous_guidance, .binary_guidance').forEach(el => el.style.display = 'none');
                     document.querySelectorAll('.%s').forEach(el => el.style.display = 'block');
                     ", selection))
  })

  observe({
    selection <- paste0(input$format,"_guidance")
    shinyjs::runjs(sprintf("
                     document.querySelectorAll('.long_guidance, .wide_guidance').forEach(el => el.style.display = 'none');
                     document.querySelectorAll('.%s').forEach(el => el.style.display = 'block');
                     ", selection))
  })


  # Create a definable reactive value to allow reloading of data
  gargoyle::init("load_reset")

  # Render function for file input dynamically to allow the button to be set to Null
  output$data_out <- renderUI({
    gargoyle::watch("load_reset")
    fileInput(ns("data"), label = NULL, buttonLabel = "Select", accept = c(".csv", ".xlsx"))
  })

  # Update run button if a file has been uploaded
  observe({
    if (!is.null(input$data)){
      updateActionButton(session, "run", label = "Load data")
    }
  })

  output$reset_out <- renderUI({
    gargoyle::watch("setup_load")
    gargoyle::watch("load_reset")
    req(common$is_data_uploaded)
    div(
      style = "float:right",
      actionButton(ns("reset"), "Delete data",
                   icon = icon("trash"),
                   style = "color: #fff; background-color: #dc3545; border-color: #dc3545")
    )
  })

  observeEvent(input$reset, {
    common$reset()
    updateActionButton(session, "run", label = "Load example data")
    gargoyle::trigger("load_reset")
   })

  output$download_out <- renderUI({
    gargoyle::watch("setup_load")
    gargoyle::watch("load_reset")
    if (is.null(common$is_data_uploaded) || !common$is_data_uploaded){
      downloadButton(ns("download"), "Download example data")
    }
  })

  observeEvent(input$run, {
    # WARNING ####
    # none for this module

    # FUNCTION CALL ####
    result <- setup_load(input$data$datapath, input$metaoutcome, common$logger)

    if (result$is_data_valid){
      if (result$is_data_uploaded){
        common$logger %>% writeLog(type= "complete", "Data was uploaded successfully")
      } else {
        common$logger %>% writeLog(type= "complete", glue::glue("Default {tolower(input$metaoutcome)} data has been loaded"))
      }
    }

    # LOAD INTO COMMON ####
    common$data <- result$data
    common$is_data_valid <- result$is_data_valid
    common$is_data_uploaded <- result$is_data_uploaded
    common$treatment_df <- result$treatment_df
    common$metaoutcome <- input$metaoutcome

    # METADATA ####
    common$meta$setup_load$used <- TRUE
    common$meta$setup_load$data <- common$data
    common$meta$setup_load$metaoutcome <- input$metaoutcome
    common$meta$setup_load$format <- input$format

    # TRIGGER
    gargoyle::trigger("setup_load")

    show_results(parent_session)


  })

  output$download <- downloadHandler(
    filename = glue::glue("MetaInsight_{tolower(input$metaoutcome)}_{input$format}.csv"),
    content = function(file) {
      file.copy(
        system.file("extdata",
                    glue::glue("{tolower(input$metaoutcome)}_{input$format}.csv"),
                    package = "metainsight"),
        file)
    }
  )

  # show the loaded data in the results tab
  output$data <- DT::renderDataTable({
    gargoyle::watch("setup_load")
    req(common$data)
    common$data
  })

  return(list(
    save = function() {list(
      ### Manual save start
      ### Manual save end
      metaoutcome = input$metaoutcome, 
      format = input$format)
    },
    load = function(state) {
      ### Manual load start
      ### Manual load end
      updateRadioButtons(session, "metaoutcome", selected = state$metaoutcome) 
      updateRadioButtons(session, "format", selected = state$format)
    }
  ))
})
}

setup_load_module_result <- function(id) {
  ns <- NS(id)
  DT::dataTableOutput(ns("data"))
}

setup_load_module_rmd <- function(common){ list(
  setup_load_knit = !is.null(common$meta$setup_load$used),
  setup_load_data = common$meta$setup_load$data,
  setup_load_metaoutcome = common$meta$setup_load$metaoutcome,
  setup_load_format = common$meta$setup_load$format)
}

