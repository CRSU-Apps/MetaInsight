setup_load_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    radioButtons(ns("outcome"), "Select outcome type", choices = c(
      "Continuous (e.g. mean difference) " = "Continuous",
      "Binary (e.g. Odds Ratio)" = "Binary")
    ),
    radioButtons(ns("format"), "Select data format", choices = c(
      "Long" = "long",
      "Wide" = "wide")
    ),
    p(tags$strong("Maximum file size is 5MB.")),
    uiOutput(ns("data_out")),
    actionButton(ns("run"), "Load example data", icon = icon("arrow-turn-down")),
    uiOutput(ns("reset_out")),
    br(),
    uiOutput(ns("download_out"))
  )
}

setup_load_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Toggle visibility of guidance depending on selection
    observe({
      selection <- paste0(tolower(input$outcome),"_guidance")
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
    init("setup_reset")

    # make this id flexible to enable complete reset
    file_id <- reactiveValues(id = "file1", value = 1)

    # Render function for file input dynamically to allow the button to be set to Null
    output$data_out <- renderUI({
      watch("setup_reset")
      fileInput(ns(file_id$id), label = "Select a data file (.csv or .xlsx) to upload", buttonLabel = "Select", accept = c(".csv", ".xlsx"))
    })

    # Update run button if a file has been uploaded
    observe({
      if (!is.null(input[[file_id$id]])){
        updateActionButton(session, "run", label = "Load data")
      }
    })

    output$reset_out <- renderUI({
      watch("setup_load")
      watch("setup_reset")
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
      file_id$value <- file_id$value + 1
      file_id$id <- paste0("file", file_id$value)
      trigger("setup_reset")
    })

    output$download_out <- renderUI({
      watch("setup_load")
      watch("setup_reset")
      if (is.null(common$is_data_uploaded) || !common$is_data_uploaded){
        downloadButton(ns("download"), "Download example data")
      }
    })

    observeEvent(input$run, {
      # WARNING ####
      # none for this module
      # FUNCTION CALL ####
      result <- setup_load(input[[file_id$id]]$datapath, input$outcome, common$logger)

      if (result$is_data_valid){
        if (result$is_data_uploaded){
          common$logger %>% writeLog(type= "complete", "Data was uploaded successfully")
        } else {
          common$logger %>% writeLog(type= "complete", glue::glue("Default {tolower(input$outcome)} data has been loaded"))
        }
      }

      # LOAD INTO COMMON ####
      common$data <- result$data
      common$is_data_valid <- result$is_data_valid
      common$is_data_uploaded <- result$is_data_uploaded
      common$treatment_df <- result$treatment_df
      common$outcome <- input$outcome

      # METADATA ####
      common$meta$setup_load$used <- TRUE
      common$meta$setup_load$outcome <- input$outcome
      common$meta$setup_load$format <- input$format

      # TRIGGER
      trigger("setup_load")

      show_results(parent_session)


    })

    output$download <- downloadHandler(
      filename = glue::glue("MetaInsight_{tolower(input$outcome)}_{input$format}.csv"),
      content = function(file) {
        file.copy(
          system.file("extdata",
                      glue::glue("{tolower(input$outcome)}_{input$format}.csv"),
                      package = "metainsight"),
          file)
      }
    )

    # show the loaded data in the results tab
    output$table <- DT::renderDataTable({
      watch("setup_load")
      req(common$data)
      common$data
    })

    return(list(
      save = function() {list(
        ### Manual save start
        ### Manual save end
        outcome = input$outcome,
        format = input$format)
      },
      load = function(state) {
        ### Manual load start
        ### Manual load end
        updateRadioButtons(session, "outcome", selected = state$outcome)
        updateRadioButtons(session, "format", selected = state$format)
      }
    ))
  })
}

setup_load_module_result <- function(id) {
  ns <- NS(id)
  DT::dataTableOutput(ns("table"))
}

setup_load_module_rmd <- function(common){ list(
  setup_load_knit = !is.null(common$meta$setup_load$used),
  setup_load_data = common$data,
  setup_load_treatment_df = common$treatment_df,
  setup_load_outcome = common$meta$setup_load$outcome
)
}

