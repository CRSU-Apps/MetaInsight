
#' Module ui for uploading data into the app.
#' 
#' @param id ID of the module
#' @return Div containing data upload controls
data_input_panel_ui <- function(id) {
  ns <- NS(id)
  div(
    h4(tags$strong("Select a data file (.csv) to upload")),
    p(tags$strong("Files used before version 5.0 are no longer compatible. To use an older file, replace the numeric treatment IDs in the \"T\" column(s) with the treatment names.",
                  style = "color:red")),
    p(tags$strong("Note: Excel files should be saved in 'csv (Comma delimited) (*.csv)' format. Default maximum file size is 5MB.")),
    uiOutput(outputId = ns("file_input_panel")),
    selectizeInput(inputId = ns('reference_treatment'), label = 'Select Reference Treatment', choices = c()),
    conditionalPanel(
      condition = 'output.data_uploaded == true',
      ns = ns,
      div(
        style = "float:right",
        actionButton(inputId = ns("reload_button"),
                     label = "Delete Data",
                     incon = icon("trash"),
                     style = "color: #fff; background-color: #dc3545; border-color: #dc3545")
      )
    ),
    div(class = "clearfix")
  )
}

#' Find all of the treatment names in the data, both for long and wide formats.
#' 
#' @param data Data frame in which to search for treatment names
#' @return Vector of all treatment names
find_all_treatments <- function(data) {
  if ('T' %in% colnames(data)) {
    # Long format
    return(unique(data$T))
  } else {
    # Wide format
    all <- c()
    for (col in paste0('T.', seq(6))) {
      if (col %in% colnames(data)) {
        all <- c(all, data[[col]])
      }
    }
    return(unique(all[!is.na(all)]))
  }
}

#' Create a copy of a vector with the given item as the first element.
#' 
#' @param vector Vector to reorder
#' @param first_item The element to push to the front of the vector
#' @return The reordered vector
vector_with_item_first <- function(vector, first_item) {
  if (is.null(first_item) || !(first_item %in% vector)) {
    return(vector)
  }
  return(c(first_item, vector[vector != first_item]))
}

# Treatments are in priority order, such that or any study with multiple matching treatments,
# the first in this vector will be used as the reference.
potential_reference_treatments = c(
  'control',
  'usual_care',
  'standard_care',
  'placebo',
  'no_contact'
)

#' Find the expected reference treatment from a vector.
#' This is done by comparing treatment names to expected reference treatment names.
#' 
#' @param treatments vector containing all treatment names
#' @return Name of the expected reference treatment if one is found, else NULL
find_expected_reference_treatment <- function(treatments) {
  expected_reference_treatments <- unlist(
    lapply(potential_reference_treatments,
           function(expected_ref) {
             match(expected_ref, tolower(treatments))
           }))
  expected_reference_treatments <- expected_reference_treatments[!is.na(expected_reference_treatments)]
  if (length(expected_reference_treatments) > 0) {
    return(treatments[expected_reference_treatments[1]])
  } else {
    return(NULL)
  }
}

#' Module server for uploading data into the app.
#' 
#' @param id ID of the module
#' @param metaoutcome Reactive containing the outcome type selected
#' @return List of reactives:
#'   - 'data' is the uplodaded data
#'   - 'treatment_list' is the data frame containing the treatment ID ('Number') and the treatment name ('Label')
data_input_panel_server <- function(id, metaoutcome) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Create a definable reactive value to allow reloading of data
    reload <- reactiveVal(FALSE)

    # Render function for file input dynamically to allow the button to be set to Null
    default_file_input <- renderUI({
      fileInput(
        inputId = ns("data"),
        label = "",
        buttonLabel = "Select",
        placeholder = "No file selected"
      )
    })

    # Logical to show reset button only when data uploaded
    data_uploaded <- reactiveVal(FALSE)
    output$data_uploaded <- reactive({data_uploaded()})
    outputOptions(output, 'data_uploaded', suspendWhenHidden = FALSE)

    # Render the file input intially
    output$file_input_panel <- default_file_input
    
    # Load default data
    defaultD <- reactive({
      if (metaoutcome() == 'Continuous') {
        defaultD <- read.csv("Cont_long.csv")
      } else {
        defaultD <- read.csv("Binary_long.csv")
      }
    })
    
    # Make data reactive i.e. default or user uploaded
    data <- reactive({ 
      file1 <- input$data # Name the data file that was uploaded file1
      # if a reload is triggered show the reload the file input and data
      if (reload()) {
        output$file_input_panel <- default_file_input
        df <- defaultD()
      } else if (is.null(file1)) {
        # if data is triggered without reload, only load the default data
        df <- defaultD()
      } else {
        df <- read.table(file = file1$datapath,
                         sep = ",",
                         header = TRUE,
                         stringsAsFactors = FALSE,
                         quote = "\"",
                         fileEncoding = 'UTF-8-BOM')
      }
      return(dplyr::mutate(df, across(where(is.character), stringr::str_trim)))
    })
    
    # Create the treatment list with the reference treatment being the first item in the data frame
    treatment_list <- reactive({
      treatment_names = vector_with_item_first(find_all_treatments(data()), input$reference_treatment)
      return(data.frame(Number = seq(length(treatment_names)), Label = treatment_names))
    })
    
    
    #####
    # observer functions to trigger specific reactions
    #####
    
    # if the outcome is changed, reload the data and labels, reset the file input and hide the reload button
    observeEvent(metaoutcome(),
                 {
                   reload(TRUE)
                   output$file_input_panel <- default_file_input
                   data_uploaded(FALSE)
                 })
    
    # if the data is changed load the new data and show the reload button
    observeEvent(input$data,
                 {
                   reload(FALSE)
                   data_uploaded(TRUE)
                 })
    
    # if the reload button is clicked, reload the appropriate default data and labels and hide the reload button
    observeEvent(input$reload_button,
                 {
                   reload(TRUE)
                   output$file_input_panel <- default_file_input
                   data_uploaded(FALSE)
                 })
    
    # Reset the reference treatment when the data changes, by scanning through the uploaded data
    observe({
              treatments = unique(data()$T)
              updateSelectInput(inputId = 'reference_treatment',
                                choices = treatments,
                                selected = find_expected_reference_treatment(treatments))
            })
    
    return(list(data = data, treatment_list = treatment_list))
  })
}