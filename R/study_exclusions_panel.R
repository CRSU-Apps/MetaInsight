
#' Module UI for the data analysis options panel
#' 
#' @param id ID of the module
#' @return Div for the panel
study_exclusions_panel_ui <- function(id) {
  ns <- NS(id)
  div(
    h3("Select studies to exclude:"),
    checkboxGroupInput(
      inputId = ns("exclusionbox"),
      label = NULL,
      choices = c()
    )
  )
}


#' Module server for the data analysis options panel
#' 
#' @param id ID of the module
#' @param data Reactive containing data to analyse
#' @param treatment_df Reactive containing data frame containing treatment IDs (Number) and names (Label)
#' 
#' @return Vector of names of studies being excluded from the sensitivity analysis
study_exclusions_panel_server <- function(id, data, treatment_df) {
  moduleServer(id, function(input, output, session) {
    all_exclusions <- debounce(
      reactive({ input$exclusionbox }),
      millis = 1500
    )
    
    all_studies <- reactive({
      unique(data()$Study)
    })
    
    # Create check boxes for studies in data
    observe({
      priority = 2
      shiny::updateCheckboxGroupInput(
        inputId = "exclusionbox",
        choices = all_studies()
      )
    })
    
    initial_connected_data <- reactive({
      indices <- 1:length(data()$Study)
      subnetworks <- IdentifySubNetworks(data(), treatment_df())
      primary_network <- subnetworks$subnet_1
      
      connected_indices <- indices[data()$Study %in% primary_network$studies]
      return(data()[connected_indices, ])
    })
    
    initial_subnetwork_exclusions <- reactive({
      all_studies()[!all_studies() %in% initial_connected_data()$Study]
    })
    
    
    
    
    
    ######
    ### This is incorrect because once the study is identified as being disconnected by the filtering,
    ### it is checked, then it's only noted as filtered out and no longer identified as needing to be disabled
    #######
    
    filtered_data <- reactive({
      data()[!data()$Study %in% all_exclusions(), ]
    })
    
    filtered_studies <- reactive({
      unique(filtered_data()$Study)
    })
    
    
    
    
    filtered_connected_data <- reactive({
      indices <- 1:length(filtered_data()$Study)
      
      subnetworks <- IdentifySubNetworks(filtered_data(), treatment_df())
      primary_network <- subnetworks$subnet_1
      
      connected_indices <- indices[filtered_data()$Study %in% primary_network$studies]
      return(filtered_data()[connected_indices, ])
    })
    
    filtered_subnetwork_exclusions <- reactive({
      filtered_studies()[!filtered_studies() %in% filtered_connected_data()$Study]
    })
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    # Disable check boxes for disconnected studies
    observe(
      priority = 1,
      {
        shinyjs::enable(id = "exclusionbox")
        
        print(initial_subnetwork_exclusions())
        print(filtered_subnetwork_exclusions())
        print("---")
        
        disconnected_indices <- which(all_studies() %in% initial_subnetwork_exclusions() | all_studies() %in% filtered_subnetwork_exclusions())
        selected = unique(c(input$exclusionbox, all_studies()[disconnected_indices]))
        shiny::updateCheckboxGroupInput(inputId = "exclusionbox", selected = selected)
        
        lapply(
          disconnected_indices,
          function(index) {
            subElement <- glue::glue("#{session$ns('exclusionbox')} .checkbox:nth-child({index}) label")
            # This delay shouldn't be needed, but the checkboxes aren't disabled without it
            # shinyjs::delay(
            #   ms = 0,
              shinyjs::disable(selector = subElement)
            # )
          }
        )
        print("------")
      }
    )
    
    return(all_exclusions)
  })
}