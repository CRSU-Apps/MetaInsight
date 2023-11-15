
#' Module UI for the study exclusion panel.
#' 
#' @param id ID of the module.
#' @return Div for the panel.
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


#' Module server for the study exclusion panel.
#' 
#' @param id ID of the module.
#' @param data Reactive containing data to analyse.
#' @param treatment_df Reactive containing data frame containing treatment IDs (Number) and names (Label).
#' 
#' @return List of reactives:
#' - "initial_connected_data" is a data frame containing only the studies which form a connected network, containing the reference treatment.
#' - "initial_connected_treatment_list" is a data frame containing the updated treatment IDs for the connected data.
#' - "filtered_connected_data" is a data frame containing only the filtered studies which form a connected network, containing the reference treatment.
#' - "filtered_connected_treatment_list" is a data frame containing the updated treatment IDs for the connected filtered data.
study_exclusions_panel_server <- function(id, data, treatment_df) {
  moduleServer(id, function(input, output, session) {
    all_exclusions <- debounce(
      reactive({ input$exclusionbox }),
      millis = 1500
    )
    
    all_studies <- reactive({
      unique(data()$Study)
    })
    
    ### Primary subnetwork
    
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
    
    ### Filtered subnetwork
    
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
    
    ### Event handlers
    
    # Create check boxes for studies in data
    observe({
      priority = 1003
      shiny::updateCheckboxGroupInput(
        inputId = "exclusionbox",
        choices = all_studies()
      )
    })
    
    # Select check boxes for disconnected studies
    observe(
      priority = 1002,
      {
        disconnected_indices <- which(all_studies() %in% initial_subnetwork_exclusions() | all_studies() %in% filtered_subnetwork_exclusions())
        selected = unique(c(input$exclusionbox, all_studies()[disconnected_indices]))
        shiny::updateCheckboxGroupInput(inputId = "exclusionbox", selected = selected)
      }
    )
    
    # Disable check boxes for disconnected studies
    observeEvent(
      priority = 1001,
      eventExpr = {
        all_exclusions()
      },
      handlerExpr = {
        shinyjs::enable(id = "exclusionbox")

        filtered_treatments <- FindAllTreatments(filtered_data())

        lapply(
          all_studies(),
          function(study) {

            study_treatments <- FindAllTreatments(data()[data()$Study == study, ])

            if (any(study_treatments %in% filtered_treatments)) {
              return()
            }

            index <- match(study, all_studies())

            subElement <- glue::glue("#{session$ns('exclusionbox')} .checkbox:nth-child({index}) label")
            shinyjs::disable(selector = subElement)
          }
        )
      }
    )
    
    initial_connected_dewrangled_data <- reactive({
      ReinstateTreatmentIds(initial_connected_data(), treatment_df())
    })
    
    initial_connected_treatment_list <- reactive({
      treatments <- FindAllTreatments(initial_connected_dewrangled_data())
      return(CreateTreatmentIds(treatments))
    })
    
    initial_connected_wrangled_data <- reactive({
      ReplaceTreatmentIds(initial_connected_dewrangled_data(), initial_connected_treatment_list())
    })
    
    filtered_connected_dewrangled_data <- reactive({
      ReinstateTreatmentIds(filtered_connected_data(), treatment_df())
    })
    
    filtered_connected_treatment_list <- reactive({
      treatments <- FindAllTreatments(filtered_connected_dewrangled_data())
      return(CreateTreatmentIds(treatments))
    })
    
    filtered_connected_wrangled_data <- reactive({
      ReplaceTreatmentIds(filtered_connected_dewrangled_data(), filtered_connected_treatment_list())
    })
    
    return(
      list(
        initial_connected_data = initial_connected_wrangled_data,
        initial_connected_treatment_list = initial_connected_treatment_list,
        filtered_connected_data = filtered_connected_wrangled_data,
        filtered_connected_treatment_list = filtered_connected_treatment_list
      )
    )
  })
}