
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
#' @param reference_treatment Reactive containing the ID of the selected reference treatment.
#' 
#' @return List of reactives:
#' - "initial_connected_data" is a data frame containing only the studies which form a connected network, containing the reference treatment.
#' - "initial_connected_treatment_list" is a data frame containing the updated treatment IDs for the connected data.
#' - "filtered_connected_data" is a data frame containing only the filtered studies which form a connected network, containing the reference treatment.
#' - "filtered_connected_treatment_list" is a data frame containing the updated treatment IDs for the connected filtered data.
#' - "filtered_reference_treatment" is the name of the reference treatment for the sensitivity analysis.
study_exclusions_panel_server <- function(id, data, treatment_df, reference_treatment) {
  moduleServer(id, function(input, output, session) {
    all_exclusions <- debounce(
      reactive({ input$exclusionbox }),
      millis = 1500
    )
    
    all_studies <- reactive({
      unique(data()$Study)
    })
    
    ### Primary subnetwork
    
    initial_subnetworks <- reactive({
      if (is.null(reference_treatment()) || reference_treatment() == "" || !(reference_treatment() %in% treatment_df()$Label)) {
        return(NULL)
      }
      return(IdentifySubNetworks(data(), treatment_df(), reference_treatment()))
    })
    
    initial_connected_data <- reactive({
      indices <- 1:length(data()$Study)
      subnetworks <- initial_subnetworks()
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
    
    filtered_subnetworks <- reactive({
      IdentifySubNetworks(filtered_data(), treatment_df())
    })
    
    filtered_connected_data <- reactive({
      indices <- 1:length(filtered_data()$Study)
      
      subnetworks <- filtered_subnetworks()
      primary_network <- subnetworks$subnet_1
      
      if (length(primary_network$studies) == 0) {
        primary_network <- subnetworks$subnet_2
      }
      
      connected_indices <- indices[filtered_data()$Study %in% primary_network$studies]
      return(filtered_data()[connected_indices, ])
    })
    
    filtered_subnetwork_exclusions <- reactive({
      filtered_studies()[!filtered_studies() %in% filtered_connected_data()$Study]
    })
    
    
    filtered_reference_treatment <- reactive({
      filtered_connected_treatment_list()$Label[1]
    })
    
    ### Event handlers
    
    # Inform the user that the uploaded data is disconnected
    observeEvent(
      initial_subnetworks(),
      {
        if (length(initial_subnetworks()) > 1) {
          shiny::showModal(
            modalDialog(
              title = "Disconnected Network",
              p(glue::glue("The uploaded data comprises a disconnected network. Only the subnetwork containing the reference treatment ({reference_treatment()}) will be displayed"))
            )
          )
        }
      }
    )
    
    # Inform the user that the sensitivity analysis data is disconnected
    observeEvent(
      all_exclusions(),
      {
        if (length(filtered_subnetwork_exclusions()) > 0) {
          shiny::showModal(
            modalDialog(
              title = "Disconnected Network",
              p(glue::glue("The filtered data comprises a disconnected network. Only the subnetwork containing the reference treatment ({filtered_reference_treatment()}) will be displayed"))
            )
          )
        }
      }
    )
    
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
      return(CreateTreatmentIds(treatments, reference_treatment = reference_treatment()))
    })
    
    initial_connected_wrangled_data <- reactive({
      ReplaceTreatmentIds(initial_connected_dewrangled_data(), initial_connected_treatment_list())
    })
    
    filtered_connected_dewrangled_data <- reactive({
      ReinstateTreatmentIds(filtered_connected_data(), treatment_df())
    })
    
    filtered_connected_treatment_list <- reactive({
      treatments <- FindAllTreatments(filtered_connected_dewrangled_data())
      return(CreateTreatmentIds(treatments, reference_treatment = reference_treatment()))
    })
    
    filtered_connected_wrangled_data <- reactive({
      ReplaceTreatmentIds(filtered_connected_dewrangled_data(), filtered_connected_treatment_list())
    })
    
    return(
      list(
        initial_connected_data = initial_connected_wrangled_data,
        initial_connected_treatment_list = initial_connected_treatment_list,
        filtered_connected_data = filtered_connected_wrangled_data,
        filtered_connected_treatment_list = filtered_connected_treatment_list,
        filtered_reference_treatment = filtered_reference_treatment
      )
    )
  })
}