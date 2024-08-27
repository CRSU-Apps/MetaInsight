
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
#' - "initial_data" contains the data frame of the connected subnetwork of the uploaded data
#' - "sensitivity_data" contains the data frame of the connected subnetwork for the sensitivity analysis
#' - "sensitivity_treatment_list" contains the treatment names ("Label") and IDs ("Number") for the sensitivity analysis data
study_exclusions_panel_server <- function(id, data, treatment_df, reference_treatment) {
  moduleServer(id, function(input, output, session) {
    
    all_studies <- reactive({
      unique(data()$Study)
    })
    
    # Create check boxes for studies in data
    observe({
      shiny::updateCheckboxGroupInput(
        inputId = "exclusionbox",
        choices = all_studies()
      )
    })
    
    reference_treatment <- reactive({
      treatment_df()$Label[treatment_df()$Number == 1]
    })
    
    #################
    # Main analysis #
    #################

    # See `developer_resources/study_exclusions_panel_reactivity.png`
    # for a description of how the disconnected network detection works.
    
    # Subnetworks from initial data
    main_subnetworks <- eventReactive(
      data(),
      {
        if (is.null(reference_treatment()) || reference_treatment() == "" || !(reference_treatment() %in% treatment_df()$Label)) {
          return(NULL)
        }
        return(IdentifySubNetworks(data(), treatment_df(), reference_treatment()))
      }
    )

    ## Initial data filtered to only the studies in the subnetwork containing the reference treatment
    main_connected_data <- eventReactive(
      main_subnetworks(),
      {
        indices <- 1:length(data()$Study)

        subnetworks <- main_subnetworks()
        primary_network <- subnetworks$subnet_1

        connected_indices <- indices[data()$Study %in% primary_network$studies]
        return(data()[connected_indices, ])
      }
    )

    # Names of studies not included in the main subnetwork
    main_subnetwork_exclusions <- reactive({
      studies <- isolate(all_studies())
      return(studies[!studies %in% main_connected_data()$Study])
    })
    
    # Update the checkboxes to tick the excluded studies
    observe({
      # Select check boxes for disconnected studies
      disconnected_indices <- which(all_studies() %in% main_subnetwork_exclusions())
      selected = all_studies()[disconnected_indices]
      shiny::updateCheckboxGroupInput(inputId = "exclusionbox", selected = selected)
    }) |> bindEvent(main_subnetwork_exclusions())

    # Update which studies can be selected from the sensitivity analysis by taking the initial data subnetwork
    observe({
      filtered_treatments <- FindAllTreatments(main_connected_data())
      
      lapply(
        all_studies(),
        function(study) {
          index <- match(study, all_studies())
          subElement <- glue::glue("#{session$ns('exclusionbox')} .checkbox:nth-child({index}) label")
          
          study_treatments <- FindAllTreatments(data()[data()$Study == study, ])
          
          if (any(study_treatments %in% filtered_treatments)) {
            # The 0ms delay is required to disable studies disconnected within the initial uploaded data
            shinyjs::delay(0, shinyjs::enable(selector = subElement))
          } else {
            # The 0ms delay is required to disable studies disconnected within the initial uploaded data
            shinyjs::delay(0, shinyjs::disable(selector = subElement))
          }
        }
      )
    }) |> bindEvent(main_connected_data())
    
    # Exclusions from the initial data, last time the data was checked
    recent_main_subnetwork_exclusions <- reactiveVal()

    # Inform the user that the uploaded data is disconnected
    observe({
      # This prevents Duplicate notifications due to chatty reactives
      if (!is.null(recent_main_subnetwork_exclusions()) && identical(recent_main_subnetwork_exclusions(), main_subnetwork_exclusions())) {
        return()
      }
      
      # Update the exclusions
      recent_main_subnetwork_exclusions(main_subnetwork_exclusions())
      
      if (length(main_subnetwork_exclusions()) > 0) {
        shiny::showModal(
          modalDialog(
            title = "Disconnected Network",
            p(glue::glue("The uploaded data comprises a disconnected network. Only the subnetwork containing the reference treatment ({reference_treatment()}) will be displayed"))
          )
        )
      }
    }) |> bindEvent(main_subnetwork_exclusions())
    
    ########################
    # Sensitivity analysis #
    ########################
    
    # This debouncing prevents an infinite loop of updating the selections
    ui_exclusions <- debounce(
      millis = 100,
      r = reactive({
        input$exclusionbox
      })
    )
    
    # Sensitivity analysis data with studies excluded using "selections"
    selection_data <- reactive({
      data <- isolate(data())
      return(data[!data$Study %in% ui_exclusions(), ])
    })
    
    debounced_sensitivity_data <- debounce(
      millis = 1200,
      r = reactive({
        selection_data()
      })
    )
    
    sensitivity_dewrangled_data <- reactive({
      return(ReinstateTreatmentIds(debounced_sensitivity_data(), treatment_df()))
    })
    
    sensitivity_treatment_list <- reactive({
      treatments <- FindAllTreatments(sensitivity_dewrangled_data())
      return(CreateTreatmentIds(treatments, reference_treatment = reference_treatment()))
    })
    
    return(
      list(
        initial_data = main_connected_data,
        sensitivity_data = reactive({
          ReplaceTreatmentIds(sensitivity_dewrangled_data(), sensitivity_treatment_list())
        }),
        sensitivity_treatment_list = sensitivity_treatment_list
      )
    )
  })
}