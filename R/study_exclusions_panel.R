
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
    
    main_subnetworks <- eventReactive(
      data(),
      {
        if (is.null(reference_treatment()) || reference_treatment() == "" || !(reference_treatment() %in% treatment_df()$Label)) {
          return(NULL)
        }
        return(IdentifySubNetworks(data(), treatment_df(), reference_treatment()))
      }
    )

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

    main_subnetwork_exclusions <- reactive({
      studies <- isolate(all_studies())
      return(studies[!studies %in% main_connected_data()$Study])
    })

    observe({
      selections(main_subnetwork_exclusions())
    })
    
    recent_main_subnetwork_exclusions <- reactiveVal()

    # Inform the user that the uploaded data is disconnected
    observe({
      # This prevents Duplicate notifications due to chatty reactives
      if (!is.null(recent_main_subnetwork_exclusions()) && identical(recent_main_subnetwork_exclusions(), main_subnetwork_exclusions())) {
        return()
      }
      
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
    
    selections <- reactiveVal(c())
    filtered_selections <- reactiveVal(c())
    exclusions <- reactiveVal(c())
    
    observe({
      selections(c())
      filtered_selections(c())
      exclusions(c())
    }) |> bindEvent(data())
    
    # This debouncing prevents an infinite loop of updating the selections
    ui_exclusions <- debounce(
      millis = 100,
      r = reactive({
        input$exclusionbox
      })
    )
    
    # Update "selections" reactive
    observe({
      selections(ui_exclusions())
    })
    
    #############################
    # Find connected subnetwork #
    #############################
    # vvvvvvvvvvvvvvvvvvvvvvvvvvv
    
    selection_data <- reactive({
      data <- isolate(data())
      return(data[!data$Study %in% selections(), ])
    })
    
    subnetworks <- eventReactive(
      selection_data(),
      {
        if (is.null(reference_treatment()) || reference_treatment() == "" || !(reference_treatment() %in% treatment_df()$Label)) {
          return(NULL)
        }
        return(IdentifySubNetworks(selection_data(), treatment_df(), reference_treatment()))
      }
    )
    
    connected_data <- eventReactive(
      subnetworks(),
      {
        indices <- 1:length(selection_data()$Study)
        
        subnetworks <- subnetworks()
        primary_network <- subnetworks$subnet_1
        
        connected_indices <- indices[selection_data()$Study %in% primary_network$studies]
        return(selection_data()[connected_indices, ])
      }
    )
    
    subnetwork_exclusions <- reactive({
      studies <- isolate(all_studies())
      return(studies[!studies %in% connected_data()$Study])
    })
    
    # ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    
    # Update "filtered_selections" reactive
    observe({
      filtered_selections(subnetwork_exclusions())
    })
    
    # If "filtered_selections" != "selections"
    observe({
      if (!identical(filtered_selections(), selections())) {
        selections(filtered_selections())
      }
    }) |> bindEvent(filtered_selections())
    
    # If "filtered_selections" == "selections"
    # If "filtered_selections" != "exclusions"
    observe({
      if (identical(filtered_selections(), selections()) && !identical(filtered_selections(), exclusions())) {
        exclusions(filtered_selections())
      }
    }) |> bindEvent(filtered_selections())
    
    observe({
      # Select check boxes for disconnected studies
      disconnected_indices <- which(all_studies() %in% exclusions())
      selected = all_studies()[disconnected_indices]
      shiny::updateCheckboxGroupInput(inputId = "exclusionbox", selected = selected)
    }) |> bindEvent(exclusions())
    
    app_data <- reactive({
      return(data()[!data()$Study %in% exclusions(), ])
    })
    
    # Disable check boxes for disconnected studies
    observe({
      filtered_treatments <- FindAllTreatments(app_data())
      
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
    }) |> bindEvent(app_data())
    
    # Reactives to return to the rest of the app
    
    delayed_app_data <- shiny::debounce(
      millis = 1500,
      r = reactive({
        app_data()
      })
    )
    
    filtered_connected_dewrangled_data <- reactive({
      ReinstateTreatmentIds(delayed_app_data(), treatment_df())
    })
    
    filtered_connected_treatment_list <- reactive({
      treatments <- FindAllTreatments(filtered_connected_dewrangled_data())
      return(CreateTreatmentIds(treatments, reference_treatment = reference_treatment()))
    })
    
    filtered_reference_treatment <- reactive({
      filtered_connected_treatment_list()$Label[1]
    })
    
    filtered_connected_wrangled_data <- reactive({
      ReplaceTreatmentIds(filtered_connected_dewrangled_data(), filtered_connected_treatment_list())
    })
    
    return(
      list(
        initial_data = main_connected_data,
        sensitivity_data = filtered_connected_wrangled_data,
        sensitivity_treatment_list = filtered_connected_treatment_list
      )
    )
  })
}