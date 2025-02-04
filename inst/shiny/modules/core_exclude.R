core_exclude_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    radioButtons(
      inputId = ns("model"),
      label = "Model:",
      choices = c(
        "Random effect (RE)" = "random",
        "Fixed effect (FE)" = "fixed"
      )
    ),
    checkboxGroupInput(
      inputId = ns("exclusions"),
      label = "Studies to exclude:",
      choices = c()
    ),
  )
}

core_exclude_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    gargoyle::init("exclude")
    gargoyle::init("model")

    observe({
      gargoyle::watch("load_load")
      req(common$data)
      updateCheckboxGroupInput(session, "exclusions", choices = unique(common$data$Study))
    })


    # Update which studies can be selected from the sensitivity analysis by taking the initial data subnetwork
    observe({
      gargoyle::watch("load_define")
      req(common$disconnected_indices)

      all_studies <- unique(common$data$Study)

      selected <- all_studies[common$disconnected_indices]
      updateCheckboxGroupInput(inputId = "exclusions", selected = selected)

      filtered_treatments <- FindAllTreatments(common$main_connected_data)
      lapply(
        all_studies,
        function(study) {
          index <- match(study, all_studies)
          sub_element <- glue::glue("#{session$ns('exclusions')} .checkbox:nth-child({index}) label")

          study_treatments <- FindAllTreatments(common$data[common$data$Study == study, ])

          if (any(study_treatments %in% filtered_treatments)) {
            # The 0ms delay is required to disable studies disconnected within the initial uploaded data
            shinyjs::delay(0, shinyjs::enable(selector = sub_element))
          } else {
            # The 0ms delay is required to disable studies disconnected within the initial uploaded data
            shinyjs::delay(0, shinyjs::disable(selector = sub_element))
          }
        }
      )
    })

    observeEvent(list(input$exclusions, input$model, gargoyle::watch("load_define")), {
      req(common$bugsnetdt)
      common$excluded_studies <- input$exclusions
      result <- core_exclude(common$main_connected_data,
                             common$treatment_df,
                             common$reference_treatment,
                             common$metaoutcome,
                             common$outcome_measure,
                             input$model,
                             input$exclusions,
                             common$logger)

      common$bugsnetdt_sub <- result$bugsnetdt_sub
      common$freq_sub <- result$freq_sub

      gargoyle::trigger("exclude")
    })

    observeEvent(input$model, {
      common$model_type <- input$model
      gargoyle::trigger("model")
    })

  })
}
