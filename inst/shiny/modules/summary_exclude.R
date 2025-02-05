summary_exclude_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    radioButtons(ns("model"), label = "Model:",
                 choices = c("Random effect (RE)" = "random", "Fixed effect (FE)" = "fixed")),
    checkboxGroupInput(ns("exclusions"), label = "Studies to exclude:", choices = c())
  )
}

summary_exclude_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    gargoyle::init("model")

    observe({
      gargoyle::watch("setup_load")
      req(common$data)
      updateCheckboxGroupInput(session, "exclusions", choices = unique(common$data$Study))
    })

    # Update which studies can be selected from the sensitivity analysis by taking the initial data subnetwork
    observe({
      gargoyle::watch("setup_define")
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

    observeEvent(list(debounce(input$exclusions, 1200),
                      input$model,
                      gargoyle::watch("setup_define")), {
      # WARNING ####
      # Something if a whole treatment becomes excluded?
      req(common$bugsnetdt)

      # FUNCTION CALL ####
      result <- summary_exclude(common$main_connected_data,
                             common$treatment_df,
                             common$reference_treatment,
                             common$metaoutcome,
                             common$outcome_measure,
                             input$model,
                             input$exclusions,
                             common$logger)

      # LOAD INTO COMMON ####
      common$excluded_studies <- input$exclusions
      common$bugsnetdt_sub <- result$bugsnetdt_sub
      common$freq_sub <- result$freq_sub

      # METADATA ####
      common$meta$summary_exclude$used <- TRUE
      common$meta$summary_exclude$exclusions <- input$exclusions
      common$meta$summary_exclude$model <- input$model
      # Populate using metadata()

      # TRIGGER
      gargoyle::trigger("summary_exclude")
    })

    observeEvent(input$model, {
      common$model_type <- input$model
      gargoyle::trigger("model")
    })

  return(list(
    save = function() {list(
      ### Manual save start
      ### Manual save end
      exclusions = input$exclusions, 
      model = input$model)
    },
    load = function(state) {
      ### Manual load start
      ### Manual load end
      updateCheckboxGroupInput(session, "exclusions", selected = state$exclusions) 
      updateRadioButtons(session, "model", selected = state$model)
    }
  ))
})
}

summary_exclude_module_rmd <- function(common){ list(
  summary_exclude_knit = !is.null(common$meta$summary_exclude$used),
  summary_exclude_exclusions = common$meta$summary_exclude$exclusions,
  summary_exclude_model = common$meta$summary_exclude$model)
}

