summary_exclude_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    bslib::accordion(
      bslib::accordion_panel("Select model and exclude studies",
         radioButtons(ns("model"), label = "Model:",
                      choices = c("Random effect (RE)" = "random", "Fixed effect (FE)" = "fixed")),
         checkboxGroupInput(ns("exclusions"), label = "Studies to exclude:", choices = c())
      )
    )
  )
}

summary_exclude_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    init("model")

    observe({
      watch("setup_load")
      req(common$data)
      # selected = is required to restore selections on reload
      updateCheckboxGroupInput(session, "exclusions", choices = unique(common$data$Study), selected = common$excluded_studies)
    })

    # Update which studies can be selected from the sensitivity analysis by taking the initial data subnetwork
    observe({
      watch("setup_define")
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

    # update freq_all if model selection changes
    observeEvent(input$model, {
      req(common$non_covariate_data_all)
      common$freq_all <- frequentist(common$non_covariate_data_all,
                                     common$outcome,
                                     common$treatment_df,
                                     common$outcome_measure,
                                     common$model_type,
                                     common$treatment_df$Label[common$treatment_df$Number == 1])
    })

    # listen to all the triggers but only fire once they're static for 1200ms
    exclusion_triggers <- reactive({
      list(input$exclusions,
           input$model,
           watch("setup_define"))
    }) %>% debounce(1200)

    observeEvent(exclusion_triggers(), {
      req(common$bugsnet_all)

      # WARNING ####
      # Something if a whole treatment becomes excluded (SS)?

      # FUNCTION CALL ####
      result <- summary_exclude(common$main_connected_data,
                               common$treatment_df,
                               common$reference_treatment_all,
                               common$outcome,
                               common$outcome_measure,
                               input$model,
                               input$exclusions,
                               common$logger)

      # LOAD INTO COMMON ####
      common$excluded_studies <- input$exclusions
      common$bugsnet_sub <- result$bugsnet_sub
      common$freq_sub <- result$freq_sub

      # METADATA ####
      common$meta$summary_exclude$used <- TRUE
      common$meta$summary_exclude$exclusions <- input$exclusions
      common$meta$summary_exclude$model <- input$model

      # TRIGGER
      trigger("summary_exclude")
    })

    observeEvent(input$model, {
      common$model_type <- input$model
      trigger("model")
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

