summary_exclude_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    radioButtons(ns("model"), label = "Model:",
                  choices = c("Random effect (RE)" = "random", "Fixed effect (FE)" = "fixed"), inline = TRUE),
    shinyWidgets::pickerInput(ns("exclusions"), label = "Studies to exclude:", choices = c(), multiple = TRUE,
                              options = shinyWidgets::pickerOptions(liveSearch = TRUE))
  )
}

summary_exclude_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    init("model")

    observe({
      watch("setup_define")
      req(common$data)
      # selected = is required to restore selections on reload
      shinyWidgets::updatePickerInput(session, "exclusions",
                                      choices = unique(common$data$Study),
                                      selected = common$excluded_studies,
                                      choicesOpt = list(disabled = !c(unique(common$data$Study) %in% unique(common$main_connected_data$Study))))

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

      withProgress(message = "Updating selected studies", {
        # FUNCTION CALL ####
        result <- summary_exclude(common$main_connected_data,
                                  common$treatment_df,
                                  common$reference_treatment_all,
                                  common$outcome,
                                  common$outcome_measure,
                                  input$model,
                                  input$exclusions,
                                  common$logger)
      })

      # LOAD INTO COMMON ####
      common$excluded_studies <- input$exclusions
      common$bugsnet_sub <- result$bugsnet_sub
      common$freq_sub <- result$freq_sub
      common$reference_treatment_sub <- result$reference_treatment_sub

      if (common$reference_treatment_sub != common$reference_treatment_all){
        common$logger %>% writeLog(type = "info",
                                   glue::glue("The reference treatment for the sensitivity analysis
                                              has been changed to {common$reference_treatment_sub}
                                              because the {common$reference_treatment_all} treatment
                                              has been removed from the network of sensitivity analysis."))
      }

      # METADATA ####
      common$meta$summary_exclude$used <- TRUE
      common$meta$summary_exclude$exclusions <- input$exclusions
      common$meta$summary_exclude$model <- input$model

      # TRIGGER
      # required for testing to wait until the debounce has triggered
      shinyjs::runjs("Shiny.setInputValue('summary_exclude-complete', 'complete');")
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
  summary_exclude_model = common$meta$summary_exclude$model,
  summary_exclude_reference_treatment = common$reference_treatment_sub)
}

