setup_exclude_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    radioButtons(ns("model"), label = "Model:",
                  choices = c("Random effect (RE)" = "random", "Fixed effect (FE)" = "fixed"), inline = TRUE),
    shinyWidgets::pickerInput(ns("exclusions"), label = "Studies to exclude:", choices = c(), multiple = TRUE,
                              options = shinyWidgets::pickerOptions(liveSearch = TRUE))
  )
}

setup_exclude_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    init("model")

    observe({
      watch("setup_configure")
      req(common$data)
      # selected = is required to restore selections on reload
      shinyWidgets::updatePickerInput(session, "exclusions",
                                      choices = unique(common$data$Study),
                                      selected = common$excluded_studies,
                                      choicesOpt = list(disabled = !c(unique(common$data$Study) %in% unique(common$main_connected_data$Study))))
    })

    common$tasks$setup_exclude_all <- ExtendedTask$new(
      function(...) mirai::mirai(run(...), run = frequentist, .args = environment())
    )

    # needed to cancel in progress
    excluding <- NULL
    common$tasks$setup_exclude_sub <- ExtendedTask$new(
      function(...) excluding <<- mirai::mirai(run(...), run = setup_exclude, .args = environment())
    )

    # update freq_all if model selection changes
    observeEvent(input$model, {
      req(common$non_covariate_data_all)

      common$tasks$setup_exclude_all$invoke(common$non_covariate_data_all,
                                              common$outcome,
                                              common$treatment_df,
                                              common$outcome_measure,
                                              common$model_type,
                                              common$treatment_df$Label[common$treatment_df$Number == 1])

      result_all$resume()
    })

    # listen to all the triggers but only fire once they're static for 1200ms
    exclusion_triggers <- reactive({
      # prevent it triggering on reload
      req((!identical(input$exclusions, common$excluded_studies) || watch("setup_configure") > 0))
      list(input$exclusions,
           input$model,
           watch("setup_configure"))
    }) %>% debounce(1200)

    observeEvent(exclusion_triggers(), {
      req(common$bugsnet_all)

      # cancel if already updating
      if (common$tasks$setup_exclude_sub$status() == "running"){
        mirai::stop_mirai(excluding)
      }

      common$tasks$setup_exclude_sub$invoke(common$main_connected_data,
                                              common$treatment_df,
                                              common$reference_treatment_all,
                                              common$outcome,
                                              common$outcome_measure,
                                              input$model,
                                              input$exclusions)

      if (length(input$exclusions) == 0){
        common$logger %>% writeLog(type = "starting", "Running initial sensitivity analysis")
      } else {
        common$logger %>% writeLog(type = "starting", "Updating sensitivity analysis")
      }

      # storing this here so they are always in sync
      common$excluded_studies <- input$exclusions

      # METADATA ####
      common$meta$setup_exclude$used <- TRUE
      common$meta$setup_exclude$exclusions <- input$exclusions
      common$meta$setup_exclude$model <- input$model

      result_sub$resume()

    })

    result_all <- observe({
      common$freq_all <- common$tasks$setup_exclude_all$result()
      result_all$suspend()
    })

    result_sub <- observe({
      # prevent loading when the task is cancelled
      if (common$tasks$setup_exclude_sub$status() == "success"){
        result_sub$suspend()
        result <- common$tasks$setup_exclude_sub$result()
        common$bugsnet_sub <- result$bugsnet_sub
        common$freq_sub <- result$freq_sub
        common$reference_treatment_sub <- result$reference_treatment_sub
        common$subsetted_data <- result$subsetted_data
        common$subsetted_treatment_df <- result$subsetted_treatment_df

        if (common$reference_treatment_sub != common$reference_treatment_all){
          common$logger %>% writeLog(type = "info",
                                     glue::glue("The reference treatment for the sensitivity analysis
                                              has been changed to {common$reference_treatment_sub}
                                              because the {common$reference_treatment_all} treatment
                                              has been removed from the network of sensitivity analysis."))
        }

        if (length(input$exclusions) == 0){
          common$logger %>% writeLog(type = "complete", "Initial sensitivity analysis is complete")
        } else {
          common$logger %>% writeLog(type = "complete", "Selected studies have been updated")
        }

        # required for testing to wait until the debounce has triggered
        shinyjs::runjs("Shiny.setInputValue('setup_exclude-complete', 'complete');")
        trigger("setup_exclude")
      }
    })

    observeEvent(input$model, {
      # prevent it triggering on reload
      req(!identical(input$model, common$model_type))
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

setup_exclude_module_rmd <- function(common){ list(
  setup_exclude_knit = !is.null(common$meta$setup_exclude$used),
  setup_exclude_exclusions = common$meta$setup_exclude$exclusions,
  setup_exclude_model = common$meta$setup_exclude$model,
  setup_exclude_reference_treatment = common$reference_treatment_sub)
}

