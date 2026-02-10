setup_exclude_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    radioButtons(ns("effects"), label = "Model:",
                  choices = c("Random effect (RE)" = "random", "Fixed effect (FE)" = "fixed"), inline = TRUE),
    uiOutput(ns("exclusions_out"))
  )
}

setup_exclude_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    init("effects")

    # can't get updatePickerInput to reblank on reset
    output$exclusions_out <- renderUI({
      watch("setup_configure")
      watch("setup_reset")
      if (is.null(common$configured_data)){
        shinyWidgets::pickerInput(session$ns("exclusions"), label = "Studies to exclude:", multiple = TRUE,
                                  choices = c(),
                                  options = shinyWidgets::pickerOptions(liveSearch = TRUE))
      } else {
        wrangled <- unique(common$configured_data$wrangled_data$Study)
        connected <- unique(common$configured_data$connected_data$Study)
        shinyWidgets::pickerInput(session$ns("exclusions"), label = "Studies to exclude:", multiple = TRUE,
                                  choices = wrangled,
                                  choicesOpt = list(disabled = !c(wrangled %in% connected)),
                                  options = shinyWidgets::pickerOptions(liveSearch = TRUE))
      }
    })

    common$tasks$setup_exclude_all <- ExtendedTask$new(
      function(...) mirai::mirai(run(...), run = frequentist, .args = environment())
    )

    # needed to cancel in progress
    excluding <- NULL
    common$tasks$setup_exclude_sub <- ExtendedTask$new(
      function(...) excluding <<- mirai::mirai(run(...), run = setup_exclude, .args = environment())
    )

    # update freq_all if effects selection changes
    observeEvent(input$effects, {
      req(common$configured_data)

      common$tasks$setup_exclude_all$invoke(common$configured_data$non_covariate_data,
                                            common$configured_data$outcome,
                                            common$configured_data$treatments,
                                            common$configured_data$outcome_measure,
                                            common$effects,
                                            common$configured_data$treatments$Label[common$configured_data$treatments$Number == 1])

      result_all$resume()
    })

    # listen to all the triggers but only fire once they're static for 1200ms
    exclusion_triggers <- reactive({
      # prevent it triggering on reload
      req((!identical(input$exclusions, common$excluded_studies) || watch("setup_configure") > 0 || watch("effects") > 0))
      list(input$exclusions,
           input$effects,
           watch("setup_configure"))
    }) |> debounce(1200)

    observeEvent(exclusion_triggers(), {
      req(common$configured_data)

      # cancel if already updating
      if (common$tasks$setup_exclude_sub$status() == "running"){
        mirai::stop_mirai(excluding)
      }

      common$tasks$setup_exclude_sub$invoke(common$configured_data,
                                            input$exclusions,
                                            async = TRUE)

      if (length(common$freq_sub) == 0){
        common$logger |> writeLog(type = "starting", "Running initial frequentist analysis")
      } else {
        common$logger |> writeLog(type = "starting", "Updating sensitivity analysis")
      }

      # storing this here so they are always in sync
      common$excluded_studies <- input$exclusions

      # METADATA ####
      common$meta$setup_exclude$used <- TRUE
      common$meta$setup_exclude$exclusions <- input$exclusions
      common$meta$setup_exclude$effects <- input$effects

      result_sub$resume()

    })

    result_all <- observe({
      common$configured_data$freq <- common$tasks$setup_exclude_all$result()
      result_all$suspend()
    })

    result_sub <- observe({
      # prevent loading when the task is cancelled
      if (common$tasks$setup_exclude_sub$status() == "success"){

        initial <- ifelse(is.null(common$subsetted_data), TRUE, FALSE)

        result_sub$suspend()
        result <- common$tasks$setup_exclude_sub$result()
        if (inherits(result, "configured_data")){
          common$subsetted_data <- result

          if (common$configured_data$reference_treatment != common$subsetted_data$reference_treatment){
            common$logger |> writeLog(type = "info",
                                      glue("The reference treatment for the sensitivity analysis
                                              has been changed to {common$subsetted_data$reference_treatment}
                                              because the {common$configured_data$reference_treatment} treatment
                                              has been removed from the network of sensitivity analysis."))
          }

          # if opened in setup_configure
          close_loading_modal()
          if (initial){
            common$logger |> writeLog(type = "complete", "Initial frequentist analysis is complete")
          } else {
            common$logger |> writeLog(type = "complete", "Sensitivity analysis has been updated")
          }

          # required for testing to wait until the debounce has triggered
          shinyjs::runjs("Shiny.setInputValue('setup_exclude-complete', 'complete');")
          trigger("setup_exclude")

        } else {
          common$logger |> writeLog(type = "error", result)
        }

      }
    })

    # stop triggering at app load, but do so once data is loaded
    observe({
      common$effects <- input$effects
      # prevent it triggering on reload
      req(!identical(input$effects, common$effects))
      trigger("effects")
    })

  return(list(
    save = function() {list(
      ### Manual save start
      ### Manual save end
      choices = unique(common$configured_data$wrangled_data$Study),
      exclusions = input$exclusions,
      disabled = !c(unique(common$configured_data$wrangled_data$Study) %in% unique(common$configured_data$connected_data$Study)),
      effects = input$effects)
    },
    load = function(state) {
      ### Manual load start
      ### Manual load end
      if (is.null(state$exclusions)){
        shinyWidgets::updatePickerInput(session, "exclusions",
                                        choices = state$choices,
                                        choicesOpt = list(disabled = state$disabled))
      } else {
        shinyWidgets::updatePickerInput(session, "exclusions",
                                        selected = state$exclusions,
                                        choices = state$choices,
                                        choicesOpt = list(disabled = state$disabled))
      }
      updateRadioButtons(session, "effects", selected = state$effects)
    }
  ))
})
}

setup_exclude_module_rmd <- function(common){ list(
  setup_exclude_knit = !is.null(common$meta$setup_exclude$used),
  setup_exclude_exclusions = common$meta$setup_exclude$exclusions)
}

