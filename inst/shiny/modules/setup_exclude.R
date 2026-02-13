setup_exclude_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    radioButtons(ns("effects"), label = "Model:",
                  choices = c("Random effect (RE)" = "random", "Fixed effect (FE)" = "fixed"), inline = TRUE)
  )
}

setup_exclude_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    init("effects")
    init("freq_all")

    common$tasks$setup_exclude_all <- ExtendedTask$new(
      function(...) mirai::mirai(run(...), run = frequentist, .args = environment())
    )

    # needed to cancel in progress
    excluding <- NULL
    common$tasks$setup_exclude_sub <- ExtendedTask$new(
      function(...) excluding <<- mirai::mirai(run(...), run = setup_exclude, .args = environment())
    )

    # update freq_all if effects selection changes or analysis has been reconfigured
    observeEvent(list(input$effects, watch("setup_configure")), {
      req(common$configured_data)
      common$tasks$setup_exclude_all$invoke(common$configured_data$non_covariate_data,
                                            common$configured_data$outcome,
                                            common$configured_data$treatments,
                                            common$configured_data$outcome_measure,
                                            input$effects,
                                            common$configured_data$treatments$Label[common$configured_data$treatments$Number == 1])

      result_all$resume()
    })

    # listen to all the triggers but only fire once they're static for 1200ms
    exclusion_triggers <- reactive({
      # prevent it triggering on reload
      req((!identical(input$exclusions, common$excluded_studies) || watch("setup_configure") > 0 || watch("effects") > 1))
      list(input$exclusions,
           input$effects,
           watch("setup_configure"))
    }) |> debounce(1200)

    observeEvent(exclusion_triggers(), {
      req(common$configured_data)

      # log removals / additions of studies
      new_exclusions <- input$exclusions[!(input$exclusions %in% common$excluded_studies)]
      if (length(new_exclusions) > 0){
        single_plural <- ifelse(length(new_exclusions) == 1, "has", "have")
        new_exclusions <- paste(new_exclusions, collapse = ", ")
        common$logger |> writeLog(type = "complete", glue("{new_exclusions} {single_plural} been excluded from the sensitivity analysis"))
      }

      new_additions <- common$excluded_studies[!(common$excluded_studies %in% input$exclusions)]
      if (length(new_additions) > 0){
        single_plural <- ifelse(length(new_additions) == 1, "has", "have")
        new_additions <- paste(new_additions, collapse = ", ")
        common$logger |> writeLog(type = "complete", glue("{new_additions} {single_plural} been added to the sensitivity analysis"))
      }

      # storing this here so they are always in sync
      common$excluded_studies <- input$exclusions

      # cancel if already updating
      if (common$tasks$setup_exclude_sub$status() == "running"){
        mirai::stop_mirai(excluding)
      }

      common$tasks$setup_exclude_sub$invoke(common$configured_data,
                                            input$exclusions,
                                            async = TRUE)

      if (is.null(common$subsetted_data)){
        common$logger |> writeLog(type = "starting", "Running initial frequentist analysis")
      } else {
        common$logger |> writeLog(type = "starting", "Updating sensitivity analysis")
      }

      # METADATA ####
      common$meta$setup_exclude$used <- TRUE
      common$meta$setup_exclude$exclusions <- input$exclusions
      common$meta$setup_exclude$effects <- input$effects

      result_sub$resume()

    })

    result_all <- observe({
      common$configured_data$freq <- common$tasks$setup_exclude_all$result()
      trigger("freq_all")
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
            # required for testing to wait until the debounce has triggered
            shinyjs::runjs("Shiny.setInputValue('setup_exclude-complete', 'initial');")
          } else {
            common$logger |> writeLog(type = "complete", "Sensitivity analysis has been updated")
            shinyjs::runjs("Shiny.setInputValue('setup_exclude-complete', 'complete');")
          }

          trigger("setup_exclude")

        } else {
          common$logger |> writeLog(type = "error", result)
        }

      }
    })

    # stop triggering at app load, but do so once data is loaded
    observe({
      # prevent it triggering on reload
      req(!identical(input$effects, common$effects))
      common$effects <- input$effects

      if(!is.null(common$configured_data)){
        common$configured_data$effects <- input$effects
      }
      if(!is.null(common$subsetted_data)){
        common$subsetted_data$effects <- input$effects
      }
      trigger("effects")
    })

    output$plot <- renderUI({
      watch("setup_reset")
      watch("setup_configure")
      req(common$configured_data)
      tagList(
        svg_container(
          setup_exclude_plot(common$configured_data, isolate(input$exclusions), hover = TRUE),
          style = "max-width: 1200px; margin: 0 auto;"
        ),
        tags$script(HTML(sprintf('
          $(document).ready(function() {
            var selectedStudies = [];

            // Function to initialize selectedStudies from existing visual state
            function reload() {
              // Check if the element exists
              if ($("#summary_exclude_interface").length === 0) {
                // If not, wait and try again
                setTimeout(reload, 100);
                return;
              }

              // Find all groups with rects that have opacity 0.5
              $("#summary_exclude_interface g[id^=\'setup_exclude-line\']").each(function() {
                var rect = $(this).find("rect");
                if (rect.css("opacity") == "0.5") {
                  var studyName = $(this).attr("data-study-name");
                  if (studyName && selectedStudies.indexOf(studyName) === -1) {
                    selectedStudies.push(studyName);
                  }
                }
              });

            }

            // Start trying to initialize after a delay
            setTimeout(reload, 200);

            $("#summary_exclude_interface g[id^=\'setup_exclude-line\']").on("click", function() {
              var clickedStudy = $(this).attr("data-study-name");

              // Toggle study selection
              var index = selectedStudies.indexOf(clickedStudy);
              if (index > -1) {
                selectedStudies.splice(index, 1);
              } else {
                selectedStudies.push(clickedStudy);
              }

              // Update opacity for all study lines
              $("#summary_exclude_interface g[data-study-name=\'" + clickedStudy + "\']").each(function() {
                  var rect = $(this).find("rect");
                  if (selectedStudies.includes(clickedStudy)) {
                      rect.css("opacity", "0.5");
                  } else {
                      rect.css("opacity", "0.0");
                  }
              });

              // Send selected studies to Shiny input
              Shiny.setInputValue("%s", selectedStudies);
            });
          });
        ', session$ns("exclusions"))))
      )
    })

  return(list(
    save = function() {list(
      ### Manual save start
      ### Manual save end
      exclusions = input$exclusions,
      effects = input$effects)
    },
    load = function(state) {
      ### Manual load start
      # format to JS array
      exclusions <- paste0("['", paste(state$exclusions, collapse = "','"), "']")
      shinyjs::runjs(glue::glue("Shiny.setInputValue('setup_exclude-exclusions', {exclusions});"))
      ### Manual load end
      updateRadioButtons(session, "effects", selected = state$effects)
    }
  ))
})
}

setup_exclude_module_results <- function(id){
  ns <- NS(id)
  tagList(
    accordion(
      id = ns("collapse"),
      open = FALSE,
      accordion_panel(
        title = "Exclude studies (Click to open / hide this panel)",
        "Click on a study arm in the plot to exclude or replace studies in the sensitivity analysis",
        uiOutput(ns("plot"))
      )
    )
  )
}

setup_exclude_module_rmd <- function(common){ list(
  setup_exclude_knit = !is.null(common$meta$setup_exclude$used),
  setup_exclude_exclusions = common$meta$setup_exclude$exclusions)
}

