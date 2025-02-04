summary_study_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    numericInput(ns("ForestContent"), label="Download text size:", value=12),
    numericInput(ns("ForestTitle"), label = "Title text size:", value = 1, step = 0.1),
    numericInput(ns("ForestHeader"), label = "Group headers text size:", value = 1, step = 0.1),
    radioButtons(ns("format_freq0"), label = 'Document format', choices = c("PDF" = "pdf", "SVG" = "svg"), inline = TRUE),
    downloadButton(ns("downloadStudy")),
    actionButton(ns("run"), "Run module summary_study")
  )
}

summary_study_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  # it doesn't make much sense to calculate this here, but it relies on the model definition in core_exclude
  observe({
    gargoyle::watch("setup_define")
    gargoyle::watch("model")
    req(common$initial_non_covariate_data)

    common$freq_all <- frequentist(common$initial_non_covariate_data,
                        common$metaoutcome,
                        common$treatment_df,
                        common$outcome_measure,
                        common$model_type,
                        common$treatment_df$Label[common$treatment_df$Number == 1])
  })


  observeEvent(list(input$run, gargoyle::watch("exclude")), {
    # WARNING ####

    # FUNCTION CALL ####

    # LOAD INTO COMMON ####

    # METADATA ####

    # TRIGGER
    gargoyle::trigger("summary_study")
  })

  output$forestPlot <- renderPlot({
    gargoyle::watch("summary_study")
    req(common$freq_sub)
    # can't be made into a reactive as it creates a plot
    make_netStudy(common$freq_sub, common$outcome_measure, input$ForestHeader, input$ForestTitle)
  })

  output$downloadStudy <- downloadHandler(
    filename = function() {
      paste0('StudyResults.', input$format_freq0)
    },
    content = function(file) {
      if (input$format_freq0 == "pdf") {
        pdf(file = file, pointsize = input$ForestContent, width = 8, height =  make_netStudy(common$freq_sub, common$outcome_measure, input$ForestHeader, input$ForestTitle)$size)
      } else {
        svg(file = file, pointsize = input$ForestContent, width = 8, height =  make_netStudy(common$freq_sub, common$outcome_measure, input$ForestHeader, input$ForestTitle)$size)
      }
      make_netStudy(common$freq_sub, common$outcome_measure, input$ForestHeader, input$ForestTitle)
      dev.off()
      }
  )


  return(list(
    save = function() {
      # Save any values that should be saved when the current session is saved
    },
    load = function(state) {
      # Load
    }
  ))
})
}

summary_study_module_result <- function(id) {
  ns <- NS(id)

  # Result UI
  plotOutput(ns("forestPlot"), height = "1000px", width = "800px")
}

summary_study_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
}

