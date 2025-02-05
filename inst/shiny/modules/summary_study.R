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


  observeEvent(list(input$run, gargoyle::watch("summary_exclude")), {
    # WARNING ####

    # FUNCTION CALL ####

    # LOAD INTO COMMON ####

    # METADATA ####
      common$meta$summary_study$used <- TRUE
      common$meta$summary_study$ForestContent <- as.numeric(input$ForestContent)
      common$meta$summary_study$ForestTitle <- as.numeric(input$ForestTitle)
      common$meta$summary_study$ForestHeader <- as.numeric(input$ForestHeader)
      common$meta$summary_study$format_freq0 <- input$format_freq0

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
    save = function() {list(
      ### Manual save start
      ### Manual save end
      ForestContent = input$ForestContent,
      ForestTitle = input$ForestTitle,
      ForestHeader = input$ForestHeader,
      format_freq0 = input$format_freq0)
    },
    load = function(state) {
      ### Manual load start
      ### Manual load end
      updateNumericInput(session, "ForestContent", value = state$ForestContent)
      updateNumericInput(session, "ForestTitle", value = state$ForestTitle)
      updateNumericInput(session, "ForestHeader", value = state$ForestHeader)
      updateRadioButtons(session, "format_freq0", selected = state$format_freq0)
    }
  ))
})
}

summary_study_module_result <- function(id) {
  ns <- NS(id)

  # Result UI
  # the size of this should adjust depending on the number of studies
  plotOutput(ns("forestPlot"), height = "1000px", width = "800px")
}

summary_study_module_rmd <- function(common){ list(
  summary_study_knit = !is.null(common$meta$summary_study$used),
  summary_study_ForestContent = common$meta$summary_study$ForestContent,
  summary_study_ForestTitle = common$meta$summary_study$ForestTitle,
  summary_study_ForestHeader = common$meta$summary_study$ForestHeader,
  summary_study_format_freq0 = common$meta$summary_study$format_freq0)
}

