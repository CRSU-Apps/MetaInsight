
#' Module UI for the nodesplit model panel
#' 
#' @param id ID of the module
#' @param item_name Name of this nodesplit item.
#' @return Div for the panel
nodesplit_panel_ui <- function(id, item_name) {
  ns <- NS(id)
  div(
    invalid_model_panel_ui(id = ns("model_invalid")),
    actionButton(inputId = ns("node"), label = glue::glue("Click here to run the nodesplitting analysis for {item_name}")),
    uiOutput(outputId = ns("node_plot_placeholder")),
    radioButtons(
      inputId = ns('download_format'),
      label = 'Document format',
      choices = c('PDF', 'PNG'),
      inline = TRUE
    ),
    downloadButton(outputId = ns('downloadnode'))
  )
}


#' Module server for the nodesplit model panel.
#' 
#' @param id ID of the module
#' @param data Reactive containing data to analyse
#' @param treatment_df Reactive containing data frame containing treatment IDs (Number) and names (Label)
#' @param metaoutcome Reactive containing meta analysis outcome: "Continuous" or "Binary"
#' @param outcome_measure Reactive containing meta analysis outcome measure: "MD", "SMD", "OR, "RR", or "RD"
#' @param model_effects Reactive containing model effects: either "random" or "fixed"
nodesplit_panel_server <- function(
    id,
    data,
    treatment_df,
    metaoutcome,
    outcome_measure,
    model_effects
    ) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    model_nodesplit <- eventReactive(
      input$node,
      {
        nodesplit_model <- tryCatch(
          expr = {
            nodesplit(
              data(),
              treatment_df(),
              metaoutcome(),
              outcome_measure(),
              model_effects()
            )
          },
          error = function(exptn) {
            return(NULL)
          }
        )
        return(nodesplit_model)
      }
    )
    
    model_valid = reactiveVal(NULL)
    parameter_matcher <- ParameterMatcher$new()
    
    observe({
      # Only assess the validity once the model has been run the first time
      if (is.null(model_valid())) {
        return()
      }
      
      model_valid(
        parameter_matcher$Matches(
          data = data(),
          treatment_df = treatment_df(),
          metaoutcome = metaoutcome(),
          outcome_measure = outcome_measure(),
          model_effects = model_effects()
        )
      )
    })
    
    observe({
      parameter_matcher$SetParameters(
        data = data(),
        treatment_df = treatment_df(),
        metaoutcome = metaoutcome(),
        outcome_measure = outcome_measure(),
        model_effects = model_effects()
      )
      model_valid(!is.null(model_nodesplit()))
    }) |> bindEvent(model_nodesplit())
    
    observe({
      if (is.null(model_valid()) || !model_valid()) {
        shinyjs::disable(id = "downloadnode")
      } else {
        shinyjs::enable(id = "downloadnode")
      }
    })
    
    invalid_model_panel_server(id = "model_invalid", model_valid = model_valid)
    
    # number of comparisons
    ncomp <- reactive({
      if (is.null(model_nodesplit())) {
        return(0)
      }
      return(as.numeric(length(model_nodesplit()) - 1))
    })
  
    
    output$node_plot_placeholder <- renderUI({
      if (is.null(model_nodesplit())) {
        return(
          div(
            p("Nodesplit model cannot be run, likely because there are no closed loops in the network"),
            style = "color: red;"
          )
        )
      } else {
        plotOutput(
          outputId = ns("node_plot"),
          height = NodePixels(ncomp())
        )
      }
    })
    
    output$node_plot<- renderPlot({
      if (is.null(model_valid()) || !model_valid()) {
        return()
      }
      req(input$node)
      plot(summary(model_nodesplit()), digits = 3)
    })

    output$downloadnode <- downloadHandler(
      filename = function() {
        paste0("Nodesplit.",
               input$download_format)
      },
      content = function(file) {
        if (input$download_format == "PDF") {
          pdf(file = file, width = 9, height = NodeDownloadHeight(ncomp(), 'in'))
        } else if (input$download_format == "PNG") {
          png(file = file, width = 850, height = NodeDownloadHeight(ncomp(), 'px'))
        }
        plot(summary(model_nodesplit()), digits = 3)
        dev.off()
      },
      contentType = "image/pdf"
    )
  })
}