.CreateInlineBlock <- function(..., style = NULL) {
  return(
    div(
      ...,
      style = paste0("display: inline-block; ", style)
    )
  )
}

.AddTooltip <- function(..., tooltip) {
  return(
    div(
      ...,
      title = tooltip
    )
  )
}

#' Create the regression plot panel.
#'
#' @param id ID of the module.
#' @return Div containing the module UI.
regression_plot_panel_ui <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    position = "right",
    mainPanel = mainPanel(
      width = 9,
      plotOutput(outputId = ns("regression_plot"), height = "800px")
    ),
    sidebarPanel = sidebarPanel(
      width = 3,
      # Add comparators
      selectInput(
        inputId = ns("add_comparator_dropdown"),
        label = .AddTooltip(
          tags$html("Add Comparator", tags$i(class="fa-regular fa-circle-question")),
          tooltip = "All treatments are compared to the reference treatment"
        ),
        choices = c(),
        selectize = FALSE
      ),
      actionButton(inputId = ns("add_comparator_btn"), label = "Add to plot"),
      div(
        style = "float: right;",
        actionButton(inputId = ns("add_all_btn"), label = "Add all")
      ),
      br(),
      br(),
      # Remove comparators
      selectInput(inputId = ns("remove_comparator_dropdown"), label = "Remove Comparator", choices = c(), selectize = FALSE),
      actionButton(inputId = ns("remove_comparator_btn"), label = "Remove"),
      div(
        style = "float: right;",
        actionButton(inputId = ns("remove_all_btn"), label = "Remove all")
      ),
      
      h3("Plot Options"),
      # Covariate value
      .AddTooltip(
        checkboxInput(
          inputId = ns("covariate"),
          label = tags$html("Show covariate value ", tags$i(class="fa-regular fa-circle-question")),
          value = TRUE
          ),
        tooltip = "Show the covariate value as a vertical line at the current value"
      ),
      # Confidence regions
      .AddTooltip(
        checkboxInput(
          inputId = ns("confidence"),
          label = tags$html("Show confidence regions", tags$i(class="fa-regular fa-circle-question"))
        ),
        tooltip = "Show confidence regions for the added comparisons"
      ),
      sliderInput(
        inputId = ns("confidence_opacity"),
        label = "Confidence Region Opacity",
        min = 0,
        max = 0.5,
        step = 0.01,
        value = 0.2
      ),
      # Regression lines
      .AddTooltip(
        checkboxInput(
          inputId = ns("ghosts"),
          label = tags$html("Show ghost comparisons", tags$i(class="fa-regular fa-circle-question")),
          value = TRUE
        ),
        tooltip = "Show other available comparisons in light grey behind the main plot"
      ),
      .AddTooltip(
        checkboxInput(
          inputId = ns("extrapolate"),
          label = tags$html("Extrapolate regression lines", tags$i(class="fa-regular fa-circle-question")),
          value = TRUE
        ),
        tooltip = "Extrapolate the regression lines beyond the range of the original data"
      ),
      # Contributions
      .AddTooltip(
        checkboxInput(
          inputId = ns("contributions"),
          label = tags$html("Show contributions", tags$i(class="fa-regular fa-circle-question")),
          value = TRUE
        ),
        tooltip = "Show study contributions as circles, where a bigger circle represents a larger contribution"
      ),
      
      div(
        id = ns("contribution_options"),
        radioButtons(
          inputId = ns("contribution_toggle"),
          label = "Study circle sized by:",
          choiceNames = list(
            div(
              tags$html("% Contribution", tags$i(class="fa-regular fa-circle-question")),
              title = "Circles scaled by percentage contribution of each study to each treatment regression"
            ),
            div(
              tags$html("Inverse Variance", tags$i(class="fa-regular fa-circle-question")),
              title = "Circles scaled by inverse variance of each study"
            )
          ),
          choiceValues = c("percentage", "inverse variance")
        ),
        .AddTooltip(
          numericInput(
            inputId = ns("circle_multipler"),
            label = tags$html("Circle Size Multiplier", tags$i(class="fa-regular fa-circle-question")),
            min = 0,
            value = 1,
            step = 0.5
          ),
          tooltip = "Multiply the size of every study contribution circle by this amount"
        )
      ),
      selectInput(
        inputId = ns("legend_position_dropdown"),
        label = "Legend position",
        choices = c(
          "Bottom-Right" = "BR",
          "Bottom-Left" = "BL",
          "Top-Right" = "TR",
          "Top-Left" = "TL"
        ),
        selectize = FALSE
      )
    )
  )
}

#' Create the regression plot server.
#'
#' @param id ID of the module.
#' @param model_output GEMTC model results found by calling `CovariateModelOutput()`.
#' @param treatment_df Reactive containing data frame containing treatment IDs (Number), sanitised names (Label), and original names (RawLabel).
regression_plot_panel_server <- function(id, model_output, treatment_df, reference) {
  shiny::moduleServer(id, function(input, output, session) {
    
    available_to_add <- reactive({
      raw_labels = treatment_df()$RawLabel
      raw_reference = treatment_df()$RawLabel[treatment_df()$Label == model_output()$reference_name]
      return(raw_labels[(raw_labels != raw_reference) & !(raw_labels %in% added_comparators())])
    })
    
    added_comparators <- reactiveVal(c())
    
    # Add comparator on button click
    observe({
      added = unique(c(added_comparators(), input$add_comparator_dropdown))
      added_comparators(added)
    }) %>%
      bindEvent(input$add_comparator_btn)
    
    # Add all comparators on button click
    observe({
      added = unique(c(added_comparators(), available_to_add()))
      added_comparators(added)
    }) %>%
      bindEvent(input$add_all_btn)
    
    # Remove comparator on button click
    observe({
      added = added_comparators()[added_comparators() != input$remove_comparator_dropdown]
      added_comparators(added)
    }) %>%
      bindEvent(input$remove_comparator_btn, ignoreNULL = FALSE)
    
    # Remove all comparators on button click
    observe({
      added_comparators(character())
    }) %>%
      bindEvent(input$remove_all_btn, ignoreNULL = FALSE)
    
    # Update inputs on added comparators change
    observe({
      updateSelectInput(inputId = "add_comparator_dropdown", choices = available_to_add())
      updateSelectInput(inputId = "remove_comparator_dropdown", choices = added_comparators())
    })
    
    # Disable opacity when confidence regions not shown
    observe({
      shinyjs::toggleState(id = "confidence_opacity", condition = input$confidence)
    })
    
    # Disable contribution options when contributions not shown
    observe({
      shinyjs::toggleState(id = "contribution_options", condition = input$contributions)
    })
    
    contribution_type <- reactive({
      input$contribution_toggle
    })
    
    output$regression_plot <- renderPlot({
      if (length(added_comparators()) == 0) {
        comparators = c()
      } else {
        comparators <- sapply(added_comparators(), function(treatment) { treatment_df()$Label[treatment_df()$RawLabel == treatment] })
      }
      
      CreateCompositeMetaRegressionPlot(
        model_output = model_output(),
        treatment_df = treatment_df(),
        comparators = comparators,
        contribution_type = contribution_type(),
        include_covariate = input$covariate,
        include_ghosts = input$ghosts,
        include_extrapolation = input$extrapolate,
        include_confidence = input$confidence,
        confidence_opacity = input$confidence_opacity,
        include_contributions = input$contributions,
        contribution_multiplier = input$circle_multipler,
        legend_position = input$legend_position_dropdown
      )
    })
  })
}












# Test code to launch the panel as a self-contained app.
shiny::shinyApp(
  ui = fluidPage(
    tags$head(
      shinyjs::useShinyjs(),
      tags$script(src = "https://kit.fontawesome.com/23f0e167ac.js", crossorigin = "anonymous")
    ),
    numericInput(inputId = "covariate", label = "Covariate Value", value = 0),
    regression_plot_panel_ui(id = "TEST")
  ),
  server = function(input, output, session) {
    
    raw_data <- reactive({
      read.csv("../../tests/testthat/Cont_long_continuous_cov.csv")
    })
    
    raw_treatment_df <- reactive({
      CreateTreatmentIds(FindAllTreatments(raw_data()))
    })
    
    data <- reactive({
      return(WrangleUploadData(raw_data(), raw_treatment_df(), "Continuous"))
    })
    
    treatment_df <- reactive({
      CleanTreatmentIds(raw_treatment_df())
    })
    
    model <- reactive({
      RunCovariateModel(data(), treatment_df(), "Continuous", 'MD', "covar.age", "age", 'random', 'unrelated', "the_Little")
    })
    
    model_output = reactive({
      CovariateModelOutput(model(), input$covariate)
    })
    
    regression_plot_panel_server(
      id = "TEST",
      model_output = model_output,
      treatment_df = treatment_df
    )
  }
)