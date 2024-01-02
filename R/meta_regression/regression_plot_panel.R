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
      selectInput(inputId = ns("add_comparator_dropdown"), label = "Add Comparator", choices = c(), selectize = FALSE),
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
        checkboxInput(inputId = ns("covariate"), label = "Show covariate value", value = TRUE),
        tooltip = "Show the covariate value as a vertical line at the current value"
      ),
      # Confidence regions
      .AddTooltip(
        checkboxInput(inputId = ns("confidence"), label = "Show confidence regions"),
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
        checkboxInput(inputId = ns("ghosts"), label = "Show ghost comparisons", value = TRUE),
        tooltip = "Show other available comparisons in light grey behind the main plot"
      ),
      .AddTooltip(
        checkboxInput(inputId = ns("extrapolate"), label = "Extrapolate regression lines", value = TRUE),
        tooltip = "Extrapolate the regression lines beyond the range of the original data"
      ),
      # Contributions
      .AddTooltip(
        checkboxInput(inputId = ns("contributions"), label = "Show contributions", value = TRUE),
        tooltip = "Show study contributions as circles, where a bigger circle represents a larger contribution"
      ),
      
      div(
        id = ns("contribution_options"),
        p("Study circle sized by:"),
        radioButtons(
          inputId = ns("contribution_toggle"),
          label = "Study circle sized by:",
          choices = c(
            "% Contribution" = "percentage",
            "Inverse Variance" = "inverse variance"
          )
        ),
        numericInput(
          inputId = ns("circle_multipler"),
          label = "Circle Size Multiplier",
          min = 0,
          value = 1,
          step = 0.5
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
#' @param model Reactive containing GEMTC analysis model results.
#' @param treatment_df Reactive containing data frame containing treatment IDs (Number), sanitised names (Label), and original names (RawLabel).
#' @param reference_treatment Reactive containing name of reference treatment.
#' @param covariate_value Reactive containing the value of the covariate.
regression_plot_panel_server <- function(id, model, treatment_df, reference_treatment, covariate_value) {
  shiny::moduleServer(id, function(input, output, session) {
    
    available_to_add <- reactive({
      raw_labels = treatment_df()$RawLabel
      raw_reference = treatment_df()$RawLabel[treatment_df()$Label == reference_treatment()]
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
      
      if (input$covariate) {
        covariate <- covariate_value()
      } else {
        covariate <- NULL
      }
      
      CreateCompositeMetaRegressionPlot(
        model = model(),
        treatment_df = treatment_df(),
        reference = reference_treatment(),
        comparators = comparators,
        covariate_value = covariate,
        contribution_type = contribution_type(),
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
    shinyjs::useShinyjs(),
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
    
    regression_plot_panel_server(
      id = "TEST",
      model = model,
      treatment_df = treatment_df,
      reference_treatment = reactive({ "the_Little" }),
      covariate_value = reactive({ input$covariate })
    )
  }
)