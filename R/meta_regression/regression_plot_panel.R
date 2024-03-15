.CreateInlineBlock <- function(..., style = NULL) {
  return(
    div(
      ...,
      style = paste0("display: inline-block; ", style)
    )
  )
}

.AddRegressionOptionTooltip <- function(..., tooltip) {
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
        label = .AddRegressionOptionTooltip(
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
      .AddRegressionOptionTooltip(
        checkboxInput(
          inputId = ns("covariate"),
          label = tags$html("Show covariate value ", tags$i(class="fa-regular fa-circle-question")),
          value = TRUE
          ),
        tooltip = "Show the covariate value as a vertical line at the current value"
      ),
      # Confidence regions
      .AddRegressionOptionTooltip(
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
      .AddRegressionOptionTooltip(
        checkboxInput(
          inputId = ns("ghosts"),
          label = tags$html("Show ghost comparisons", tags$i(class="fa-regular fa-circle-question")),
          value = TRUE
        ),
        tooltip = "Show other available comparisons in light grey behind the main plot"
      ),
      .AddRegressionOptionTooltip(
        checkboxInput(
          inputId = ns("extrapolate"),
          label = tags$html("Extrapolate regression lines", tags$i(class="fa-regular fa-circle-question")),
          value = TRUE
        ),
        tooltip = "Extrapolate the regression lines beyond the range of the original data"
      ),
      # Contributions
      .AddRegressionOptionTooltip(
        checkboxInput(
          inputId = ns("contributions"),
          label = tags$html("Show contributions", tags$i(class="fa-regular fa-circle-question")),
          value = TRUE
        ),
        tooltip = "Show study contributions as circles, where a bigger circle represents a larger contribution"
      ),
      uiOutput(outputId = ns("contributions_missing")),
      
      div(
        id = ns("contribution_options"),
        radioButtons(
          inputId = ns("absolute_relative_toggle"),
          label = "Study circle sized by:",
          choiceNames = list(
            .AddRegressionOptionTooltip(
              tags$html("% Contribution", tags$i(class="fa-regular fa-circle-question")),
              tooltip = "Circles scaled by percentage contribution of each study to each treatment regression"
            ),
            .AddRegressionOptionTooltip(
              tags$html("Absolute contribution", tags$i(class="fa-regular fa-circle-question")),
              tooltip = "Circles scaled by absolute contribution of each study"
            )
          ),
          choiceValues = c("percentage", "absolute")
        ),
        radioButtons(
          inputId = ns("contribution_weight_toggle"),
          label = "Contribution type:",
          choiceNames = list(
            .AddRegressionOptionTooltip(
              tags$html("Contribution", tags$i(class="fa-regular fa-circle-question")),
              tooltip = "Circles scaled by total contribution to the regression; both the weight and the value are taken into account"
            ),
            .AddRegressionOptionTooltip(
              tags$html("Weight", tags$i(class="fa-regular fa-circle-question")),
              tooltip = "Circles scaled by weight of each study; this does not take into account how much this study affects the regression"
            )
          ),
          choiceValues = c("contribution", "weight")
        ),
        .AddRegressionOptionTooltip(
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
#' @param data Study data including covariate columns, in wide or long format.
#' @param model_output GEMTC model results found by calling `CovariateModelOutput()`.
#' @param treatment_df Reactive containing data frame containing treatment IDs (Number), sanitised names (Label), and original names (RawLabel).
#' @param outcome_type Reactive containing meta analysis outcome: "Continuous" or "Binary".
#' @param outcome_measure Reactive type of outcome (OR, RR, RD, MD or SD).
regression_plot_panel_server <- function(id, data, model_output, treatment_df, outcome_type, outcome_measure, reference) {
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
    
    mtc_summary <- reactive({
      summary(model_output()$mtcResults)
    })
      
    contribution_matrix <- reactive({
      tryCatch(
        expr = {
          if (model_output()$model == "random") {
            std_dev_d <- mtc_summary()$summaries$quantiles["sd.d", "50%"]
          } else {
            std_dev_d <- NULL
          }
          
          if (model_output()$mtcResults$model$regressor$coefficient == "exchangeable") {
            std_dev_beta <- mtc_summary()$summaries$quantiles["reg.sd", "50%"]
          } else {
            std_dev_beta <- NULL
          }
      
          CreateContributionMatrix(
            data = data(),
            treatment_ids = treatment_df(),
            outcome_type = outcome_type(),
            outcome_measure = outcome_measure(),
            effects_type = model_output()$model,
            std_dev_d = std_dev_d,
            std_dev_beta = std_dev_beta,
            cov_parameters = model_output()$mtcResults$model$regressor$coefficient,
            study_or_comparison_level = "study",
            absolute_or_percentage = input$absolute_relative_toggle,
            weight_or_contribution = input$contribution_weight_toggle,
            full_output = FALSE
          )
        },
        error = function(err) {
          return(NULL)
        }
      )
    })
    
    output$contributions_missing <- renderUI({
      if (is.null(contribution_matrix())) {
        return(
          div(
            p("Contribution matrix cannot be calculated"),
            style = "color: #ff0000;"
          )
        )
      } else {
        return(NULL)
      }
    })
    
    previous_contributions <- reactiveVal(TRUE)
    
    observe({
      if (is.null(contribution_matrix())) {
        # Store current state of contributions toggle to reinstate once checkbox is reenabled
        previous_contributions(is.null(input$contibutions) || input$contibutions)
        
        updateCheckboxInput(inputId = "contributions", value = FALSE)
        shinyjs::disable(id = "contributions")
      } else {
        updateCheckboxInput(inputId = "contributions", value = previous_contributions())
        shinyjs::enable(id = "contributions")
      }
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
        outcome_type = outcome_type(),
        comparators = comparators,
        contribution_type = input$absolute_relative_toggle,
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