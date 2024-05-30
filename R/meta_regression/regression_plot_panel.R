.CreateInlineBlock <- function(..., style = NULL) {
  return(
    div(
      ...,
      style = paste0("display: inline-block; ", style)
    )
  )
}

.AddRegressionOptionTooltip <- function(..., tooltip, style = NA) {
  return(
    div(
      ...,
      title = tooltip,
      style = style
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
      plotOutput(outputId = ns("regression_plot"), height = "800px"),
      div(
        radioButtons(
          inputId = ns("format"), 
          label = "Download format", 
          choices = c(
            "PDF" = "pdf",
            "PNG" = "png",
            "SVG" = "svg"
          ),
          inline = TRUE
        ),
        downloadButton(outputId = ns("download")),
        style = "text-align: center;"
      )
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
      div(
        id = ns("confidence_options"),
          checkboxInput(
          inputId = ns("confidence"),
          label = div(
            .AddRegressionOptionTooltip(
              "Show confidence regions",
              tags$i(class="fa-regular fa-circle-question"),
              tooltip = "Show confidence regions for the added comparisons. This will not show if there is one or zero direct contributions"
            ),
            uiOutput(outputId = ns("confidence_info")),
            style = "display: -webkit-inline-box;"
          )
        ),
        sliderInput(
          inputId = ns("confidence_opacity"),
          label = "Confidence Region Opacity",
          min = 0,
          max = 0.5,
          step = 0.01,
          value = 0.2
        )
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
        selectInput(
          inputId = ns("contributions"),
          label = tags$html("Show contributions", tags$i(class="fa-regular fa-circle-question")),
          choices = c(
            "None",
            "Treatment Effect",
            "Covariate Effect"
          ),
          selected = "Treatment Effect",
          selectize = FALSE
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
#' @param data Reactive containing study data including covariate columns, in wide or long format.
#' @param covariate_title Reactive containing title of the covariate column in the data.
#' @param covariate_name Friendly name of chosen covariate.
#' @param model_output Reactive containing model results found by calling `CovariateModelOutput()` or `BaselineRiskModelOutput()`.
#' @param treatment_df Reactive containing data frame containing treatment IDs (Number), sanitised names (Label), and original names (RawLabel).
#' @param outcome_type Reactive containing meta analysis outcome: "Continuous" or "Binary".
#' @param outcome_measure Reactive type of outcome (OR, RR, RD, MD or SD).
#' @param package package used to create the model. Either "gemtc" (default) or "bnma".
regression_plot_panel_server <- function(id, data, covariate_title, covariate_name, model_output, treatment_df, outcome_type, outcome_measure, reference, package = "gemtc") {
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
    }) |>
      bindEvent(input$add_comparator_btn)
    
    # Add all comparators on button click
    observe({
      added = unique(c(added_comparators(), available_to_add()))
      added_comparators(added)
    }) |>
      bindEvent(input$add_all_btn)
    
    # Remove comparator on button click
    observe({
      added = added_comparators()[added_comparators() != input$remove_comparator_dropdown]
      added_comparators(added)
    }) |>
      bindEvent(input$remove_comparator_btn, ignoreNULL = FALSE)
    
    # Remove all comparators on button click
    observe({
      added_comparators(character())
    }) |>
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
    
    if (!(package %in% c("gemtc", "bnma"))) {
      stop("'package' must be 'gemtc' or 'bnma'")
    }
    
    # Background process to calculate confidence regions
    confidence_regions <- shiny::ExtendedTask$new(function(model_output) {
      promises::future_promise({
        if (package == "gemtc") {
          return(CalculateConfidenceRegions(model_output))
        } else if (package == "bnma") {
          return(CalculateConfidenceRegionsBnma(model_output))
        }
      })
    })
    
    # Start calculation of confidence regions when the model output changes
    observe({
      confidence_regions$invoke(model_output())
    }) |>
      bindEvent(model_output())

    calculating_confidence_regions <- reactiveVal(FALSE)
    previous_confidence_regions_shown <- reactiveVal(FALSE)

    # When model output changes, save the current state of the confidence region checkbox, then deselect and disable it
    observe({
      calculating_confidence_regions(TRUE)
      previous_confidence_regions_shown(input$confidence)
      updateCheckboxInput(inputId = "confidence", value = FALSE)
      shinyjs::disable(id = "confidence_options")
    }) |>
      bindEvent(model_output())

    # When the confidence regions have been calculated, reenable the checkbox, and reset its value
    observe({
      shinyjs::enable(id = "confidence_options")
      updateCheckboxInput(inputId = "confidence", value = previous_confidence_regions_shown())
      calculating_confidence_regions(FALSE)
    }) |>
      bindEvent(confidence_regions$result())

    # Show a spinner when the confidence regions are being calculated
    output$confidence_info <- renderUI({
      if (!calculating_confidence_regions()) {
        return(NULL)
      }

      .AddRegressionOptionTooltip(
        tags$i(class = "fa-solid fa-circle-notch fa-spin"),
        tooltip = "Calculating confidence regions",
        style = "color: blue;"
      )
    })
    
    contribution_matrix <- reactive({
      tryCatch(
        expr = {
          if (package == "gemtc") {
            cov_parameters <- model_output()$mtcResults$model$regressor$coefficient
          } else if (package == "bnma") {
            if (model_output()$mtcResults$network$baseline == "common") {
              cov_parameters <- "shared"
            } else if (model_output()$mtcResults$network$baseline == "independent") {
              cov_parameters <- "unrelated"
            } else {
              cov_parameters <- model_output()$mtcResults$network$baseline
            }
          }

          if (model_output()$model == "random") {
            if (package == "gemtc") {
              std_dev_d <- mtc_summary()$summaries$quantiles["sd.d", "50%"]
            } else if (package == "bnma") {
              std_dev_d <- mtc_summary()$summary.samples$quantiles["sd", "50%"]
            }
          } else {
            std_dev_d <- NULL
          }

          if (cov_parameters == "exchangeable") {
            if (package == "gemtc") {
              std_dev_beta <- mtc_summary()$summaries$quantiles["reg.sd", "50%"]
            } else if (package == "bnma") {
              std_dev_beta <- mtc_summary()$summary.samples$quantiles["sdB", "50%"]
            }
          } else {
            std_dev_beta <- NULL
          }

          CalculateContributions(
            data = data(),
            covariate_title = covariate_title(),
            treatment_ids = treatment_df(),
            outcome_type = outcome_type(),
            outcome_measure = outcome_measure(),
            effects_type = model_output()$model,
            std_dev_d = std_dev_d,
            std_dev_beta = std_dev_beta,
            cov_parameters = cov_parameters,
            study_or_comparison_level = "study",
            absolute_or_percentage = input$absolute_relative_toggle,
            weight_or_contribution = input$contribution_weight_toggle,
            treatment_or_covariate_effect = input$contributions
          )
        },
        error = function(err) {
          return(NULL)
        }
      )
    })

    contributions_failed <- reactiveVal(NULL)

    # Show warning when contribution matrix fails to calculate
    output$contributions_missing <- renderUI({
      if (!is.null(contributions_failed())) {
        return(
          div(
            glue::glue("{contributions_failed()} contribution matrix cannot be calculated"), tags$i(class="fa-regular fa-circle-question"),
            style = "color: #ff0000;",
            title = "This possibly indicates a poorly fitting model. Please check model diagnostics in the Result Details and Deviance Report tabs"
          )
        )
      } else {
        return(NULL)
      }
    })

    # Reset failed contributions when model recalculated
    observe({
      contributions_failed(NULL)
    }) |> bindEvent(model_output())
    
    # Reset contributions to "None" and record failed contribution matrix calculation
    observe({
      if (!is.null(model_output()) && input$contributions != "None" && is.null(contribution_matrix())) {
        updateSelectInput(inputId = "contributions", selected = "None")
        contributions_failed(input$contributions)
      }
    })
    
    comparator_titles <- reactive({
      if (length(added_comparators()) == 0) {
        comparators <- c()
      } else {
        comparators <- sapply(added_comparators(), function(treatment) { treatment_df()$Label[treatment_df()$RawLabel == treatment] })
      }
    })
    
    output$regression_plot <- renderPlot({
      CreateCompositeMetaRegressionPlot(
        model_output = model_output(),
        treatment_df = treatment_df(),
        outcome_measure = outcome_measure(),
        comparators = comparator_titles(),
        contribution_matrix = contribution_matrix(),
        contribution_type = input$absolute_relative_toggle,
        confidence_regions = confidence_regions$result(),
        include_covariate = input$covariate,
        include_ghosts = input$ghosts,
        include_extrapolation = input$extrapolate,
        include_confidence = input$confidence,
        confidence_opacity = input$confidence_opacity,
        include_contributions = input$contributions != "None",
        contribution_multiplier = input$circle_multipler,
        legend_position = input$legend_position_dropdown
      )
    })
    
    output$download <- downloadHandler(
      filename = function() {
        return(glue::glue("regression_plot_{covariate_name()}.{input$format}"))
      },
      content = function(file) {
        ggsave(
          filename = file, 
          device = input$format,
          bg = "#ffffff",
          plot = CreateCompositeMetaRegressionPlot(
            model_output = model_output(),
            treatment_df = treatment_df(),
            outcome_measure = outcome_measure(),
            comparators = comparator_titles(),
            contribution_matrix = contribution_matrix(),
            contribution_type = input$absolute_relative_toggle,
            confidence_regions = confidence_regions$result(),
            include_covariate = input$covariate,
            include_ghosts = input$ghosts,
            include_extrapolation = input$extrapolate,
            include_confidence = input$confidence,
            confidence_opacity = input$confidence_opacity,
            include_contributions = input$contributions != "None",
            contribution_multiplier = input$circle_multipler,
            legend_position = input$legend_position_dropdown
          )
        )
      }
    )
  })
}
