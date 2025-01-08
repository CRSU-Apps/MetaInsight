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
  
  div(
    # Spinner overlaid on the plot area to show model running
    div(
      shinycssloaders::withSpinner(
        type = 6,
        uiOutput(outputId = ns("spinner"))
      ),
      style = "height: 0px;  position: inherit;"
    ),
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
        ),
        h4(style = "text-align:left",
           "This graph was adapted from",
           tags$em("Graphs of study contributions and covariate distributions for network meta-regression"),
           ", Sarah Donegan, Sofia Dias, Catrin Tudur-Smith, Valeria Marinho, Nicky J Welton, ",
           tags$em("Res Syn Meth"),
           ", 2018;",
           tags$b(9),
           ":243-260.",
           tags$b(tags$a("DOI: 10.1002/jrsm.1292", href = "https://onlinelibrary.wiley.com/doi/10.1002/jrsm.1292", target = "_blank"))
        )
      ),
      sidebarPanel = sidebarPanel(
        width = 3,
        add_remove_panel_ui(id = ns("added_comparators")),
        
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
        # Credible regions
        div(
          id = ns("credible_options"),
            checkboxInput(
            inputId = ns("credible"),
            label = div(
              .AddRegressionOptionTooltip(
                "Show credible regions",
                tags$i(class="fa-regular fa-circle-question"),
                tooltip = "Show credible regions for the added comparisons. This will not show if there is one or zero direct contributions"
              ),
              uiOutput(outputId = ns("credible_info")),
              style = "display: -webkit-inline-box;"
            )
          ),
          sliderInput(
            inputId = ns("credible_opacity"),
            label = "Credible Region Opacity",
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
                tooltip = "Circles scaled by percentage contribution of each study to each parameter"
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
#' @param model_valid Reactive containing whether or not the model is valid to be displayed.
regression_plot_panel_server <- function(
    id,
    data,
    covariate_title,
    covariate_name,
    model_output,
    treatment_df,
    outcome_type,
    outcome_measure,
    reference,
    package = "gemtc",
    model_valid = reactiveVal(TRUE)) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # This will show a spinner while the model calculates, but will not render anything once it has completed
    output$spinner <- renderUI({
      model_output()
      NULL
    })
    
    added_comparators <- add_remove_panel_server(id = "added_comparators", reactive({ treatment_df()$RawLabel }), reference)
    
    # Disable opacity when credible regions not shown
    observe({
      shinyjs::toggleState(id = "credible_opacity", condition = input$credible)
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
    
    # Background process to calculate credible regions
    credible_regions <- shiny::ExtendedTask$new(function(model_output) {
      promises::future_promise({
        if (package == "gemtc") {
          return(CalculateCredibleRegions(model_output))
        } else if (package == "bnma") {
          return(CalculateCredibleRegionsBnma(model_output))
        }
      })
    })
    
    # Start calculation of credible regions when the model output changes
    observe({
      credible_regions$invoke(model_output())
    }) |>
      bindEvent(model_output())

    calculating_credible_regions <- reactiveVal(FALSE)
    previous_credible_regions_shown <- reactiveVal(FALSE)

    # When model output changes, save the current state of the credible region checkbox, then deselect and disable it
    observe({
      calculating_credible_regions(TRUE)
      previous_credible_regions_shown(input$credible)
      updateCheckboxInput(inputId = "credible", value = FALSE)
      shinyjs::disable(id = "credible_options")
    }) |>
      bindEvent(model_output())

    # When the credible regions have been calculated, reenable the checkbox, and reset its value
    observe({
      shinyjs::enable(id = "credible_options")
      updateCheckboxInput(inputId = "credible", value = previous_credible_regions_shown())
      calculating_credible_regions(FALSE)
    }) |>
      bindEvent(credible_regions$result())

    # Show a spinner when the credible regions are being calculated
    output$credible_info <- renderUI({
      if (!calculating_credible_regions()) {
        return(NULL)
      }

      .AddRegressionOptionTooltip(
        tags$i(class = "fa-solid fa-circle-notch fa-spin"),
        tooltip = "Calculating credible regions",
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

          long_data <- reactive({
            if (FindDataShape(data()) == "wide") {
              return(WideToLong(data(), outcome_type = outcome_type()))
            } else {
              return(data())
            }
          })
          
          CalculateContributions(
            data = long_data(),
            covariate_title = covariate_title(),
            treatment_ids = treatment_df(),
            outcome_type = outcome_type(),
            outcome_measure = outcome_measure(),
            effects_type = model_output()$model,
            std_dev_d = std_dev_d,
            std_dev_beta = std_dev_beta,
            cov_parameters = cov_parameters,
            study_or_arm_level = "study",
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
            glue::glue("Contribution matrix cannot be calculated"), tags$i(class="fa-regular fa-circle-question"),
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
        comparators <- sapply(added_comparators(), function(comparator) { treatment_df()$Label[treatment_df()$RawLabel == comparator] })
      }
    })
    
    output$regression_plot <- renderPlot({
      tryCatch(
        expr = {
          if (!model_valid()) {
            stop()
          }
          shinyjs::enable(id = "download")
          CreateCompositeMetaRegressionPlot(
            model_output = model_output(),
            treatment_df = treatment_df(),
            outcome_measure = outcome_measure(),
            comparators = comparator_titles(),
            contribution_matrix = contribution_matrix(),
            contribution_type = input$absolute_relative_toggle,
            credible_regions = credible_regions$result(),
            include_covariate = input$covariate,
            include_ghosts = input$ghosts,
            include_extrapolation = input$extrapolate,
            include_credible = input$credible,
            credible_opacity = input$credible_opacity,
            include_contributions = input$contributions != "None",
            contribution_multiplier = input$circle_multipler,
            legend_position = input$legend_position_dropdown
          )
        },
        error = function(exptn) {
          shinyjs::disable(id = "download")
          ggplot(data = data.frame(text = c("Please rerun analysis"))) +
            ggplot2::theme_void() +
            ggplot2::geom_text(mapping = ggplot2::aes(label = text, x = 0, y = 0), size = 10)
        }
      )
    })
    
    # Disable download button until the model has been run
    shinyjs::disable(id = "download")
    
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
            credible_regions = credible_regions$result(),
            include_covariate = input$covariate,
            include_ghosts = input$ghosts,
            include_extrapolation = input$extrapolate,
            include_credible = input$credible,
            credible_opacity = input$credible_opacity,
            include_contributions = input$contributions != "None",
            contribution_multiplier = input$circle_multipler,
            legend_position = input$legend_position_dropdown
          )
        )
      }
    )
  })
}
