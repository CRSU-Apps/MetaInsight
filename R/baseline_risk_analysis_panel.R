#' Create the baseline risk analysis panel.
#'
#' @param id ID of the module
#' @return Div containing the module UI
baseline_risk_analysis_panel_ui <- function(id) {
  ns <- NS(id)
  div(
    fluidPage(
      # div(
      #   h2(textOutput(outputId = ns("subtitle"))),
      #   style = "display: inline-block; vertical-align: top; padding-right: 20pt;"
      # ),
      div(
        # Type selection if the covariate data is valid
        # conditionalPanel(
        #   condition = "output.valid_covariate",
        #   ns = ns,
        #   div(
        #     selectInput(
        #       inputId = ns("covariate_type_selection"),
        #       label = "",
        #       choices = c("Binary", "Continuous"),
        #       width = "120pt"
        #     ),
        #     style = "display: inline-block;"
        #   ),
          # div(
          #   # If binary data is poorly coded, then it will be identified as continuous.
          #   # Show a warning to the user when data is identified as continuous to inform them.
          #   conditionalPanel(
          #     condition = "output.inferred_type == 'Continuous'",
          #     ns = ns,
          #     div(
          #       tags$i(class = "fa-solid fa-circle-info"),
          #       title = "If the data is intended to be binary, the only allowed values are 0, 1, and NA",
          #       style = "color: orange;"
          #     )
          #   ),
          #   style = "display: inline-block; vertical-align: 65%"
          # ),
          # div(
          #   
          #   
          #   
          #   covariate_value_panel_ui(id = ns("covariate_value")),
          #   
          #   
          #   div(
          #     title = "This value will be used by all regression analysis output",
          #     style = "padding-left: 5px;",
          #     .CreateInlineBlock(
          #       h4("Covariate value:", tags$i(class="fa-regular fa-circle-question")),
          #       style = "padding-right: 5pt;"
          #     ),
          #     .CreateInlineBlock(
          #       # Continuous value input & warning if extrapolating outside of data range
          #       conditionalPanel(
          #         condition = "output.covariate_type == 'Continuous'",
          #         ns = ns,
          #         .CreateInlineBlock(
          #           numericInput(
          #             inputId = ns("numeric"),
          #             label = NULL,
          #             min = -.Machine$double.xmax,
          #             max = .Machine$double.xmax,
          #             value = 0,
          #             width = "100pt"
          #           )
          #         ),
          #         .CreateInlineBlock(
          #           conditionalPanel(
          #             condition = "output.extrapolated",
          #             ns = ns,
          #             div(
          #               "Covariate value outside data range",
          #               style = "color: orange; font-style: italic; font-weight: bold; padding-left: 10pt"
          #             )
          #           )
          #         )
          #       ),
                # Binary value input
                # conditionalPanel(
                #   condition = "output.covariate_type == 'Binary'",
                #   ns = ns,
                #   div(
                #     .CreateInlineBlock("0", style = "padding-right: 10pt;"),
                #     shinyWidgets::materialSwitch(
                #       inputId = ns("toggle"),
                #       inline = TRUE
                #     ),
                #     .CreateInlineBlock("1")
                #   )
                # )
      #         )
      #       ),
      #       
      #       
      #       
      #       style = "display: inline-block; vertical-align: 65%"
      #     )
      #   ),
      #   style = "display: inline-block;"
      # ),
      # Show error message if invalid data
      # conditionalPanel(
      #   condition = "!output.valid_covariate",
      #   ns = ns,
      #   div(
      #     textOutput(outputId = ns("error_message_box")),
      #     style = "display: inline-block; color: red; font-style: italic; font-weight: bold; padding-right: 20pt;"
      #   ),
      #   style = "vertical-align: 65%"
      # ),
      # Meta-regression UI
      # conditionalPanel(
      #   condition = "output.valid_covariate",
      #   ns = ns,
        tabsetPanel(
          tabPanel(
            title = "4b-1. Forest plot",
            
            
            
            # covariate_run_model_ui(id = ns("cov_model")),
            
            
            
            div(
              helpText(
                "Baysesian baseline risk meta-regression using the bnma package.",
                br(),
                "UPDATE LATER Heterogeneity prior: standard deviation ~ U(0,X), where X represents a ",
                tags$i("very large"),
                "difference in the analysis' outcome scale and is determined from the data.",
                br(),
                tags$strong("Please note each simulation may take 20 seconds.", style = "color:#FF0000")
              ),
              fixedRow(
                align = "center",
                p(tags$strong("Results for all studies")),
                p("Please choose your regressor type, then click the button below to run meta-regression analysis (and each time you subsequently change any options)."),
                fluidRow(
                  div(selectInput(inputId = ns("select_regressor"), 
                                  label = "Choose type of regression coefficient", 
                                  choices = c("common", "exchangeable", "independent")),
                      style = "display: inline-block;"),
                  div(
                    shinyWidgets::dropMenu(shinyWidgets::dropdownButton(
                      size = 'xs', status = "info", icon=icon('info')), align = 'left',
                      p(tags$strong("Types of regressors")),
                      p(tags$u("Common:"), " Coefficient is the same for all treatment comparisons"),
                      p(tags$u("Exchangeable:"), " Coefficient is different for each treatment comparison but all come from a shared distribution"),
                      p(tags$u("Independent:"), " Coefficient is different for each treatment comparison")),
                    style = "display:inline-block; vertical-align: top;"
                  )
                ),
                actionButton(inputId = ns("baye_do"), label = "Click here to run the main analysis for all studies")
              )
            ),
            
            
            
            
            
            fixedRow(
              align = "center",
              
              
              
              
              # bayesian_forest_plot_plus_stats_ui(id = ns("cov_forest_plots")),
              
              
              
              div(
                uiOutput(outputId = ns("bayesian_forest_plot")),
                fixedRow(
                  p("Options to change limits of the x-axis:"),
                  column(
                    width = 6,
                    align = 'center',
                    numericInput(inputId = ns('axis_min'), label = "Minimum", value = 0, step = 0.1)
                  ),
                  column(
                    width = 6,
                    align = 'center',
                    numericInput(inputId = ns('axis_max'), label = "Maximum", value = 5, step = 1)
                  )
                ),
                p("Model fit:"),
                tableOutput(outputId = ns("dic")),
                textOutput(outputId = ns("tau_text")),
                br(),
                br(),
                radioButtons(
                  inputId = ns('download_format'),
                  label = 'Document format',
                  choices = c('PDF', 'PNG'),
                  inline = TRUE
                ),
                downloadButton(outputId = ns('download_plot'))
              )
              
              
              
              
            )
          ),
          # tabPanel(
          #   title = "4c-2. Regression plot",
          #   regression_plot_panel_ui(id = ns("regression_plot"))
          # )
        )
      )
    )
  )
}


#' Create the baseline risk analysis server.
#'
#' @param id ID of the module
baseline_risk_analysis_panel_server <- function(    id, 
                                                    all_data,
                                                    treatment_df,
                                                    reference_treatment,
                                                    metaoutcome,
                                                    outcome_measure,
                                                    model_effects,
                                                    bugsnetdt
) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # covariate_title <- reactive({
    #   FindCovariateNames(all_data())[1]
    # })
    
    # covariate_name <- reactive({
    #   GetFriendlyCovariateName(covariate_title())
    # })
    
    # covariate_type <- reactive({
    #   input$covariate_type_selection
    # })
    
    # output$subtitle <- renderText({
    #   return(glue::glue("Covariate: {covariate_name()}"))
    # })
    
    # error_message <- reactiveVal("")
    # output$error_message_box <- renderText({ error_message() })
    
    # inferred_type <- reactiveVal()
    
    # Try to infer the type of the covariate, and display an error to the user if the data is bad
    # observe({
    #   tryCatch(
    #     {
    #       inferred_type <- ValidateAndInferCovariateType(all_data(), covariate_title())
    #       shiny::updateSelectInput(inputId = "covariate_type_selection", selected = inferred_type)
    #       inferred_type(inferred_type)
    #       if (inferred_type == "Continuous") {
    #         shinyjs::disable(id = "covariate_type_selection")
    #       } else {
    #         shinyjs::enable(id = "covariate_type_selection")
    #       }
    #       error_message("")
    #     },
    #     error = function(exptn) {
    #       inferred_type(NULL)
    #       error_message(exptn$message)
    #     }
    #   )
    # })
    
    # output$valid_covariate <- reactive({ error_message() == "" })
    # outputOptions(x = output, name = "valid_covariate", suspendWhenHidden = FALSE)
    
    
    # output$inferred_type <- reactive({ inferred_type() })
    # outputOptions(x = output, name = "inferred_type", suspendWhenHidden = FALSE)
    
    # 4b-1 Forest plots
    # run model
    
    
    
    
    # model_reactive <- covariate_run_model_server(
    #   id = "cov_model",
    #   data = all_data,
    #   treatment_df = treatment_df,
    #   metaoutcome = metaoutcome,
    #   outcome_measure = outcome_measure,
    #   covariate = covariate_title,
    #   cov_friendly = covariate_name,
    #   model_effects = model_effects
    # )
    
    
    
    # model <- eventReactive(input$baye_do, {
    #   RunCovariateModel(data = data(), treatment_ids = treatment_df(), outcome_type = metaoutcome(), 
    #                     outcome = outcome_measure(), covariate = covariate(), cov_friendly = cov_friendly(), 
    #                     model_type = model_effects(), regressor_type = input$select_regressor, 
    #                     ref_choice = treatment_df()$Label[match(1, treatment_df()$Number)])
    # })
    
    
    
    model_reactive <- eventReactive(input$baye_do, {
      glimpse(data())
      BaselineRiskRegression(
        BaselineRiskNetwork(
          FormatForBnma(br_data = data(),
                        treatment_ids = treatment_df(),
                        outcome_type = metaoutcome(),
                        ref = treatment_df()$Label[match(1, treatment_df()$Number)]),
          outcome_type = metaoutcome(),
          effects_type = model_effects(),
          cov_parameters = input$select_regressor),
        )
    })
    
  
    
    # covariate_value = covariate_value_panel_server(
    #   id = "covariate_value",
    #   covariate_type = reactive({ input$covariate_type_selection }),
    #   covariate_data = reactive({ all_data()[[covariate_title()]] })
    # )
    
    #Need this for the graph later on
    covariate_data <- reactive({GetReferenceOutcome(data = all_data(),
                                                    treatments = treatment_df()$Label[order(treatment_df()$Number)],
                                                    outcome_type = metaoutcome())
      })
    
    
    
    # shiny::moduleServer(id, function(input, output, session) {
      
      # Minimum covariate value in the data
      # min_value <- reactive({
      #   if (is.null(covariate_data())) {
      #     return(NULL)
      #   }
      #   return(min(covariate_data()))
      # })
      
      # Maximum covariate value in the data
      # max_value <- reactive({
      #   if (is.null(covariate_data())) {
      #     return(NULL)
      #   }
      #   return(max(covariate_data()))
      # })
      
      # Update the numeric input to the centre of the range, or the default value,
      # and the step to be a reasonable size of roughly 100 steps
      # observe({
      #   range <- max_value() - min_value()
      #   log_val <- round(log10(range))
      #   step <- 10 ** (log_val - 2)
      #   value <- (min_value() + max_value()) / 2
      #   shiny::updateNumericInput(inputId = "numeric", value = value, step = step)
      # })
      
      # covariate_value <- reactiveVal(0)
      
      # Update value when numeric input changes
      # observe({
      #   if (!is.null(covariate_type()) && covariate_type() == "Continuous") {
      #     covariate_value(input$numeric)
      #   }
      # })
      
      # Update value when toggle input changes
      # observe({
      #   if (!is.null(covariate_type()) && covariate_type() == "Binary") {
      #     covariate_value(ifelse(input$toggle, 1, 0))
      #   }
      # })
      
      # Update the client code to display the correct value input for the covariate type
      # output$covariate_type <- reactive({ covariate_type() })
      # outputOptions(x = output, name = "covariate_type", suspendWhenHidden = FALSE)
      
      # Update the client code to inform the user when the covariate value is outside the range of the data
      # output$extrapolated <- reactive({ covariate_value() < min_value() || covariate_value() > max_value() })
      # outputOptions(x = output, name = "extrapolated", suspendWhenHidden = FALSE)
      
      # return(debounce(r = reactive({ covariate_value() }), millis = 500))
    
    
    
    
    
    
    
    
    # obtain gemtc output types to be used in rest of page
    # model_output <- reactive(CovariateModelOutput(model = model_reactive(), cov_value = covariate_value()))
    
    
    
    
    
    
    # Create forest plot and associated statistics
    # bayesian_forest_plot_plus_stats_server(
    #   id = "cov_forest_plots",
    #   model_output = model_output,
    #   analysis_type = "Regression",
    #   metaoutcome = metaoutcome,
    #   outcome_measure = outcome_measure,
    #   bugsnetdt = bugsnetdt
    # )
    
    
    
    
    # forest min and max values different if continuous/binary
    observe({
      # if (outcome_measure() %in% c("OR", "RR")) {
      if (outcome_measure() == "OR") {
        updateNumericInput(inputId = "axis_min", value = 0.1, step = 0.1)
        updateNumericInput(inputId = "axis_max", value = 5)
      # } else if (outcome_measure() %in% c("SMD", "MD", "RD")) {
      } else if (outcome_measure() == "MD") {
        updateNumericInput(inputId = "axis_min", value = -10, step = 1)
        updateNumericInput(inputId = "axis_max", value = 10)
      } else {
        # stop("outcome_measure needs to me 'OR', 'RR', 'RD', 'MD', or 'SMD'")
        stop("outcome_measure needs to be 'OR' or 'MD'")
      }
    })
    
    # Forest plot
    
    # Forest plot for all studies
    output$forest_plot <- renderPlot({
      
      
      
      
      
      # CreateForestPlot(model_output(), metaoutcome(), input$axis_min, input$axis_max)
      
      
      # CreateForestPlot <- function(model, metaoutcome, bayesmin, bayesmax) {
      #   if (metaoutcome=="Binary") {
      #     return(gemtc::forest(model$mtcRelEffects, digits=3, xlim=c(log(bayesmin), log(bayesmax))))
      #   } else if (metaoutcome=="Continuous") {
      #     return(gemtc::forest(model$mtcRelEffects, digits=3, xlim=c(bayesmin, bayesmax)))
      #   }
      # }
      
      
      bnma::network.forest.plot(model_reactive(), only.reference.treatment = TRUE)
      
      
      
    #   title(paste(ifelse(analysis_type == "Full", "All studies:", 
    #                      ifelse(analysis_type == "Sub", "Results with studies excluded:",
    #                             "Regression analysis:")),
    #               "
    #               Bayesian",
    #               model_output()$a, 
    #               "consistency model forest plot results"))
    # })
    
    
    title("Bayesian consistency model forest plot results")
    })
    
    
    # DIC table for all studies
    output$dic <- renderTable ({
      # model_output()$dic
      model_reactive()$deviance$DIC
    }, digits=3, rownames=TRUE, colnames=FALSE
    )
    
    # Tau all studies
    output$tau_text <-renderText({
      
      
      # CreateTauSentence(model_output(), outcome_measure())
      # CreateTauSentence(FormatForCreateTauSentence(model_reactive()), outcome_measure())
      "Test sentence"
      
      
    })
    
    ns <- session$ns
    
    # Interactive UI
    output$bayesian_forest_plot <- renderUI({
      shinycssloaders::withSpinner(
        plotOutput(
          outputId = ns("forest_plot"),
          # outputId = "forest_plot",
          width="630px",
          height = BayesPixels(
            as.numeric(bugsnet_sumtb(bugsnetdt(), metaoutcome())$Value[1]),
            
            
            
            
            # bugsnet_sumtb <- function(data, metaoutcome){
            #   data.rh<-data.prep(arm.data=data, varname.t = "T", varname.s="Study")
            #   if (metaoutcome=="Continuous") {
            #     outcome = "Mean"
            #     typeO= "continuous"
            #   } else {
            #     outcome = "R"
            #     typeO = "binomial"
            #   }
            #   network.char <- net.tab(data = data.rh,
            #                           outcome = outcome,
            #                           N = "N",
            #                           type.outcome = typeO,
            #                           time = NULL)
            #   return(network.char$network)
            # }
            
            
            
            
            
            title = TRUE
          )
        ), type = 6)
    })
    
    output$download_plot <- downloadHandler(
      filename = function() {
        paste0(ifelse(analysis_type == "Full", 'All_studies.', 
                      ifelse(analysis_type == "Sub", 'Excluded_studies.',
                             'Regression_analysis.')),
               input$download_format)
      },
      content = function(file) {
        if (input$download_format == "PDF") {
          pdf(file = file, width = 9, height = BayesInch(as.numeric(bugsnet_sumtb(bugsnetdt(), metaoutcome())$Value[1])))
        } else if (input$download_format == "PNG") {
          png(file = file, width = 610, height = BayesPixels(as.numeric(bugsnet_sumtb(bugsnetdt(), metaoutcome())$Value[1])))
        }
        if (metaoutcome() == "Binary") {
          # gemtc::forest(model_output()$mtcRelEffects, digits = 3, xlim = c(log(input$axis_min), log(input$axis_max)))
          bnma::network.forest.plot(model_reactive(), only.reference.treatment = TRUE)
        } else if (metaoutcome() == "Continuous") {
          # gemtc::forest(model_output()$mtcRelEffects, digits = 3, xlim = c(input$axis_min, input$axis_max))
          bnma::network.forest.plot(model_reactive(), only.reference.treatment = TRUE)
        }
        if (analysis_type == "Regression") {
          # mtext(model_output()$cov_value_sentence, side = 1, adj = 0)
          mtext("CHANGE LATER", side = 1, adj = 0)
        }
        dev.off()
      }
    )
    
    })
    
    
    
    
    
    
    
    
    # # 4c-2 Regression plot
    # regression_plot_panel_server(
    #   id = "regression_plot",
    #   model = model_reactive,
    #   reference_treatment = reference_treatment,
    #   treatment_df = treatment_df,
    #   covariate_value = covariate_value
    # )
#   })
}

