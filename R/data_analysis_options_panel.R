
#' Module UI for the data analysis options panel
#' 
#' @param id ID of the module
#' @return Div for the panel
data_analysis_options_panel_ui <- function(id) {
  ns <- NS(id)
  div(
    conditionalPanel(
      condition= "output.metaoutcome == 'Continuous'",
      ns = ns,
      radioButtons(
        inputId = ns("outcomeCont"),
        label = "Outcome for continuous data:",
        choices = c(
          "Mean Difference (MD)" = "MD",
          "Standardised Mean Difference (SMD)" = "SMD"
        )
      )
    ),
    conditionalPanel(
      condition = "output.metaoutcome == 'Binary'",
      ns = ns,
      radioButtons(
        inputId = ns("outcomebina"),
        label = "Outcome for binary data:",
        choices = c(
          "Odds Ratio (OR)" = "OR",
          "Risk Ratio (RR)" = "RR",
          "Risk Difference (RD)" = "RD"
        )
      )
    ),
    radioButtons(
      inputId = ns('rankopts'),
      label = 'For treatment rankings, smaller outcome values (e.g. smaller mean values for continuous data, or ORs less than 1 for binary data) are:',
      choices = c(
        "Desirable" = "good",
        "Undesirable" = "bad"
      )
    ),
    radioButtons(
      inputId = ns("modelranfix"),
      label = "Model:",
      choices = c(
        "Random effect (RE)" = "random",
        "Fixed effect (FE)" = "fixed"
      )
    ),
    h3("Select studies to exclude:"),
    p("Tips: you can use the data table to help find the study that you want to exclude."),
    actionButton(inputId = ns("datatablebutton"), label = "Open the data table"),
    br(),
    br(),
    uiOutput(outputId = ns("Choicesexcl")), 
    h5("NB: If a whole treatment is removed from the analysis the NMA will return an error message. To overcome this, please remove the treatment from the data.")
  )
}


#' Module server for the data analysis options panel
#' 
#' @param id ID of the module
#' @param data Reactive containing data to analyse
#' @param is_default_data Reactive containing TRUE if data is an example dataset, loaded by default
#' @param metaoutcome Reactive containing meta analysis outcome: "continuous" or "binary"
#' @param OpenDataTable Function to open the data table
#' 
#' @return List of reactives:
#' - "outcome_measure" contains the outcome measure being analysed.
#'   This will be related to the outcome in "metaoutcome"
#' - "model_effects" contains "random" or "fixed"
#' - "exclusions" contains vector of names of studies being excluded from the sensitivity analysis
#' - "rank_option" contains "good" or "bad" for whether a small value is desirable or not
#' - "continuous_outcome" contains acronym of the continuous outcome:
#'   "MD" for mean difference, or "SMD" for standardised mean difference
#' - "binary_outcome" contains acronym of the binary outcome:
#'   "OR" for odds ratio, "RR" for risk ratio, or "RD" for risk difference
data_analysis_options_server <- function(id, data, is_default_data, metaoutcome, OpenDataTable) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    continuous_outcome <- reactive({
      input$outcomeCont
    })
    
    binary_outcome <- reactive({
      input$outcomebina
    })
    
    outcome_measure <- reactive({
      if (metaoutcome() == "Continuous") {
        return(continuous_outcome())
      } else {
        return(binary_outcome())
      }
    })
    
    output$metaoutcome <- reactive({
      metaoutcome()
    })
    shiny::outputOptions(output, "metaoutcome", suspendWhenHidden = FALSE)
    
    exclusions <- debounce(reactive({input$exclusionbox}), 1500)
    
    ### Ranking defaults
    choice <- reactive({
      RankingOrder(metaoutcome(), is_default_data())
    })
    
    observe({
      choice2 <- choice()
      shiny::updateRadioButtons(inputId = "rankopts", selected = choice2)
    })
    
    ### Get studies for check box input
    
    output$Choicesexcl <- renderUI({
      newData <- data()
      newData1 <- as.data.frame(newData)
      # long format data contain exactly 6 columns for continuous and 5 for binary. wide format will contain at least 2+4*2=10 columns.
      if (ncol(newData1) == 6 || ncol(newData1) == 5) {
        newData2 <- newData1[order(newData1$StudyID, -newData1$T), ]
        # create counting variable for number of arms within each study.
        newData2$number<- ave(as.numeric(newData2$StudyID), newData2$StudyID, FUN = seq_along)
        data_wide <- reshape(
          data = newData2,
          timevar = "number",
          idvar = c("Study", "StudyID"),
          direction = "wide"
        )
      } else {
        data_wide <- newData1
      }
      checkboxGroupInput(
        inputId = ns("exclusionbox"),
        label = NULL,
        choices = as.character(data_wide$Study)
      )
    })
    
    model_effects <- reactive({
      input$modelranfix
    })
    
    rank_option <- reactive({
      input$rankopts
    })
    
    observeEvent(
      input$datatablebutton,
      {
        OpenDataTable()
      }
    )
    
    return(
      list(
        outcome_measure = outcome_measure,
        model_effects = model_effects,
        exclusions = exclusions,
        rank_option = rank_option,
        continuous_outcome = continuous_outcome,
        binary_outcome = binary_outcome
      )
    )
  })
}