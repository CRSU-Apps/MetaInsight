
#' Module UI for the data analysis page.
#' 
#' @param id ID of the module
#' @return Div for the data analysis page
data_analysis_page_ui <- function(id) {
  ns <- NS(id)
  
  page_numbering <- PageNumbering$new()
  
  div(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        data_analysis_options_panel_ui(id = ns("analysis_options"))
      ),
      mainPanel(
        width = 9,
        bsCollapse(
          id = ns("collapse"),
          bsCollapsePanel(
            title = "Data table (Click to open / hide this panel)",
            "Users can use the filter box under each column of heading to select studies to exclude in the sensitivity analysis.",
            DT::dataTableOutput(outputId = ns('datatb')),
            style = "warning"
          )
        ),
        tabsetPanel(
          tabPanel(
            title = paste0(page_numbering$AddChild(), " Data summary"),
            value = "data-summary",
            data_summary_panel_ui(id = ns("data_summary"), page_numbering)
          ),
          tabPanel(
            title = paste0(page_numbering$AddChild(), " Frequentist network meta-analysis"),
            value = "frequentist-analysis",
            frequentist_analysis_panel_ui(id = ns("frequentist_analysis"), page_numbering)
          ),
          tabPanel(
            title = paste0(page_numbering$AddChild(), " Bayesian network meta-analysis"),
            value = "bayesian-analysis",
            bayesian_analysis_panel_ui(id = ns("bayesian_analysis"), page_numbering)
          ),
          tabPanel(
            title = span(page_numbering$AddChild(), " Meta-regression", tags$sup("beta")),
            value = "meta-regression",
            meta_regression_tab_ui(id = ns("meta_regression"), page_numbering)
          )
        )
      )
    )
  )
}


#' Module server for the data analysis page.
#' 
#' @param id ID of the module
#' @param data Reactive containing data to analyse
#' @param is_default_data Reactive containing TRUE if data is an example dataset, loaded by default
#' @param treatment_df Reactive containing data frame containing treatment IDs (Number) and names (Label)
#' @param metaoutcome Reactive containing meta analysis outcome: "continuous" or "binary"
data_analysis_page_server <- function(id, data, is_default_data, treatment_df, metaoutcome) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    non_covariate_data <- reactive({ RemoveCovariates(data()) })
    
    OpenDataTable <- function() {
      updateCollapse(session, "collapse", open = "Data table (Click to open / hide this panel)")
    }

    analysis_options_reactives <- data_analysis_options_server(
      id = "analysis_options",
      data = data,
      is_default_data = is_default_data,
      metaoutcome = metaoutcome,
      OpenDataTable = OpenDataTable
    )


    outcome_measure = analysis_options_reactives$outcome_measure
    model_effects = analysis_options_reactives$model_effects
    exclusions = analysis_options_reactives$exclusions
    rank_option = analysis_options_reactives$rank_option
    continuous_outcome = analysis_options_reactives$continuous_outcome
    binary_outcome = analysis_options_reactives$binary_outcome

    #####
    # Reactive functions used in various places, based on the data
    #####

    # Make frequentist function (in fn_analysis.R) reactive - NVB
    freq_all <- reactive({
      return(frequentist(non_covariate_data(), metaoutcome(), treatment_df(), outcome_measure(), model_effects()))
    })

    # Make frequentist function (in fn_analysis.R) reactive with excluded studies - NVB
    freq_sub <- reactive({
      return(frequentist(non_covariate_data(), metaoutcome(), treatment_df(), outcome_measure(), model_effects(), exclusions()))
    })

    # Make bugsnetdata function (in analysis_generic.R) reactive - NVB
    bugsnetdt <- reactive({
      return(bugsnetdata(non_covariate_data(), metaoutcome(), treatment_df()))
    })

    # Make ref_alter function (in analysis_generic.R) reactive - NVB
    reference_alter <- reactive({
      return(ref_alter(non_covariate_data(), metaoutcome(), exclusions(), treatment_df()))
    })
    
    ### Get data for data table
    
    
    filtertable <- function() {
      label <- treatment_df()
      dt <- non_covariate_data()
      ntx <- nrow(label)
      dt$T <- factor(dt$T,
                     levels = c(1:ntx),
                     labels = as.character(label$Label))
      return(dt)
    }

    colnames <- function(){
      if (metaoutcome()=="Continuous") {
        colnames <- c('StudyID', 'Author','Treatment','Number of participants in each arm',
                      'Mean value of the outcome in each arm', 'Standard deviation of the outcome in each arm')

      } else{
        colnames <- c('StudyID', 'Author','Treatment','Number of participants with the outcome of interest in each arm','Number of participants in each arm'
        )
      }}

    output$datatb <- DT::renderDataTable(DT::datatable({
      filtertable()
    },editable=TRUE, rownames= FALSE,
    colnames= colnames(),
    filter = list(
      position = 'top', clear = FALSE, stateSave = TRUE)

    ))
    
    
    
    
    #######################
    ### 1. Data Summary ###
    #######################

    data_summary_panel_server(
      id = "data_summary",
      metaoutcome = metaoutcome,
      outcome_measure = outcome_measure,
      exclusions = exclusions,
      bugsnetdt = bugsnetdt,
      freq_all = freq_all,
      freq_sub = freq_sub
    )



    ######################
    ### 2. Frequentist ###
    ######################
    
    frequentist_analysis_panel_server(
      id = "frequentist_analysis",
      metaoutcome = metaoutcome,
      outcome_measure = outcome_measure,
      model_effects = model_effects,
      exclusions = exclusions,
      rank_option = rank_option,
      freq_all = freq_all,
      freq_sub = freq_sub,
      bugsnetdt = bugsnetdt,
      reference_alter = reference_alter
    )


    #####################
    #### 3. Bayesian ####
    #####################
    
    bayesian_analysis_panel_server(
      id = "bayesian_analysis",
      data = data,
      treatment_df = treatment_df,
      metaoutcome = metaoutcome,
      outcome_measure = outcome_measure,
      continuous_outcome = continuous_outcome,
      binary_outcome = binary_outcome,
      model_effects = model_effects,
      exclusions = exclusions,
      rank_option = rank_option,
      freq_all = freq_all,
      freq_sub = freq_sub,
      bugsnetdt = bugsnetdt,
      reference_alter = reference_alter
    )
    
    ############################
    #### 4. Meta-regression ####
    ############################
    meta_regression_tab_server(
      id = "meta_regression",
      all_data = data,
      treatment_df = treatment_df,
      reference_treatment = reactive({ reference_alter()$ref_all }),
      metaoutcome = metaoutcome,
      outcome_measure = outcome_measure,
      model_effects = model_effects,
      rank_option = rank_option,
      freq_all = freq_all,
      bugsnetdt = bugsnetdt
    )
  })
}