
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
    
    OpenDataTable <- function() {
      updateCollapse(session, "collapse", open = "Data table (Click to open / hide this panel)")
    }

    analysis_options_reactives <- data_analysis_options_server(
      id = "analysis_options",
      data = data,
      is_default_data = is_default_data,
      treatment_df = treatment_df,
      metaoutcome = metaoutcome,
      OpenDataTable = OpenDataTable
    )

    outcome_measure = analysis_options_reactives$outcome_measure
    model_effects = analysis_options_reactives$model_effects
    initial_data = analysis_options_reactives$initial_data
    sensitivity_data = analysis_options_reactives$sensitivity_data
    sensitivity_treatment_list = analysis_options_reactives$sensitivity_treatment_list
    rank_option = analysis_options_reactives$rank_option
    continuous_outcome = analysis_options_reactives$continuous_outcome
    binary_outcome = analysis_options_reactives$binary_outcome

    #####
    # Reactive functions used in various places, based on the data
    #####
    
    # Initial (uploaded) data with covariates removed
    initial_non_covariate_data <- reactive({
      RemoveCovariates(initial_data())
    })
    
    # Sensitivity analysis data with covariates removed
    sensitivity_non_covariate_data <- reactive({
      RemoveCovariates(sensitivity_data())
    })
    
    # Make ref_alter function (in analysis_generic.R) reactive
    reference_alter <- reactive({
      return(
        list(
          ref_all = treatment_df()$Label[treatment_df()$Number == 1],
          ref_sub = sensitivity_treatment_list()$Label[sensitivity_treatment_list()$Number == 1]
        )
      )
    })

    # Make frequentist function (in fn_analysis.R) reactive
    freq_all <- reactive({
      return(
        frequentist(
          initial_non_covariate_data(),
          metaoutcome(),
          treatment_df(),
          outcome_measure(),
          model_effects(),
          reference_alter()$ref_all
        )
      )
    })

    # Make frequentist function (in fn_analysis.R) reactive with excluded studies
    freq_sub <- reactive({
      return(
        frequentist(
          sensitivity_non_covariate_data(),
          metaoutcome(),
          sensitivity_treatment_list(),
          outcome_measure(),
          model_effects(),
          reference_alter()$ref_sub
        )
      )
    })
    
    # Make bugsnetdata function (in analysis_generic.R) reactive
    bugsnetdt <- reactive({
      return(bugsnetdata(initial_non_covariate_data(), metaoutcome(), treatment_df()))
    })
    
    # Make bugsnetdata function (in analysis_generic.R) reactive
    bugsnetdt_sub <- reactive({
      return(bugsnetdata(sensitivity_non_covariate_data(), metaoutcome(), sensitivity_treatment_list()))
    })
    
    ### Get data for data table
    
    
    filtertable <- function() {
      label <- treatment_df()
      dt <- initial_non_covariate_data()
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

    output$datatb <- DT::renderDataTable(
      DT::datatable(
        filtertable(),
        editable = TRUE,
        rownames = FALSE,
        colnames = colnames(),
        filter = list(position = 'top', clear = FALSE, stateSave = TRUE)
      )
    )
    
    #######################
    ### 1. Data Summary ###
    #######################

    data_summary_panel_server(
      id = "data_summary",
      metaoutcome = metaoutcome,
      outcome_measure = outcome_measure,
      bugsnetdt = bugsnetdt,
      bugsnetdt_sub = bugsnetdt_sub,
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
      rank_option = rank_option,
      freq_all = freq_all,
      freq_sub = freq_sub,
      bugsnetdt = bugsnetdt,
      bugsnetdt_sub = bugsnetdt_sub,
      reference_alter = reference_alter
    )


    #####################
    #### 3. Bayesian ####
    #####################
    
    bayesian_analysis_panel_server(
      id = "bayesian_analysis",
      data = initial_data,
      sensitivity_data = sensitivity_data,
      treatment_df = treatment_df,
      sensitivity_treatment_df = sensitivity_treatment_list,
      metaoutcome = metaoutcome,
      outcome_measure = outcome_measure,
      continuous_outcome = continuous_outcome,
      binary_outcome = binary_outcome,
      model_effects = model_effects,
      rank_option = rank_option,
      freq_all = freq_all,
      freq_sub = freq_sub,
      bugsnetdt = bugsnetdt,
      bugsnetdt_sub = bugsnetdt_sub,
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