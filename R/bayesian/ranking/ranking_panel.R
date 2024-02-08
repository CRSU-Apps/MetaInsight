
#' Module UI for the ranking panel.
#' 
#' @param id ID of the module.
#' @param title Panel title.
#' @param table_label Label for the drop-down to show ranking values and SUCRA values.
#' @return Div for the panel.
ranking_panel_ui <- function(id, title, table_label) {
  ns <- NS(id)
  shinydashboard::box(
    title = title,
    status = 'primary',
    solidHeader = TRUE,
    width = 12,
    collapsible = TRUE,
    splitLayout(
      cellWidths = c("30%", "40%", "30%"),
      cellArgs = list(style = "height: 780px; padding: 16px; border: 2px solid gold; white-space: normal"),
      fluidRow(
        align = "center",
        ranking_forest_panel_ui(id = ns("forest"))
      ),
      fluidRow(
        align = "center",
        rankogram_panel_ui(id = ns("rankogram"), table_label = table_label)
      ),
      fluidRow(
        align = "center",
        ranking_network_panel_ui(id = ns("network"))
      )
    )
  )
}


#' Module server for the ranking panel.
#' 
#' @param id ID of the module
#' @param data Reactive containing data to analyse
#' @param treatment_df Reactive containing data frame containing treatment IDs (Number) and names (Label)
#' @param model Reactive containing bayesian meta-analysis for all studies
#' @param metaoutcome Reactive containing meta analysis outcome: "Continuous" or "Binary"
#' @param rank_option Reactive containing ranking option: "good" or "bad" depending on whether small values are desirable or not
#' @param frequentist Reactive containing frequentist meta-analysis
#' @param bugsnetdt Reactive containing bugsnet meta-analysis
#' @param filename_prefix Text to prefix the filename of all the downloads]
#' @param title_prefix Text to prefix the title of plots
#' @param cov_value Value of covariate for regression analysis
#' @param exclusions Reactive containing names of studies excluded from the sensitivity analysis
ranking_panel_server <- function(
    id,
    data,
    treatment_df,
    model,
    metaoutcome,
    rank_option,
    frequentist,
    bugsnetdt,
    filename_prefix,
    title_prefix,
    cov_value = reactive({NA}),
    exclusions = reactive({ c() })
    ) {
  moduleServer(id, function(input, output, session) {
    
    ranking_data <- eventReactive(model(), {
      r_data <- obtain_rank_data(data(), metaoutcome(), treatment_df(), model(), rank_option(), cov_value(), exclusions())
      return(r_data)
    })

    # Network plots for ranking panel (Bayesian) (they have slightly different formatting to those on tab1) CRN
    treat_order <- reactive({
      ranking_data()$SUCRA[order(ranking_data()$SUCRA$SUCRA), 1]
    }) # obtain treatments ordered by SUCRA #
    
    frequentist_react <- eventReactive(model(), {
      # These two lines are needed in case someone jumped to Bayesian page without running frequentist section, but am aware this can cause frequentist analysis to run twice (CRN)
      frequentist()
    })
    
    bugsnetdt_react <- eventReactive(model(), {
      bugsnetdt()
    })
    
    ranking_forest_panel_server(
      id = "forest",
      model = model,
      treat_order = treat_order,
      frequentist_react = frequentist_react,
      bugsnetdt_react = bugsnetdt_react,
      filename_prefix = filename_prefix,
      title_prefix = title_prefix
    )
    
    regression_text <- reactive({
      if (is.na(cov_value()) == FALSE) {
        return(model()$cov_value_sentence)
      } else {
        return("")
      }
    })
    
    rankogram_panel_server(
      id = "rankogram",
      ranking_data = ranking_data,
      filename_prefix = filename_prefix,
      regression_text = regression_text
    )
    
    ranking_network_panel_server(
      id = "network",
      treat_order = treat_order,
      frequentist_react = frequentist_react,
      bugsnetdt_react = bugsnetdt_react,
      filename_prefix = filename_prefix,
      title_prefix = title_prefix
    )
  })
}



#' Module server for the baseline risk ranking panel.
#' 
#' @param id ID of the module
#' @param data Reactive containing data to analyse
#' @param treatment_df Reactive containing data frame containing treatment IDs (Number) and names (Label)
#' @param model Reactive containing bayesian meta-analysis for all studies
#' @param metaoutcome Reactive containing meta analysis outcome: "Continuous" or "Binary"
#' @param rank_option Reactive containing ranking option: "good" or "bad" depending on whether small values are desirable or not
#' @param bugsnetdt Reactive containing bugsnet meta-analysis
#' @param filename_prefix Text to prefix the filename of all the downloads
#' @param title_prefix Text to prefix the title of plots
#' @param exclusions Reactive containing names of studies excluded from the sensitivity analysis
ranking_panel_baseline_risk_server <- function(
    id,
    data,
    treatment_df,
    model,
    metaoutcome,
    rank_option,
    frequentist,
    bugsnetdt,
    filename_prefix,
    title_prefix,
    exclusions = reactive({ c() })
) {
  moduleServer(id, function(input, output, session) {
    
    ranking_data <- eventReactive(model(), {
      r_data <- obtain_rank_data(data(), metaoutcome(), treatment_df(), model(), rank_option(), cov_value(), exclusions(),
                                 package = "bnma")
      return(r_data)
    })
    
    # Network plots for ranking panel (Bayesian) (they have slightly different formatting to those on tab1) CRN
    treat_order <- reactive({
      ranking_data()$SUCRA[order(ranking_data()$SUCRA$SUCRA), 1]
    }) # obtain treatments ordered by SUCRA #
    
    frequentist_react <- eventReactive(model(), {
      # These two lines are needed in case someone jumped to Bayesian page without running frequentist section, but am aware this can cause frequentist analysis to run twice (CRN)
      frequentist()
    })

    bugsnetdt_react <- eventReactive(model(), {
      bugsnetdt()
    })
    
    ranking_forest_panel_baseline_risk_server(
      id = "forest",
      model = model,
      treat_order = treat_order,
      bugsnetdt_react = bugsnetdt_react,
      filename_prefix = filename_prefix,
      title_prefix = title_prefix
    )
    
    regression_text <- reactive({
        return("")
    })
    
    rankogram_panel_server(
      id = "rankogram",
      ranking_data = ranking_data,
      filename_prefix = filename_prefix,
      regression_text = regression_text
    )
    
    ranking_network_panel_server(
      id = "network",
      treat_order = treat_order,
      frequentist_react = frequentist_react,
      bugsnetdt_react = bugsnetdt_react,
      filename_prefix = filename_prefix,
      title_prefix = title_prefix
    )
  })
}