###### Bayesian V4 ######

#if packages not installed, please install them first by running the line below.
#install.packages(c("dplyr","metafor", "netmeta","shiny", "shinyAce","rmarkdown", "knitr", "gemtc", "plyr", data.table"
#  , "shinyalert", "plotly"))

# the data for meta-regression is from: http://nicedsu.org.uk/wp-content/uploads/2016/03/TSD3-Heterogeneity.final-report.08.05.12.pdf


shinyServer(function(input, output, session) {
  
  ### GDPR
  
  showModal(
    modalDialog(
      title = "Important message",
      easyClose = FALSE,
      p(
        tags$strong("In accordance with Data Protection legislation, we would like to inform you of the following before you use our website:"),
        "We collect your usage data within the MetaInsight app to perform analytics of usage and improve our app. By clicking",
        tags$i(tags$u("I consent")),
        "below, you consent to the use of data by us through Google Analytics. For details of policy, please check the 'Privacy notice' tab within the app, and ",
        tags$a(href="https://policies.google.com/privacy?hl=en", "Google Privacy & Terms.",target="_blank")
      ),
      br(),
      modalButton("I consent"),
      footer = NULL
    )
  )
  
  #####
  # Reactive functions used in various places
  #####
  
  # Define outcome measure (continuous or binary) - NVB
  metaoutcome <- home_page_server(id = "home")
  
  data_reactives <- load_data_page_server(
    id = 'load_data_page',
    metaoutcome = metaoutcome
  )
  data <- data_reactives$data
  is_default_data = data_reactives$is_default_data
  treatment_df <- data_reactives$treatment_df
  
  data_analysis_page_server(
    id = "data_analysis",
    data = data,
    is_default_data = is_default_data,
    treatment_df = treatment_df,
    metaoutcome = metaoutcome
  )
  
  user_guide_page_server(id = "user_guide")
  
  
  meta_regression_tab_server(
    id = "meta_regression",
    all_data = data
  )
  
  MetaRegressionTabServer(
    id = "meta_regression",
    all_data = data
  )
  
    
  shiny::observeEvent(
    data,
    {
      # Check if any covariates in data
      # if (length(FindCovariateNames(data)) == 0) {
      shiny::hideTab(
        inputId = "main_tabs",
        target = "4. Meta-regression"
      )
      # } else {
      #   shiny::showTab(
      #     inputId = "main_tabs",
      #     target = "4. Meta-regression"
      #   )
      # }
    }
  })
})
