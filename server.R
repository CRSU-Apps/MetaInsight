###### Bayesian V4 ######

#if packages not installed, please install them first by running the line below.
#install.packages(c("dplyr","metafor", "netmeta","shiny", "shinyAce","rmarkdown", "knitr", "gemtc", "plyr", data.table"
#  , "shinyalert", "plotly"))

# the data for meta-regression is from: http://nicedsu.org.uk/wp-content/uploads/2016/03/TSD3-Heterogeneity.final-report.08.05.12.pdf


shinyServer(function(input, output, session) {
  
  # Current tab as a reactive
  tab <- reactive(input$top_bar)
  
  # GDPR Module Server (R/analytics_header.R)
  GdprServer(
    id = "cookies",
    cookies = reactive(input$cookies),
    google_analytics_id = "G-H3241DM66M",
    tab = tab
  )
  
  #####
  # Reactive functions used in various places
  #####
  
  home_page_server(id = "home")
  
  data_reactives <- load_data_page_server(id = 'load_data_page')
  data <- data_reactives$data
  is_default_data = data_reactives$is_default_data
  treatment_df <- data_reactives$treatment_df
  metaoutcome <- data_reactives$metaoutcome

  data_analysis_page_server(
    id = "data_analysis",
    data = data,
    is_default_data = is_default_data,
    treatment_df = treatment_df,
    metaoutcome = metaoutcome
  )

  user_guide_page_server(id = "user_guide")

  # Reset the top bar to show the previously selected tab when the "Troubleshooting" tab is selected
  top_bar_selection <- reactiveVal()
  observe({
    if (input$top_bar != "Troubleshooting") {
      top_bar_selection(input$top_bar)
    } else {
      shiny::updateNavbarPage(inputId = "top_bar", selected = top_bar_selection())
    }
  })
  
})
