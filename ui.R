###### MetaInsight ######

dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(
      shinyjs::useShinyjs(),
      # load custom stylesheet
      # To ensure white background and no horizontal scroll bars on ranking panel
      tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),
      includeHTML("www/favicon/favicon.html"),
      tags$meta(name="description", content="A interactive web tool for network meta-analysis (NMA) that leverages established analysis routines"),
      tags$meta(name="keywords", content="MetaInsight, NMA, Network, Meta, Analysis, App"),
      tags$meta(property="og:title", content="Meta Insight: v5.2.1"),
      tags$meta(property="og:description", content="An interactive web tool for network meta-analysis (NMA) that leverages established analysis routines"),
      tags$meta(property="og:image", content="https://raw.githubusercontent.com/CRSU-Apps/MetaInsight/main/www/images/MetaInsightLogo.png"),
      google_analytics_header_ui(id = "analytics")
    ),
    navbarPage(
      id = "top_bar",
      title = "MetaInsight",
      tabPanel(
        title = "Home",
        home_page_ui(id = "home")
      ),
      tabPanel(
        title = "Load Data",
        load_data_page_ui(id = 'load_data_page')
      ),
      tabPanel(
        title = "Data analysis",
        data_analysis_page_ui(id = "data_analysis")
      ),
      tabPanel(
        title = "User Guide",
        user_guide_page_ui(id = "user_guide")
      ),
      tabPanel(
        title = "Troubleshooting",
        # Script to open the troubleshooting wiki page in a new tab when the "Troubleshooting" tab is selected
        tags$head(
          tags$script(
            HTML(
              "$(document).on('shiny:inputchanged', function(event) {
                if (event.name == 'top_bar' && event.value == 'Troubleshooting') {
                  window.open('https://github.com/CRSU-Apps/MetaInsight/wiki/Troubleshooting', '_blank');
                }
              });"
            )
          )
        )
      )
    )
  )
)
