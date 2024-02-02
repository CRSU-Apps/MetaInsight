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
      tags$meta(property="og:title", content="Meta Insight: v5.1.2"),
      tags$meta(property="og:description", content="An interactive web tool for network meta-analysis (NMA) that leverages established analysis routines"),
      tags$meta(property="og:image", content="https://raw.githubusercontent.com/CRSU-Apps/MetaInsight/main/www/images/MetaInsightLogo.png"),
      tags$script(src = "https://kit.fontawesome.com/23f0e167ac.js", crossorigin = "anonymous"),
      google_analytics_header_ui(id = "analytics")
    ),
    navbarPage(
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
        tags$iframe(style = "height:1500px; width:100%; scrolling=yes",
                    src = "trouble_shooting.pdf")
      )
    )
  )
)