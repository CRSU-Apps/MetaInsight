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
      tags$meta(property="og:title", content="Meta Insight: v5.0.1"),
      tags$meta(property="og:description", content="An interactive web tool for network meta-analysis (NMA) that leverages established analysis routines"),
      tags$meta(property="og:image", content="https://raw.githubusercontent.com/CRSU-Apps/MetaInsight/main/www/images/MetaInsightLogo.png"),
      tags$script(src = "https://kit.fontawesome.com/23f0e167ac.js", crossorigin = "anonymous")
    ),
    navbarPage(
      title = "MetaInsight", 
      header = singleton(tags$head(includeScript("google_analytics2.js"))),
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
), width=9))),

###################################
### Tab 4 - User Guide ###
###################################

tabPanel("User Guide",
         h2 (tags$strong("User Guide")),
         h4 (tags$strong("User Guide", style = "color: #2196c4")),
                            p("Click the button below to download a pdf copy of the MetaInsight User Guide."),
                            p(tags$strong("Please note:"), 
                              "the user guide is based on version 3 of MetaInsight. Some elements of the app have been changed since the guide was originally produced."),
                            downloadButton("UG", "Download User Guide"),
                            br(),
                            br(),
         h4 (tags$strong("ESMARConf 2023 Tutorial", style = "color: #2196c4")),
         p(tags$strong(" MetaInsight - an R Shiny web-app for conducting network meta-analysis")),
         p("A tutorial for MetaInsight v4.0.0 produced for ESMARConf 2023"),
         HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/g-RDnQ75Hv4" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>'),
         br(),
         br(),
         h4 (tags$strong("Treatment Ranking Demo", style = "color: #2196c4")),
         p("A short demo video of how to use the Bayesian analysis ranking panel in tab 3c"),
         HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/scbLwTY0kvc" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
         br(),
         br(),
         h4 (tags$strong("Cochrane Training Webinar", style = "color: #2196c4")),
         p(tags$strong("MetaInsight: Background, introduction, demonstration, limitations, and future plans")),
         p("These videos were recorded live in 2019 as part of the ",
           tags$a(href="https://training.cochrane.org/resource/metainsight-complex-review-support-unit-crsu-network-meta-analysis-nma-web-based-app", "Cochrane Training network meta-analysis learning live webinar series.",target="_blank"),
           "They are intended for people who are interested in undertaking a network meta-analysis using MetaInsight.",
           tags$strong("Please note:"), "these videos refer to a past version of MetaInsight and elements of the app have changed since the videos were originally recorded."),
         br(),
         HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/RR_tkICQv_s" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
         HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/b-fYoUdksRo" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
         HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/g0n5yxQ4Z34" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
         )
,


###################################
### Tab 5 - Troublehshooting.   ###
###################################

tabPanel(id="trouble", "Troubleshooting",
#          tags$div(
# includeHTML("troublesh.html")
#          )
tags$iframe(style = "height:1500px; width:100%; scrolling=yes",
            src = "trouble_shooting.pdf")
),

##############################
### Tab 6 - Privacy notice ###
##############################



tabPanel(id="privacy", "Privacy notice",
   tags$iframe(style="height:1500px; width:100%; scrolling=yes",
               src="gdpr.pdf")
)
