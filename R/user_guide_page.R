
#' Module UI for the user guide page.
#' 
#' @param id ID of the module
#' @return Div for the user guide page
user_guide_page_ui <- function(id) {
  ns <- NS(id)
  div(
    h2(tags$strong("User Guide")),
    h4(tags$strong("User Guide", style = "color: #2196c4")),
    p("Click the button below to download a pdf copy of the MetaInsight User Guide."),
    p(
      tags$strong("Please note:"), 
      "the user guide is based on version 3 of MetaInsight. Some elements of the app have been changed since the guide was originally produced."
    ),
    downloadButton(outputId = ns("UG"), label = "Download User Guide"),
    br(),
    br(),
    h4(tags$strong("ESMARConf 2023 Tutorial", style = "color: #2196c4")),
    p(tags$strong(" MetaInsight - an R Shiny web-app for conducting network meta-analysis")),
    p("A tutorial for MetaInsight v4.0.0 produced for ESMARConf 2023"),
    HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/g-RDnQ75Hv4" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>'),
    br(),
    br(),
    h4(tags$strong("Treatment Ranking Demo", style = "color: #2196c4")),
    p("A short demo video of how to use the Bayesian analysis ranking panel in tab 3c"),
    HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/scbLwTY0kvc" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
    br(),
    br(),
    h4(tags$strong("Cochrane Training Webinar", style = "color: #2196c4")),
    p(tags$strong("MetaInsight: Background, introduction, demonstration, limitations, and future plans")),
    p(
      "These videos were recorded live in 2019 as part of the ",
      tags$a(href="https://training.cochrane.org/resource/metainsight-complex-review-support-unit-crsu-network-meta-analysis-nma-web-based-app", "Cochrane Training network meta-analysis learning live webinar series.",target="_blank"),
      "They are intended for people who are interested in undertaking a network meta-analysis using MetaInsight.",
      tags$strong("Please note:"), "these videos refer to a past version of MetaInsight and elements of the app have changed since the videos were originally recorded."
    ),
    br(),
    HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/RR_tkICQv_s" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
    HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/b-fYoUdksRo" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
    HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/g0n5yxQ4Z34" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
  )
}


#' Module server for the user guide page.
#' 
#' @param id ID of the module
user_guide_page_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$UG <- downloadHandler(
      filename = "MetaInsightUserGuide.pdf",
      content = function(file) {
        file.copy("www/MetaInsightUserGuide.pdf", file)
      }
    )
  })
}