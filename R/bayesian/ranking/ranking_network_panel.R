
#' Module UI for the ranking network panel.
#' 
#' @param id ID of the module.
#' @return Div for the panel.
ranking_network_panel_ui <- function(id) {
  ns <- NS(id)
  div(
    h4("Summary of evidence"),
    shinycssloaders::withSpinner(
      plotOutput(outputId = ns("netGraphStatic1_rank")),
      type = 6
    ),
    conditionalPanel(
      condition = "input.networkstyle_rank == 'networkp1'",
      ns = ns,
      p("Numbers on the line indicate number of trials conducted for the comparison. Any shaded areas indicate existence of multi-arm trials between the comparisons.")
    ),
    conditionalPanel(
      condition = "input.networkstyle_rank == 'networkp2'",
      ns = ns,
      p("The size of the nodes and thickness of edges represent the number of studies that examined a treatment and compared two given treatments respectively.")
    ),
    radioButtons(
      inputId = ns("networkstyle_rank"),
      label = "Please choose a network plot style",
      choices = c(
        "Number of trials shown on the line" = "networkp1",
        "Number of trials indicated by node size and line thickness" = "networkp2"
      ),
      inline = TRUE
    ),
    radioButtons(
      inputId = ns('network_rank_choice'),
      label = 'Document format',
      choices = c(
        'PDF' = 'pdf',
        'PNG' = 'png'
      ),
      inline = TRUE
    ),
    downloadButton(outputId = ns('download_network_rank'))
  )
}


#' Module server for the ranking network panel.
#' 
#' @param id ID of the module
#' @param treat_order Reactive containing treatments ordered by SUCRA
#' @param frequentist_react Reactive containing frequentist meta-analysis
#' @param bugsnetdt_react Reactive containing bugsnet meta-analysis
#' @param filename_prefix Prefix to add before file names.
#' @param title_prefix Prefix to add beofre plot titles.
ranking_network_panel_server <- function(
    id,
    treat_order,
    frequentist_react,
    bugsnetdt_react,
    filename_prefix,
    title_prefix
    ) {
  moduleServer(id, function(input, output, session) {
    
    output$netGraphStatic1_rank <- renderPlot({
      if (input$networkstyle_rank == 'networkp1') {
        # Number of trials on line
        make_netgraph_rank(frequentist_react(), treat_order())
      } else {
        # Number of trials by nodesize and line thickness
        make_netplot(bugsnetdt_react(), order = list(order = treat_order()))
      }
      title(paste0(title_prefix, ": Network plot of all studies"))
    })

    output$download_network_rank <- downloadHandler(
      filename = function() {
        paste0(filename_prefix, 'Network.', input$network_rank_choice)
      },
      content = function(file) {
        draw_network_rank <- function() {
          if (input$networkstyle_rank == 'networkp1') {
            make_netgraph_rank(frequentist_react(), treat_order())
          } else {
            make_netplot(bugsnetdt_react(), order = list(order = treat_order()))
          }
          title(paste0(title_prefix, ": Network plot of all studies"))
        }
        write_plot(
          file,
          input$network_rank_choice,
          draw_network_rank
        )
      }
    )
  })
}