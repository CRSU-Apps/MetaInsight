
#' Module UI for the data summary panel.
#' 
#' @param id ID of the module
#' @return Div for the panel
data_summary_panel_ui <- function(id) {
  ns <- NS(id)
  div(
    tabsetPanel(
      tabPanel(
        title = "1a. Data Characteristics",
        p("This tab shows a summary of study characteristics."),
        column(
          width = 6,
          h4("Characteristics table of all studies"),
          tableOutput(outputId = ns("sumtb"))
        ),
        column(
          width = 6,
          h4("Characteristics table with studies excluded"),
          tableOutput(outputId = ns("sumtb_sub"))
        )
      ),
      tabPanel(
        title = "1b. Study Results",
        p("If the formatting of the text for this plot needs adjusting please see options at the bottom."),
        plotOutput(outputId = ns("forestPlot"), height = "1000px", width = "800px"),
        h5("If the formatting of the text in the above plot needs adjusting (for on screen or download) please use the following options:"),
        column(
          width = 4,
          numericInput(
            inputId = ns("ForestContent"),
            label="Download text size:",
            value=12
          )
        ),
        column(
          width = 4,
          numericInput(inputId = ns("ForestTitle"), label = "Title text size:", value = 1)
        ),
        column(
          width = 4,
          numericInput(inputId = ns("ForestHeader"), label = "Group headers text size:", value = 1)
        ),
        helpText("The download text size alters the text sizing for all elements of the plot and is integer point sizes. The title and group header options are an additional text sizing option in terms of percentage (e.g. 0.5 indicates half the specified size)."),
        p("Please note: the formatting is not guaranteed to be identical between what is shown on screen and what is downloaded."),
        radioButtons(
          inputId = ns('format_freq0'),
          label = 'Document format',
          choices = c('PDF', 'SVG'),
          inline = TRUE
        ),
        downloadButton(outputId = ns('downloadStudy'))
      ),
      tabPanel(
        title = "1c. Network Plot",
        column(
          width = 6,
          plotOutput(outputId = ns("netGraphStatic1")),
          conditionalPanel(
            condition = "input.networkstyle == 'networkp1'",
            ns = ns,
            p("Numbers on the line indicate the number of trials conducted for the comparison. The shaded areas indicate there exist multi-arm trials between the comparisons.")
          ),
          conditionalPanel(
            condition= "input.networkstyle == 'networkp2'",
            ns = ns,
            p("The size of the nodes and thickness of edges represent the number of studies that examined a treatment and compared two given treatments respectively.")
          ),
          radioButtons(
            inputId = ns("networkstyle"),
            label = "Please choose a network plot style",
            choices = c(
              "Number of trials shown on the line" = "networkp1",
              "Number of trials indicated by node size and line thickness" = "networkp2"
            ),
            selected = "networkp2",
            width='100%'
          ),
          column(
            width = 6,
            radioButtons(
              inputId = ns('format_freq1'),
              label = 'Document format',
              choices = c('PDF', 'PNG'),
              inline = TRUE
            )
          ),
          column(
            width = 6,
            numericInput(inputId = ns('label_all'), label = 'Label size multiplier', value = 1.25)
          ),
          downloadButton(outputId = ns('downloadNetwork')),
          br(),
          br(),
          br(),
          verbatimTextOutput(outputId = ns("netconnect"))
        ),
        column(
          width = 6,
          plotOutput(outputId = ns("netGraphUpdating")),
          conditionalPanel(
            condition = "input.networkstyle_sub == 'networkp1'",
            ns = ns,
            p("Numbers on the line indicate the number of trials conducted for the comparison. The shaded areas indicate there exist multi-arm trials between the comparisons.")
          ),
          conditionalPanel(
            condition = "input.networkstyle_sub == 'networkp2'",
            ns = ns,
            p("The size of the nodes and thickness of edges represent the number of studies that examined a treatment and compared two given treatments respectively.")
          ),
          radioButtons(
            inputId = ns("networkstyle_sub"),
            label = "Please choose a network plot style",
            choices = c(
              "Number of trials shown on the line" = "networkp1",
              "Number of trials indicated by node size and line thickness" = "networkp2"
            ),
            selected = "networkp2",
            width = '100%'
          ),
          column(
            width = 6,
            radioButtons(
              inputId = ns('format_freq2'),
              label = 'Document format',
              choices = c('PDF', 'PNG'),
              inline = TRUE
            )
          ),
          column(
            width = 6,
            numericInput( inputId = ns('label_excluded'), label = 'Label size multiplier', value = 1.25)
          ),
          downloadButton(outputId = ns('downloadNetworkUpdate')),
          br(),
          br(),
          br(),
          verbatimTextOutput("netconnect_sub")
        )
      )
    )
  )
}


#' Module server for the data summary panel.
#' 
#' @param id ID of the module
#' @param data Reactive containing data to analyse
#' @param is_default_data Reactive containing TRUE if data is an example dataset, loaded by default
#' @param treatment_df Reactive containing data frame containing treatment IDs (Number) and names (Label)
#' @param metaoutcome Reactive containing meta analysis outcome: "continuous" or "binary"
data_summary_panel_server <- function(id, metaoutcome, outcome_measure, exclusions, bugsnetdt, freq_sub, freq_all) {
  moduleServer(id, function(input, output, session) {
    
    # 1a. Data Characteristics

    # Characteristics table of all studies
    output$sumtb <- renderTable({
      summary_table_plot(bugsnetdt(), metaoutcome())
    })

    # Characteristics table with studies excluded
    output$sumtb_sub <- renderTable({
      summary_table_plot(filter(bugsnetdt(), !Study %in% exclusions()), metaoutcome())
    })

    # 1b. Study Results

    # Forest plot
    output$forestPlot <- renderPlot({
      make_netStudy(freq_sub(), outcome_measure(), input$ForestHeader, input$ForestTitle)$fplot
    })

    output$downloadStudy <- downloadHandler(
      filename = function() {
        paste0('StudyResults.', input$format_freq0)
      },
      content = function(file) {
        if (input$format_freq0 == "PDF") {
          pdf(file = file, pointsize = input$ForestContent, width = 8, height = make_netStudy(freq_sub(), outcome_measure(), input$ForestHeader, input$ForestTitle)$size)
        } else {
          svg(file = file, pointsize = input$ForestContent, width = 8, height = make_netStudy(freq_sub(), outcome_measure(), input$ForestHeader, input$ForestTitle)$size)
        }
        make_netStudy(freq_sub(), outcome_measure(), input$ForestHeader, input$ForestTitle)
        dev.off()
      }
    )

    # 1c. Network Plot

    # Network plot of all studies
    output$netGraphStatic1 <- renderPlot({
      if (input$networkstyle=='networkp1') {
        # Number of trials on line
        make_netgraph(freq_all(),input$label_all)
      } else {
        # Number of trials by nodesize and line thickness
        make_netplot(bugsnetdt(), input$label_all)
      }
      title("Network plot of all studies")
    })

    # Network connectivity all studies
    output$netconnect <- renderPrint ({
      make_netconnect(freq_all())
    })

    # Network plot with studies excluded
    output$netGraphUpdating <- renderPlot({
      if (input$networkstyle_sub=='networkp1') {
        # Number of trials on line
        make_netgraph(freq_sub(),input$label_excluded)
      } else {
        # Number of trials by nodesize and line thickness
        make_netplot(filter(bugsnetdt(), !Study %in% exclusions()), input$label_excluded)
      }
      title("Network plot with studies excluded")
    })

    # Network connectivity with studies excluded
    output$netconnect_sub <- renderPrint ({
      make_netconnect(freq_sub())
    })

    output$downloadNetwork <- downloadHandler(
      filename = function() {
        paste0('Network.', input$format_freq1)
      },
      content = function(file) {
        draw_network <- function() {
          if (input$networkstyle == 'networkp1') {
            make_netgraph(freq_all(), input$label_all)
          } else {
            data.rh <- data.prep(arm.data = bugsnetdt(), varname.t = "T", varname.s = "Study")
            net.plot(data.rh, node.scale = 3, edge.scale = 1.5, node.lab.cex = input$label_all)
          }
          title("Network plot of all studies")
        }
        write_to_pdf_or_png(
          file,
          input$format_freq1,
          draw_network
        )
      }
    )

    output$downloadNetworkUpdate <- downloadHandler(
      filename = function() {
        paste0('Network_sen.', input$format_freq2)
      },
      content = function(file) {
        draw_network <- function() {
          if (input$networkstyle_sub == 'networkp1') {
            make_netgraph(freq_sub(), input$label_excluded)
          } else {
            long_sort2_sub <- filter(bugsnetdt(), !Study %in% exclusions())  # subgroup
            data.rh <- data.prep(arm.data = long_sort2_sub, varname.t = "T", varname.s = "Study")
            net.plot(data.rh, node.scale = 3, edge.scale=1.5, node.lab.cex = input$label_excluded)
          }
          title("Network plot with studies excluded")
        }
        write_to_pdf_or_png(
          file,
          input$format_freq2,
          draw_network
        )
      }
    )


    ############### bugsnet code #################

    ### (notification on disconnection)
    disconnect <- function(){
      showModal(
        modalDialog(
          title = "Disconnected network",
          easyClose = FALSE,
          p(
            tags$strong(
              "Please note that the network of sensitivity analysis is disconnected. Two or more networks exist. The disconnected networks are displayed at 'Data analysis' - '1c. Network Plot' tab - 'Network plot with studies excluded'.
              Please continue excluding studies until only one network remains, or adding studies back until the network is re-connected, as appropriate."
            )
          ),
          br()
        )
      )
    }

    # Notify user if sensitvity analysis produces a disconnected network
    observeEvent(exclusions(),{
      longsort2 <- bugsnetdt()
      longsort2_sub <- filter(bugsnetdt(), !Study %in% exclusions())  # subgroup
      sumtb_sub <- bugsnet_sumtb(longsort2_sub, metaoutcome())
      if (sumtb_sub$Value[6] == "FALSE") {
        disconnect()
      }
    })
  })
}