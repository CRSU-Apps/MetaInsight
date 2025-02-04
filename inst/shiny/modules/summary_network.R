summary_network_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    radioButtons(ns("networkstyle"),
      label = "Network plot style",
      choices = c(
        "Number of trials shown on the line" = "networkp1",
        "Number of trials indicated by node size and line thickness" = "networkp2"
      ),
      selected = "networkp2"),
    actionButton(ns("run"), "Run module summary_network")
  )
}

summary_network_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  observeEvent(list(input$run, gargoyle::watch("exclude")), {
    # WARNING ####

    # FUNCTION CALL ####

    # LOAD INTO COMMON ####

    # METADATA ####

    # TRIGGER
    gargoyle::trigger("summary_network")
  })


  # Network plot of all studies
  output$netGraphStatic1 <- renderPlot({
    gargoyle::watch("summary_network")
    if (input$networkstyle == 'networkp1') {
      # Number of trials on line
      req(common$freq_all)
      browser()
      make_netgraph(common$freq_all, input$label_all)
    } else {
      # Number of trials by nodesize and line thickness
      req(common$bugsnetdt)
      make_netplot(common$bugsnetdt, input$label_all)
    }
    title("Network plot of all studies")
  })

  # Network connectivity all studies
  output$netconnect <- renderPrint({
    gargoyle::watch("summary_network")
    req(common$freq_all)
    make_netconnect(common$freq_all)
  })

  # Network plot with studies excluded
  output$netGraphUpdating <- renderPlot({
    gargoyle::watch("summary_network")
    if (input$networkstyle == 'networkp1') {
      # Number of trials on line
      req(common$freq_sub)
      make_netgraph(common$freq_sub, input$label_excluded)
    } else {
      # Number of trials by nodesize and line thickness
      req(common$bugsnetdt_sub)
      make_netplot(common$bugsnetdt_sub, input$label_excluded)
    }
    title("Network plot with selected studies excluded")
  })

  # Network connectivity with studies excluded
  output$netconnect_sub <- renderPrint({
    gargoyle::watch("summary_network")
    req(common$freq_sub)
    make_netconnect(common$freq_sub)
  })


  output$downloadNetwork <- downloadHandler(
    filename = function() {
      paste0('Network.', input$format_freq1)
    },
    content = function(file) {
      draw_network <- function() {
        if (input$networkstyle == 'networkp1') {
          make_netgraph(common$freq_all, input$label_all)
        } else {
          data.rh <- data.prep(arm.data = common$bugsnetdt, varname.t = "T", varname.s = "Study")
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
          make_netgraph(common$freq_sub, input$label_excluded)
        } else {
          long_sort2_sub <- common$bugsnetdt_sub  # subgroup
          data.rh <- data.prep(arm.data = long_sort2_sub, varname.t = "T", varname.s = "Study")
          net.plot(data.rh, node.scale = 3, edge.scale=1.5, node.lab.cex = input$label_excluded)
        }
        title("Network plot with selected studies excluded")
      }
      write_to_pdf_or_png(
        file,
        input$format_freq2,
        draw_network
      )
    }
  )

  return(list(
    save = function() {
      # Save any values that should be saved when the current session is saved
    },
    load = function(state) {
      # Load
    }
  ))
})
}

summary_network_module_result <- function(id) {
  ns <- NS(id)
  fluidRow(
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
    column(
      width = 6,
      plotOutput(ns("netGraphStatic1")),
      radioButtons(ns('format_freq1'), label = 'Document format', choices = c('PDF', 'PNG'), inline = TRUE),
      numericInput(ns('label_all'), label = 'Label size multiplier', value = 1.2, step = 0.1),
      downloadButton(ns('downloadNetwork')),
      br(),
      br(),
      br(),
      verbatimTextOutput(ns("netconnect"))
    ),
    column(
      width = 6,
      plotOutput(ns("netGraphUpdating")),
      radioButtons(ns('format_freq2'), label = 'Document format', choices = c('PDF', 'PNG'), inline = TRUE),
      numericInput(ns('label_excluded'), label = 'Label size multiplier', value = 1.25, step = 0.1),
      downloadButton(outputId = ns('downloadNetworkUpdate')),
      br(),
      br(),
      br(),
      verbatimTextOutput(outputId = ns("netconnect_sub"))
    )
  )
}

summary_network_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
}

