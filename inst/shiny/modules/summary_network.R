summary_network_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    radioButtons(ns("style"),
      label = "Network plot style",
      choices = c(
        "Number of trials shown on the line" = "netgraph",
        "Number of trials indicated by node size and line thickness" = "netplot"
      ),
      selected = "netplot"),
    actionButton(ns("run"), "Run module summary_network")
  )
}

summary_network_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  observeEvent(list(input$run, gargoyle::watch("summary_exclude")), {
    # WARNING ####

    # FUNCTION CALL ####

    # LOAD INTO COMMON ####

    # METADATA ####
      common$meta$summary_network$used <- TRUE
      common$meta$summary_network$label_all <- as.numeric(input$label_all)
      common$meta$summary_network$label_excluded <- as.numeric(input$label_excluded)
      common$meta$summary_network$networkstyle <- input$networkstyle

    # TRIGGER
    gargoyle::trigger("summary_network")
  })

  # Network plot of all studies
  output$plot_all <- renderPlot({
    gargoyle::watch("summary_network")
    req(common$freq_all)
    summary_network(common$freq_all, common$bugsnetdt, input$style, input$label_all, common$logger)
    title("Network plot of all studies")
  })

  # Network connectivity all studies
  output$netconnect_all <- renderPrint({
    gargoyle::watch("summary_network")
    req(common$freq_all)
    make_netconnect(common$freq_all)
  })

  # Network plot with studies excluded
  output$plot_sub <- renderPlot({
    gargoyle::watch("summary_network")
    req(common$freq_sub)
    summary_network(common$freq_sub, common$bugsnetdt_sub, input$style, input$label_all, common$logger)
    title("Network plot with selected studies excluded")
  })

  # Network connectivity with studies excluded
  output$netconnect_sub <- renderPrint({
    gargoyle::watch("summary_network")
    req(common$freq_sub)
    make_netconnect(common$freq_sub)
  })


  output$download_all <- downloadHandler(
    filename = function() {
      paste0('Network.', input$format_all)
    },
    content = function(file) {
      draw_network <- function() {
        summary_network(common$freq_all, common$bugsnetdt, input$style, input$label_all, common$logger)
        # if (input$networkstyle == 'networkp1') {
        #   make_netgraph(common$freq_all, input$label_all)
        # } else {
        #   data.rh <- data.prep(arm.data = common$bugsnetdt, varname.t = "T", varname.s = "Study")
        #   net.plot(data.rh, node.scale = 3, edge.scale = 1.5, node.lab.cex = input$label_all)
        # }
        title("Network plot of all studies")
      }
      write_to_pdf_or_png(
        file,
        input$format_all,
        draw_network
      )
    }
  )

  output$download_sub <- downloadHandler(
    filename = function() {
      paste0('Network_sen.', input$format_sub)
    },
    content = function(file) {
      draw_network <- function() {
        # if (input$networkstyle_sub == 'netplot') {
        #   make_netgraph(common$freq_sub, input$label_excluded)
        # } else {
        #   long_sort2_sub <- common$bugsnetdt_sub  # subgroup
        #   data.rh <- data.prep(arm.data = long_sort2_sub, varname.t = "T", varname.s = "Study")
        #   net.plot(data.rh, node.scale = 3, edge.scale=1.5, node.lab.cex = input$label_excluded)
        # }
        summary_network(common$freq_sub, common$bugsnetdt_sub, input$style, input$label_sub, common$logger)
        title("Network plot with selected studies excluded")
      }
      write_to_pdf_or_png(
        file,
        input$format_sub,
        draw_network
      )
    }
  )

  return(list(
    save = function() {list(
      ### Manual save start
      ### Manual save end
      label_all = input$label_all,
      label_sub = input$label_sub,
      networkstyle = input$style,
      format_all = input$format_all,
      format_sub = input$format_sub)
    },
    load = function(state) {
      ### Manual load start
      ### Manual load end
      updateNumericInput(session, "label_all", value = state$label_all)
      updateNumericInput(session, "label_sub", value = state$label_sub)
      updateRadioButtons(session, "style", selected = state$style)
      updateRadioButtons(session, "format_all", selected = state$format_all)
      updateRadioButtons(session, "format_sub", selected = state$format_sub)
    }
  ))
})
}

summary_network_module_result <- function(id) {
  ns <- NS(id)
  fluidRow(
    conditionalPanel(
      condition = "input.style == 'netplot'",
      ns = ns,
      p("Numbers on the line indicate the number of trials conducted for the comparison. The shaded areas indicate there exist multi-arm trials between the comparisons.")
    ),
    conditionalPanel(
      condition = "input.style == 'netgraph'",
      ns = ns,
      p("The size of the nodes and thickness of edges represent the number of studies that examined a treatment and compared two given treatments respectively.")
    ),
    column(
      width = 6,
      plotOutput(ns("plot_all")),
      radioButtons(ns("format_all"), label = 'Document format', choices = c('PDF', 'PNG'), inline = TRUE),
      numericInput(ns("label_all"), label = 'Label size multiplier', value = 1.2, step = 0.1),
      downloadButton(ns("download_all")),
      br(),
      br(),
      br(),
      verbatimTextOutput(ns("netconnect_all"))
    ),
    column(
      width = 6,
      plotOutput(ns("plot_sub")),
      radioButtons(ns('format_sub'), label = 'Document format', choices = c('PDF', 'PNG'), inline = TRUE),
      numericInput(ns('label_sub'), label = 'Label size multiplier', value = 1.2, step = 0.1),
      downloadButton(outputId = ns('download_sub')),
      br(),
      br(),
      br(),
      verbatimTextOutput(ns("netconnect_sub"))
    )
  )
}

summary_network_module_rmd <- function(common){ list(
  summary_network_knit = !is.null(common$meta$summary_network$used),
  summary_network_label_all = common$meta$summary_network$label_all,
  summary_network_label_sub = common$meta$summary_network$label_sub,
  summary_network_style = common$meta$summary_network$style)
}

