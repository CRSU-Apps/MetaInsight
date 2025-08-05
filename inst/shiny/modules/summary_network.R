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
    actionButton(ns("run"), "Generate plots", icon = icon("arrow-turn-down")),
    div(class = "summary_network_div",
      fluidRow(
        tags$label("Label size"),
        column(width = 6,
               numericInput(ns("label_all"), label = "All studies", value = 1.2, step = 0.1)
        ),
        column(width = 6,
               numericInput(ns('label_sub'), label = "Selected studies excluded", value = 1.2, step = 0.1)
        )
      ),
      download_button_pair(id)
    )
  )
}

summary_network_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    shinyjs::hide(selector = ".summary_network_div")

    observeEvent(input$run, {
      # WARNING ####
      if (is.null(common$freq_sub)){
        common$logger |> writeLog(type = "error", "Please configure the analysis first in the Setup component")
        return()
      }
      # TRIGGER
      trigger("summary_network")
      shinyjs::show(selector = ".summary_network_div")
      common$meta$summary_network$used <- TRUE
      common$meta$summary_network$label_all <- as.numeric(input$label_all)
      common$meta$summary_network$label_sub <- as.numeric(input$label_sub)
      common$meta$summary_network$style <- input$style
    })

    output$plot_all <- renderPlot({
      req(watch("summary_network") > 0)
      summary_network(common$freq_all,
                      common$bugsnet_all,
                      input$style,
                      as.numeric(input$label_all),
                      common$logger)
      title("Network plot of all studies")
    })

    output$plot_sub <- renderPlot({
      watch("setup_exclude")
      req(watch("summary_network") > 0)
      summary_network(common$freq_sub,
                      common$bugsnet_sub,
                      input$style,
                      as.numeric(input$label_sub),
                      common$logger)
      title("Network plot with selected studies excluded")
    })

    netconnect_all <- reactive({
      req(watch("summary_network") > 0)
      make_netconnect(common$freq_all)
    })

    netconnect_sub <- reactive({
      watch("setup_exclude")
      req(watch("summary_network") > 0)
      make_netconnect(common$freq_sub)
    })

    output$table <- renderTable({
      df <- data.frame(netconnect_all(), netconnect_sub())
      rownames(df) <- c("Studies", "Pairwise comparisons", "Treatments", "Designs", "Subnetworks")
      colnames(df) <- c("All studies", "With selected studies excluded")
      df
    }, rownames = TRUE)

    output$download_all <- downloadHandler(
      filename = function() {
        paste0("MetaInsight_network_plot_all.", common$download_format)
      },
      content = function(file) {
        draw_network <- function() {
          summary_network(common$freq_all, common$bugsnet_all, input$style, input$label_all, common$logger)
          title("Network plot of all studies")
        }
        write_plot(
          file,
          common$download_format,
          draw_network,
          height = 6,
          width = 9
        )
      }
    )

    output$download_sub <- downloadHandler(
      filename = function() {
        paste0("MetaInsight_network_plot_sub.", common$download_format)
      },
      content = function(file) {
        draw_network <- function() {
          summary_network(common$freq_sub, common$bugsnet_sub, input$style, input$label_sub, common$logger)
          title("Network plot with selected studies excluded")
        }
        write_plot(
          file,
          common$download_format,
          draw_network,
          height = 6,
          width = 9
        )
      }
    )

    return(list(
      save = function() {list(
        ### Manual save start
        ### Manual save end
        label_all = input$label_all,
        label_sub = input$label_sub,
        style = input$style,
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
  div(class = "summary_network_div",
    fluidRow(
      conditionalPanel(
        condition = "input.style == 'netgraph'",
        ns = ns,
        h4("Numbers on the line indicate the number of trials conducted for the comparison. The shaded areas indicate there exist multi-arm trials between the comparisons.")
      ),
      conditionalPanel(
        condition = "input.style == 'netplot'",
        ns = ns,
        h4("The size of the nodes and thickness of edges represent the number of studies that examined a treatment and compared two given treatments respectively.")
      ),
      column(width = 6,
             plotOutput(ns("plot_all"))
      ),
      column(width = 6,
             plotOutput(ns("plot_sub"))
      ),
      div(style = "display: flex; justify-content: center; padding-top: 50px", tableOutput(ns("table")))
    )
  )
}

summary_network_module_rmd <- function(common){ list(
  summary_network_knit = !is.null(common$meta$summary_network$used),
  summary_network_label_all = common$meta$summary_network$label_all,
  summary_network_label_sub = common$meta$summary_network$label_sub,
  summary_network_style = common$meta$summary_network$style)
}
