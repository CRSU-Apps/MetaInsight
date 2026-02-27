summary_char_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    actionButton(ns("run"), "Generate tables", icon = icon("arrow-turn-down")),
    div(class = "summary_char_div download_buttons",
        downloadButton(ns("download_network"), "Network"),
        layout_columns(
          div(
            class = "d-grid gap-2",
            tags$label("All studies:"),
            downloadButton(ns("download_treatments_all"), "Treatments"),
            downloadButton(ns("download_pairs_all"), "Treatment pairs"),
          ),
          div(
            class = "d-grid gap-2",
            tags$label("With selected studies excluded:"),
            downloadButton(ns("download_treatments_sub"), "Treatments"),
            downloadButton(ns("download_pairs_sub"), "Treatment pairs"),
          )
        )
    )
  )
}

summary_char_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    hide_and_show(id)

    observeEvent(input$run, {
      # WARNING ####
      if (is.null(common$configured_data)){
        common$logger |> writeLog(type = "error", go_to = "setup_configure",
                                  "Please configure the analysis first in the Setup section")
        return()
      }
      # TRIGGER
      trigger("summary_char")
    })

    summary_all <- reactive({
      req(watch("summary_char") > 0)
      common$meta$summary_char$used <- TRUE
      summary_char(common$configured_data)
    })

    summary_sub <- reactive({
      watch("setup_exclude")
      req(watch("summary_char") > 0)
      summary_char(common$subsetted_data)
    })

    network <- reactive({
      df <- data.frame(summary_all()$network$Value, summary_sub()$network$Value)
      colnames(df) <- c("All studies", "With selected studies excluded")
      rownames(df) <- summary_all()$network$Characteristic
      df
    })

    output$network <- renderTable({
      network()
    }, rownames = TRUE)

    output$download_network <- downloadHandler(
      filename = "MetaInsight_network_characteristics.csv",
      content = function(file) {
        write.csv(network(), file)
      }
    )

    output$treatments_all <- renderTable({
      summary_all()$treatments
    })

    output$treatments_sub <- renderTable({
      summary_sub()$treatments
    })

    output$download_treatments_all <- downloadHandler(
      filename = "MetaInsight_treatment_characteristics_all.csv",
      content = function(file) {
        write.csv(summary_all()$treatments, file)
      }
    )

    output$download_treatments_sub <- downloadHandler(
      filename = "MetaInsight_treatment_characteristics_sub.csv",
      content = function(file) {
        write.csv(summary_sub()$treatments, file)
      }
    )

    output$pairs_all <- renderTable({
      summary_all()$pairs
    })

    output$pairs_sub <- renderTable({
      summary_sub()$pairs
    })

    output$download_pairs_all <- downloadHandler(
      filename = "MetaInsight_treatment_pair_characteristics_all.csv",
      content = function(file) {
        write.csv(summary_all()$pairs, file)
      }
    )

    output$download_pairs_sub <- downloadHandler(
      filename = "MetaInsight_treatement_characteristics_sub.csv",
      content = function(file) {
        write.csv(summary_sub()$pairs, file)
      }
    )
})
}

summary_char_module_result <- function(id) {
  ns <- NS(id)
  fluidRow(
    div(class = "summary_char_div",
        h4("Network characteristics"),
        tableOutput(ns("network")),
        h4("Treatments"),
        h5("All studies"),
        tableOutput(ns("treatments_all")),
        h5("With selected studies excluded"),
        tableOutput(ns("treatments_sub")),
        h4("Treatment pairs"),
        h5("All studies"),
        tableOutput(ns("pairs_all")),
        h5("With selected studies excluded"),
        tableOutput(ns("pairs_sub")),
      )
  )
}

summary_char_module_rmd <- function(common) {
  list(summary_char_knit = !is.null(common$meta$summary_char$used))
}

