freq_compare_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    actionButton(ns("run"), "Generate tables", icon = icon("arrow-turn-down")),
    div(class = "freq_compare_div", download_button_pair(id))
  )
}

freq_compare_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  hide_and_show(id)

  observeEvent(input$run, {
    # WARNING ####
    if (is.null(common$freq_all)){
      common$logger |> writeLog(type = "error", go_to = "setup_configure",
                                "Please configure the analysis first in the Setup section")
      return()
    }
    # TRIGGER
    trigger("freq_compare")
  })

  table_all <- reactive({
    watch("model")
    req(watch("freq_compare") > 0)
    shinyjs::show(selector = ".freq_compare_div")
    common$meta$freq_compare$used <- TRUE
    freq_compare(common$freq_all, common$model_type, common$ranking_option)
  })

  table_sub <- reactive({
    watch("setup_exclude")
    req(watch("freq_compare") > 0)
    freq_compare(common$freq_sub, common$model_type, common$ranking_option)
  })

  output$table_all <- renderTable(colnames = FALSE, {
    table_all()
  })

  outputOptions(output, "table_all", suspendWhenHidden = FALSE)

  output$table_sub <- renderTable(colnames = FALSE, {
    table_sub()
  })

  output$download_all <- downloadHandler(
    filename = "MetaInsight_treatment_rankings_all.csv",
    content = function(file) {
      write.csv(table_all(), file)
    }
  )

  output$download_sub <- downloadHandler(
    filename = "MetaInsight_treatment_rankings_sub.csv",
    content = function(file) {
      write.csv(table_sub(), file)
    }
  )

  })
}

freq_compare_module_result <- function(id) {
  ns <- NS(id)
  tagList(
    h4(class = "freq_compare_div",
       "Treatments are ranked from best to worst along the leading diagonal. Above the leading diagonal are estimates from pairwise meta-analyses, below the leading diagonal are estimates from network meta-analyses"),
    h4(class = "freq_compare_div",
       "Relative treatment effects in ranked order for all studies"),
    tableOutput(ns("table_all")),
    br(),
    h4(class = "freq_compare_div",
       "Relative treatment effects in ranked order with selected studies excluded"),
    tableOutput(ns("table_sub"))
  )
}

freq_compare_module_rmd <- function(common) {
  list(freq_compare_knit = !is.null(common$meta$freq_compare$used))
}

