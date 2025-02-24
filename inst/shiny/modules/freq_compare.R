freq_compare_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    actionButton(ns("run"), "Run module freq_compare")
  )
}

freq_compare_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {
    # WARNING ####

    # FUNCTION CALL ####

    # LOAD INTO COMMON ####

    # METADATA ####
    # Populate using metadata()

    # TRIGGER
    gargoyle::trigger("freq_compare")
  })

  table_all <- reactive({
    watch("setup_define")
    watch("model")
    req(common$freq_all)
    common$meta$freq_compare$used <- TRUE
    freq_compare(common$freq_all, common$model_type, common$ranking_option)
  })

  table_sub <- reactive({
    watch("summary_exclude")
    req(common$freq_sub)
    freq_compare(common$freq_sub, common$model_type, common$ranking_option)
  })

  output$table_all <- renderTable(colnames=FALSE, {
    req(table_all())
    table_all()
  })

  output$table_sub <- renderTable(colnames=FALSE, {
    req(table_sub())
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
    h4("Treatments are ranked from best to worst along the leading diagonal. Above the leading diagonal are estimates from pairwise meta-analyses, below the leading diagonal are estimates from network meta-analyses"),
    h4("Relative treatment effects in ranked order for all studies"),
    tableOutput(ns("table_all")),
    downloadButton(ns("download_all")),
    br(),
    h4("Relative treatment effects in ranked order with selected studies excluded"),
    tableOutput(ns("table_sub")),
    downloadButton(ns("download_sub"))
  )
}

freq_compare_module_rmd <- function(common) {
  list(freq_compare_knit = !is.null(common$meta$freq_compare$used))
}

