baseline_summary_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("run"), "Generate plot", icon = icon("arrow-turn-down")),
    downloadButton(ns("download"), "Download plot")
  )
}

baseline_summary_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  shinyjs::hide("download")

  observeEvent(input$run, {
    # WARNING ####
    if (is.null(common$freq_sub)){
      common$logger |> writeLog(type= "error", go_to = "setup_configure",
                                "Please configure the analysis first in the Setup section")
      return()
    }
    # FUNCTION CALL ####
    common$baseline_summary_plot <- baseline_summary(common$main_connected_data, common$outcome, common$treatment_df, common$logger)

    # METADATA ####
    common$meta$baseline_summary$used <- TRUE

    # TRIGGER
    trigger("baseline_summary")
    shinyjs::show("download")

  })

  output$plot <- renderUI({
    watch("baseline_summary")
    req(common$baseline_summary_plot)
    div(class = "svg_container", style = "max-width: 800px;",
        HTML(common$baseline_summary_plot$svg)
    )
  })

  output$download <- downloadHandler(
    filename = function(){
      paste0("MetaInsight_baseline_summary.", common$download_format)},
    content = function(file){
      write_svg_plot(file, common$download_format, common$baseline_summary_plot)
    }
  )

})
}

baseline_summary_module_result <- function(id) {
  ns <- NS(id)
  div(align = "center",
    uiOutput(ns("plot"))
  )
}

baseline_summary_module_rmd <- function(common) {
  list(
    baseline_summary_knit = !is.null(common$meta$baseline_summary$used)
  )
}
