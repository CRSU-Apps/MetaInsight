covariate_summary_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("run"), "Generate plot", icon = icon("arrow-turn-down")),
    div(class = "covariate_summary_div download_buttons",
        downloadButton(ns("download"), "Download plot")
    )
  )
}

covariate_summary_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    hide_and_show(id)

    observeEvent(input$run, {
      # WARNING ####
      if (is.null(common$freq_sub)){
        common$logger |> writeLog(type= "error", go_to = "setup_configure",
                                  "Please configure the analysis first in the Setup section")
        return()
      }
      # FUNCTION CALL ####
      common$covariate_summary_plot <- covariate_summary(common$main_connected_data, common$outcome, common$treatment_df, common$logger)

      # METADATA ####
      common$meta$covariate_summary$used <- TRUE

      # TRIGGER
      trigger("covariate_summary")

    })

    output$plot <- renderUI({
      watch("covariate_summary")
      req(common$covariate_summary_plot)
      div(class = "svg_container", style = "max-width: 800px;",
          common$covariate_summary_plot
      )
    })

    output$download <- downloadHandler(
      filename = function(){
        paste0("MetaInsight_covariate_summary.", common$download_format)},
      content = function(file){
        write_svg_plot(common$covariate_summary_plot,
                       file,
                       common$download_format)
      }
    )

  })
}

covariate_summary_module_result <- function(id) {
  ns <- NS(id)
  div(align = "center",
      uiOutput(ns("plot"))
  )
}

covariate_summary_module_rmd <- function(common) {
  list(
    covariate_summary_knit = !is.null(common$meta$covariate_summary$used)
  )
}
