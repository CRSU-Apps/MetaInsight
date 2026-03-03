core_table_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    accordion(
      id = "collapse_table",
      open = FALSE,
      accordion_panel(
        title = "Data table (Click to open / hide this panel)",
        "Use the filter box under each column of heading to select studies to exclude in the sensitivity analysis.",
        DT::dataTableOutput(ns("table"))
      )
    )
  )
}

core_table_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    # TABLE
    output$table <- DT::renderDataTable({
      watch("setup_configure")
      req(common$configured_data)

      if (common$configured_data$outcome == "Continuous") {
        colnames <- c('StudyID', 'Author', 'Treatment', 'Number of participants in each arm',
                      'Mean value of the outcome in each arm', 'Standard deviation of the outcome in each arm')

      } else {
        colnames <- c('StudyID', 'Author', 'Treatment', 'Number of participants with the outcome of interest in each arm',
                      'Number of participants in each arm')
      }

      label <- common$configured_data$treatments
      dt <- common$configured_data$non_covariate_data[, 1:length(colnames)]

      # reformat wide data
      if ("T.1" %in% colnames(dt)){
        dt <- WideToLong(dt, common$configured_data$outcome)
      }

      ntx <- nrow(label)
      dt$T <- factor(dt$T,
                     levels = c(1:ntx),
                     labels = as.character(label$Label))

      DT::datatable(
        dt,
        rownames = FALSE,
        colnames = colnames,
        filter = list(position = 'top', clear = FALSE, stateSave = TRUE)
      )
    })

    # DOWNLOAD
    output$dl_table <- downloadHandler(
      filename = function() {
        "metainsight_data_table.csv"
      },
      content = function(file) {
        write.csv(common$loaded_data$data, file, row.names = FALSE)
      }
    )

  })
}
