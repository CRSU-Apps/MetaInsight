core_save_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    br(),
    h5(em("Note: To save your session code or metadata, use the Reproduce component")),
    wellPanel(
      h4(strong("Save Session")),
      p(paste0("By saving your session into an RDS file, you can resume ",
               "working on it at a later time or you can share the file",
               " with a collaborator.")),
      shinyjs::hidden(p(
        id = "save_warning",
        icon("triangle-exclamation"),
        paste0("The current session data is large, which means the ",
               "downloaded file may be large and the download might",
               " take a long time.")
      )),
      downloadButton(ns("save_session"), "Save Session"),
      br()
  )
  )
}

core_save_module_server <- function(id, common, modules, COMPONENTS, main_input) {
  moduleServer(id, function(input, output, session) {

    output$save_session <- downloadHandler(
      filename = function() {
        paste0("metainsight-session-", Sys.Date(), ".rds")
      },
      content = function(file) {
        common$state$main <- list(
          selected_module = sapply(COMPONENTS, function(x) main_input[[glue("{x}Sel")]], simplify = FALSE)
        )

        # Store app version and name
        common$state$main$version <- as.character(packageVersion("metainsight"))
        common$state$main$app <- "metainsight"

        # Ask each module to save whatever data it wants
        for (module_id in names(modules)) {
          common$state[[module_id]] <- modules[[module_id]]$save()
        }

        saveRDS(common, file)

      }
    )
  })
}


