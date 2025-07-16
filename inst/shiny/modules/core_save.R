core_save_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    div(downloadButton(ns("save_session"), "Save Session"), style = "visibility: hidden"),
  )
}

core_save_module_server <- function(id, common, modules, COMPONENTS, main_input) {
  moduleServer(id, function(input, output, session) {

    # listen for the button press in the menu and then trigger the download
    observeEvent(input$save, {
      show_loading_modal("Saving session...")
      shinyjs::runjs("document.getElementById('core_save-save_session').click();")
    })

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

        on.exit(close_loading_modal())
        on.exit(rm("temp"), add = TRUE)

        temp <- list()
        common_items <- names(common)
        # exclude the non-public, function objects and mcmc as they are very large
        save_items  <- common_items[!common_items %in% c("clone", ".__enclos_env__", "logger", "reset", "tasks",
                                                         "bayes_mcmc_all", "bayes_mcmc_sub")]
        temp[save_items] <- as.list(common)[save_items]
        # save logger minus the header
        temp$logger <- strsplit(common$logger(), "-----<br>")[[1]][3]
        class(temp) <- "common"
        saveRDS(temp, file)

      }
    )
  })
}


