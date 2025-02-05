function(input, output, session) {

  ########################## #
  # INTRODUCTION ####
  ########################## #

  core_intro_module_server("core_intro")

  ########################## #
  # LOGGING ####
  ########################## #

  initLogMsg <- function() {
    intro <- "***WELCOME TO METAINSIGHT***"
    brk <- paste(rep("------", 14), collapse = "")
    expl <- "Please find messages for the user in this log window."
    logInit <- gsub(".{4}$", "", paste(intro, brk, expl, brk, "", sep = "<br>"))
    logInit
  }
  common$logger <- reactiveVal(initLogMsg())

  # Write out logs to the log Window
  observeEvent(common$logger(), {
    shinyjs::html(id = "logHeader", html = common$logger(), add = FALSE)
    shinyjs::js$scrollLogger()
  })

  ########################## #
  # REACTIVE VALUES LISTS ####
  ########################## #

  # tab and module-level reactives
  component <- reactive({
    input$tabs
  })
  observe({
    if (component() == "_stopapp") {
      shinyjs::runjs("window.close();")
      stopApp()
    }
  })
  module <- reactive({
    if (component() == "intro") "intro"
    else input[[glue("{component()}Sel")]]
  })

  ######################## #
  ### GUIDANCE TEXT ####
  ######################## #

  # UI for component guidance text
  output$gtext_component <- renderUI({
    file <- file.path("Rmd", glue("gtext_{component()}.Rmd"))
    if (!file.exists(file)) return()
    includeMarkdown(file)
  })

  # UI for module guidance text
  output$gtext_module <- renderUI({
    req(module())
    file <- COMPONENT_MODULES[[component()]][[module()]]$instructions
    if (is.null(file)) return()
    includeMarkdown(file)
  })

  # Help Component
  help_components <- COMPONENTS[!COMPONENTS == "rep"]
  lapply(help_components, function(component) {
    btn_id <- paste0(component, "Help")
    observeEvent(input[[btn_id]], updateTabsetPanel(session, "main", "Component Guidance"))
  })

  # Help Module
  lapply(help_components, function(component) {
    lapply(COMPONENT_MODULES[[component]], function(module) {
      btn_id <- paste0(module$id, "Help")
      observeEvent(input[[btn_id]], updateTabsetPanel(session, "main", "Module Guidance"))
      })})


  ############################################# #
  ### TABLE TAB ####
  ############################################# #

  # TABLE
  output$table <- DT::renderDataTable({
    gargoyle::watch("setup_define")
    req(common$initial_non_covariate_data)

    if (common$metaoutcome == "Continuous") {
      colnames <- c('StudyID', 'Author', 'Treatment', 'Number of participants in each arm',
                    'Mean value of the outcome in each arm', 'Standard deviation of the outcome in each arm')

    } else {
      colnames <- c('StudyID', 'Author', 'Treatment', 'Number of participants with the outcome of interest in each arm',
                    'Number of participants in each arm')
    }

    label <- common$treatment_df
    dt <- common$initial_non_covariate_data
    ntx <- nrow(label)
    dt$T <- factor(dt$T,
                   levels = c(1:ntx),
                   labels = as.character(label$Label))

    DT::datatable(
      dt,
      editable = TRUE, # is this supposed to be?
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
      write.csv(common$data, file, row.names = FALSE)
    }
  )


  ####################
  ### INITIALISATION ####
  ###################

  # Initialize all modules
  gargoyle::init("intro")
  modules <- list()
  lapply(names(COMPONENT_MODULES), function(component) {
    lapply(COMPONENT_MODULES[[component]], function(module) {
      # Initialize event triggers for each module
      gargoyle::init(module$id)
       if (module$id == "rep_markdown"){
        return <- do.call(get(module$server_function), args = list(id = module$id, common = common, parent_session = session, COMPONENT_MODULES))
      } else {
      return <- do.call(get(module$server_function), args = list(id = module$id, common = common, parent_session = session))
      }
      if (is.list(return) &&
          "save" %in% names(return) && is.function(return$save) &&
          "load" %in% names(return) && is.function(return$load)) {
        modules[[module$id]] <<- return
      }
    })
  })


  ############################################# #
  ### RUN MODULE ON ENTER ####
  ############################################# #
  observeEvent(input$run_module, {
    req(module())
    shinyjs::runjs(glue::glue("document.getElementById('{module()}-run').click();"))
  })

  ################################
  ### SAVE / LOAD FUNCTIONALITY ####
  ################################

  core_save_module_server("core_save", common, modules, COMPONENTS, input)
  core_load_module_server("core_load", common, modules, map, COMPONENT_MODULES, parent_session = session)

  ################################
  ### DEBUGGING ####
  ################################

  output$debug <- renderPrint({
    browser()
  }) |> bindEvent(input$debug_button)


  ################################
  ### EXPORT TEST VALUES ####
  ################################
  exportTestValues(common = common)
}

