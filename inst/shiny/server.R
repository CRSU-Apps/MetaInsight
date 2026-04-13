function(input, output, session) {

  ########################## #
  # INTRODUCTION ####
  ########################## #

  core_intro_module_server("core_intro")

  ########################## #
  # ANALYTICS ####
  ########################## #

  core_analytics_module_server("core_analytics", reactive(input$cookies), "G-H3241DM66M", module)

  ########################## #
  # LOAD COMMON ####
  ########################## #

  source(system.file("shiny", "common.R", package = "metainsight"))
  common <- common_class$new()

  common$seed <- sample.int(n = 1000, size = 1)

  ########################## #
  # LOGGING ####
  ########################## #

  initLogMsg <- function() {
    intro <- "***WELCOME TO METAINSIGHT***"
    brk <- paste(rep("------", 13), collapse = "")
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

  output$processing <- renderUI({
    status <- unlist(lapply(common$tasks, function(x){x$status()}))
    running <- length(status[status == "running"])
    # input$processing_intro is used to show in the introduction tour
    if (running >= 1 || !is.null(input$processing_intro)){
      return(div(icon("rotate", class = "fa-spin loading-icon"), style = "width: 100%;"))
    } else {
      return(NULL)
    }
  })

  ########################## #
  # REACTIVE VALUES LISTS ####
  ########################## #

  # tab and module-level reactives
  component <- reactive({
    input$tabs
  })

  module <- reactive({
    if (component() == "intro") "intro"
    else input[[glue("{component()}Sel")]]
  })

  ######################## #
  ### GUIDANCE TEXT ####
  ######################## #

  # UI for module guidance text
  output$gtext_module <- renderUI({
    req(module())
    file <- COMPONENT_MODULES[[component()]][[module()]]$instructions
    if (is.null(file)) return()
    includeMarkdown(file)
  })

  # Help Module
  help_components <- COMPONENTS[!COMPONENTS == "rep"]
  lapply(help_components, function(component) {
    lapply(COMPONENT_MODULES[[component]], function(module) {
      btn_id <- paste0(module$id, "Help")
      observeEvent(input[[btn_id]], updateTabsetPanel(session, "main", "Guidance"))
      })})

  ####################
  ### INITIALISATION ####
  ###################

  # Initialize all modules
  init("intro")
  modules <- list()
  lapply(names(COMPONENT_MODULES), function(component) {
    lapply(COMPONENT_MODULES[[component]], function(module) {
      # Initialize event triggers for each module
      init(module$id)
      if (module$id == "rep_markdown"){
       return <- do.call(get(module$server_function), args = list(id = module$id, common = common, parent_session = session, COMPONENT_MODULES))
      } else if (module$id == "setup_reload"){
        # do nothing (loaded below once modules list is complete)
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
  observe({
    shinyjs::js$runOnEnter(module())
  })

  ############################################# #
  ### PLOT DOWNLOAD FORMAT ####
  ############################################# #
  observe(common$download_format <- input$download_format)

  ################################
  ### SAVE / LOAD FUNCTIONALITY ####
  ################################

  core_save_module_server("core_save", common, modules, COMPONENTS, input)
  setup_reload_module_server("setup_reload", common, modules, session)

  ################################
  ### EXPORT TEST VALUES ####
  ################################

  # very ugly but only export the data
  exportTestValues(common = as.list(common)[names(common)[!names(common) %in% c("clone", ".__enclos_env__", "logger", "reset", "tasks")]],
                   logger = common$logger())
}

