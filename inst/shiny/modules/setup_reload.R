setup_reload_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    h4("Load session"),
    includeMarkdown("Rmd/text_loadsesh.Rmd"),
    div(id = "reload_inputs",
      fileInput(ns("load_session"), "", accept = ".rds"),
      actionButton(ns("goLoad_session"), "Load RDS")
    )
  )
}

setup_reload_module_server <- function(id, common, modules, parent_session) {
  moduleServer(id, function(input, output, session) {

    observe({
      shinyjs::toggleState("goLoad_session", !is.null(input$load_session$datapath))
    })

    load_session <- function(temp){

      if (!inherits(temp, "common") || temp$state$main$app != "metainsight"){
        close_loading_modal()
        common$logger |> writeLog(type = "error", "That is not a valid MetaInsight save file")
        return()
      }

      if (temp$state$main$version != as.character(packageVersion("metainsight"))){
        current_version <- as.character(packageVersion("metainsight"))
        common$logger |> writeLog(type = "warning",
                                   glue::glue("The save file was created using MetaInsight v{temp$state$main$version},
                                 but you are using MetaInsight v{current_version}"))
      }

      common$logger |> writeLog(temp$logger)

      temp_names <- names(temp)
      temp_names <- temp_names[temp_names != "logger"]
      for (name in temp_names){
        common[[name]] <- temp[[name]]
      }

      # Ask each module to load its own data
      for (module_id in names(modules)) {
        modules[[module_id]]$load(common$state[[module_id]])
      }

      for (component in names(common$state$main$selected_module)) {
        value <- common$state$main$selected_module[[component]]
        shinyWidgets::updateRadioGroupButtons(parent_session, glue("{component}Sel"), selected = value)
      }

      # restore results for used modules
      used_modules <- names(common$meta)
      # these are modules which setup data but don't need to be rerun on reloading
      setup_modules <- c("setup_load", "setup_configure", "setup_exclude", "model",
                         "bayes_model", "bayes_nodesplit", "bayes_deviance")

      # these are used to trigger some outputs to reload
      if ("bayes_deviance" %in% used_modules){
        trigger("bayes_deviance_all")
        trigger("bayes_deviance_sub")
      }
      if ("bayes_nodesplit" %in% used_modules){
        trigger("bayes_nodesplit_all")
        trigger("bayes_nodesplit_sub")
      }
      if ("bayes_model" %in% used_modules){
        trigger("bayes_model_table_all")
        trigger("bayes_model_table_sub")
      }

      used_modules <- used_modules[!(used_modules %in% setup_modules)]
      for (used_module in used_modules){
        trigger(used_module)
      }
    }

    observeEvent(input$goLoad_session, {
      show_loading_modal("Loading previous session")
      temp <- readRDS(input$load_session$datapath)
      load_session(temp)
      close_loading_modal()
      common$logger |> writeLog(type = "info", "The previous session has been loaded successfully")
    })

    # load file if run_metainsight has a load_file parameter
    load_file_path <- reactive({if (exists("load_file_path", envir = .GlobalEnv)) {
      get("load_file_path", envir = .GlobalEnv)
    } else {
      NULL
    }})

    load_on_start <- observe({
      req(load_file_path())
      if (!file.exists(load_file_path())){
        common$logger |> writeLog(type = "error", "The specified load file cannot be found - please check the path")
        load_on_start$destroy()
        return()
      }
      show_loading_modal("Loading previous session")
      load_session(readRDS(load_file_path()))
      close_loading_modal()
      common$logger |> writeLog(type = "info", "The previous session has been loaded successfully")
      load_on_start$destroy()
    })

})
}


