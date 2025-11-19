rep_markdown_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    actionButton(ns("exclude"), "Exclude modules?"),
    uiOutput(ns("modules_out")),
    selectInput(ns("file_type"), label = "Select download file type",
                choices = c("HTML" = ".html", "Quarto" = ".qmd")),
    conditionalPanel("input.file_type == '.html'",
                     bslib::input_switch(ns("render"), "Include outputs?", value = TRUE), ns = ns),
    bslib::input_task_button(ns("download"), "Download report", icon = shiny::icon("download"), type = "default"),
    div(style = "visibility: hidden;",
        downloadButton(ns("dlRMD"), "")
    )
  )
}

rep_markdown_module_server <- function(id, common, parent_session, COMPONENT_MODULES) {
  moduleServer(id, function(input, output, session) {

    # create a nicely formatted list of used modules
    choices <- reactive({
      req(input$exclude > 0)
      used_modules <- names(common$meta)
      req(length(used_modules) > 0)

      choices <- list()

      components <- unique(sapply(strsplit(used_modules, "_"), `[`, 1))
      # remove setup modules
      components <- components[-which(components == "setup")]

      for (component in components) {
        module_ids <- used_modules[grepl(paste0("^", component, "_"), used_modules)]

        module_names <- sapply(module_ids, function(id) {
          COMPONENT_MODULES[[component]][[id]]$short_name
        })

        # reverse names and values
        module_names <- setNames(names(module_names), module_names)
        full_component_name <- names(COMPONENTS[COMPONENTS == component])
        choices[[full_component_name]] <- module_names
      }

      choices
    })

    # create picker with choices
    output$modules_out <- renderUI({
      req(choices())
      shinyWidgets::pickerInput(
        inputId = session$ns("selected_modules"),
        label = "Modules to include:",
        choices = choices(),
        selected = unname(unlist(choices())),
        multiple = TRUE,
        options = list(
          `selected-text-format` = "count > 3",
          `count-selected-text` = "{0} modules selected"
        )
      )
    })

    # remove all other modules when a model is deselected
    observe({
      req(choices())
      all_modules <- unname(unlist(choices()))
      selected_modules <- input$selected_modules

      not_selected <- all_modules[!(all_modules %in% selected_modules)]
      unselected_models <- not_selected[grepl("model", not_selected)]
      if (length(unselected_models) > 0){
        unselected_model_component <- unique(sapply(strsplit(unselected_models, "_"), `[`, 1))
        updated_selection <- selected_modules[!grepl(paste0(unselected_model_component, "_"), selected_modules)]
        shinyWidgets::updatePickerInput(session, "selected_modules", selected = updated_selection)
      }

    })

    # function to create report
    # GlobalEnv ensures that rmd_functions can be found
    .GlobalEnv$make_report <- function(rep_markdown_file_type){

      md_files <- c()

      rmd_intro_file <- tempfile(pattern = "intro_", fileext = ".Rmd")
      knit_params <- c(
        file = "Rmd/userReport_intro.Rmd",
        list(seed = common$seed)
      )
      intro_rmd <- do.call(knitr::knit_expand, knit_params)
      writeLines(intro_rmd, rmd_intro_file)

      md_intro_file <- tempfile(pattern = "intro_", fileext = ".md")
      rmarkdown::render(rmd_intro_file,
                        output_format = rmarkdown::github_document(html_preview = FALSE),
                        output_file = md_intro_file,
                        clean = TRUE,
                        encoding = "UTF-8")
      md_files <- c(md_files, md_intro_file)

      module_rmds <- NULL

      # force rep_renv to beginning in qmd or remove in html as that will try to run renv::restore()
      components <- names(COMPONENT_MODULES)
      if (rep_markdown_file_type == ".qmd"){
        components <- c("rep", components[components != c("rep")])
      } else {
        components <- components[components != c("rep")]
      }

      for (component in components) {
        for (module in COMPONENT_MODULES[[component]]) {

          if (length(selected_modules) > 0){
            if (!(module$id %in% selected_modules)) next
          }

          rmd_file <- module$rmd_file
          rmd_function <- module$rmd_function
          if (is.null(rmd_file)) next
          if (is.null(rmd_function)) {
            rmd_vars <- list()
          } else {
            rmd_vars <- do.call(rmd_function, list(common))
          }
          knit_params <- c(
            file = rmd_file,
            lapply(rmd_vars, metainsight::printVecAsis)
          )
          module_rmd <- do.call(knitr::knit_expand, knit_params)

          # add a section header to create tabs
          full_component_name <- names(COMPONENTS[COMPONENTS == component])
          module_rmd <- c(glue::glue("## {full_component_name}"), module_rmd)

          module_rmd_file <- tempfile(pattern = paste0(module$id, "_"),
                                      fileext = ".Rmd")
          writeLines(module_rmd, module_rmd_file)
          module_rmds <- c(module_rmds, module_rmd_file)
        }
      }

      module_md_file <- tempfile(pattern = paste0(module$id, "_"),
                                 fileext = ".md")

      rmarkdown::render(input = "Rmd/userReport_module.Rmd",
                        params = list(child_rmds = module_rmds),
                        output_format = rmarkdown::github_document(html_preview = FALSE),
                        output_file = module_md_file,
                        clean = TRUE,
                        encoding = "UTF-8")
      md_files <- c(md_files, module_md_file)

      combined_md <-
        md_files |>
        lapply(readLines) |>
        lapply(paste, collapse = "\n") |>
        paste(collapse = "\n\n")

      combined_rmd <- gsub("``` r", "```{r}", combined_md)
      combined_rmd <- unlist(strsplit(combined_rmd , "\n"))

      # remove ## for unused components and duplicates
      is_tag <- grepl("^## ", combined_rmd)
      tag_names <- sub("^## ", "", combined_rmd)
      if (!is.null(names(common$meta))){
        used_components <- unique(sapply(strsplit(names(common$meta), "_"), function(x) x[1]))
      } else {
        used_components <- c()
      }
      used_full_components <- names(COMPONENTS[COMPONENTS %in% used_components])
      lines_to_keep <- (!is_tag) | (tag_names %in% used_full_components & !duplicated(tag_names))
      combined_rmd <- combined_rmd[lines_to_keep]

      # add quarto header
      quarto_header <- readLines("Rmd/quarto_header.txt")
      quarto_header <- append(quarto_header, glue::glue("title: MetaInsight Session {Sys.Date()}"), 1)
      combined_rmd <- c(quarto_header, combined_rmd)

      # convert chunk control lines
      chunk_control_lines <- grep("\\{r,", combined_rmd)
      chunk_starts <- grep("```\\{r\\}", combined_rmd)
      chunks_to_remove <- NA
      for (i in seq_along(chunk_control_lines)) {
        chunks_to_remove[i] <- min(chunk_starts[chunk_starts > chunk_control_lines[i]])
      }
      if (any(!is.na(chunks_to_remove))){
        combined_rmd <- combined_rmd[-chunks_to_remove]
      }
      combined_rmd <- gsub("\\{r,", "```{r,", combined_rmd)
      combined_rmd <- gsub("‘", "'", combined_rmd)
      combined_rmd <- gsub("’", "'", combined_rmd)

      # fix any very long lines
      long_lines <- which(nchar(combined_rmd) > 4000)
      for (l in long_lines){
        split_lines <- strwrap(combined_rmd[l], 4000)
        combined_rmd <- combined_rmd[-l]
        combined_rmd <- append(combined_rmd, split_lines, l-1)
      }

      # remove blank lines
      idx <- with(rle(combined_rmd == ""), rep(seq_along(lengths), lengths))
      combined_rmd <- combined_rmd[!duplicated(idx) | combined_rmd != ""]

      result_file <- paste0("combined", rep_markdown_file_type)
      if (rep_markdown_file_type == ".qmd") {
        writeLines(combined_rmd, result_file, useBytes = TRUE)
      } else {
        if (render_html){
          writeLines(combined_rmd, "combined.qmd")
          on.exit(unlink("combined.qmd"))
          quarto::quarto_render(
            input = "combined.qmd",
            output_format = "html"
          )
        } else {
          combined_rmd[grep("\\{r", combined_rmd)] <- "``` r"
          writeLines(combined_rmd, "combined.md")
          on.exit(unlink("combined.md"))
          quarto::quarto_render(
            input = "combined.md",
            output_format = "html"
          )
        }

      }
      result_file
    }

    # not ideal, but can't find a method to pass to the function
    observe({
      rep_markdown_file_type <<- input$file_type
      render_html <<- input$render
      if (is.null(input$selected_modules)){
        selected_modules <<- c()
      } else {
        # always include setup modules
        selected_modules <<- c("setup_load", "setup_configure", "setup_exclude", input$selected_modules)
      }
    })

    # task that calls the function
    task <- ExtendedTask$new(function(){
      mirai::mirai(make_report(rep_markdown_file_type), globalenv())
    }) |> bslib::bind_task_button("download")

    # start the task
    observeEvent(input$download, {
      show_loading_modal("Preparing download...")
      task$invoke()
      results$resume()
    })

    # wait for the file to be prepared and then trigger the download
    # if in testing set an input value to use when the download is ready
    results <- observe({
      if (task$status() == "success") {
        results$suspend()
        if (isTRUE(getOption("shiny.testmode"))) {
          shinyjs::runjs("Shiny.setInputValue('rep_markdown-complete', 'complete');")
        } else {
          shinyjs::click("dlRMD")
        }
        close_loading_modal()
      }

      if (task$status() == "error"){
        results$suspend()
        common$logger |> writeLog(type = "error", "An error occurred trying to produce the download")
        close_loading_modal()
      }
    })

    # handler for R Markdown download
    output$dlRMD <- downloadHandler(
      filename = function() {
        paste0("metainsight-session-", Sys.Date(), input$file_type)
      },
      content = function(file) {
        file.rename(task$result(), file)
      }
    )

  }
)}
