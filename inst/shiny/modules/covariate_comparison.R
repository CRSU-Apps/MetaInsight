metaregression_comparison_module_ui <- function(id){
  ns <- NS(id)
  downloadButton(ns("download"), "Download table")
}

covariate_comparison_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("run"), "Generate table", icon = icon("arrow-turn-down")),
    metaregression_comparison_module_ui(ns("covariate"))
  )
}

metaregression_comparison_module_server <- function(id, common, run) {
  moduleServer(id, function(input, output, session) {

    module <- glue::glue("{id}_comparison")
    model <- glue::glue("{id}_model")
    model_fit <- glue::glue("{id}_model_fit")
    class <- glue::glue(".{module}_div")

    shinyjs::hide("download")
    shinyjs::hide(selector = class)

    observeEvent(run(), {
      if (is.null(common[[model]])){
        common$logger |> writeLog(type = "error", glue::glue("Please fit the {id} model first"))
        return()
      } else {
        common$meta[[module]]$used <- TRUE
        shinyjs::show("download")
        shinyjs::show(selector = class)
        trigger(module)
      }
    })

    output$table <- renderTable({
      watch(model_fit)
      req(watch(module) > 0)
      do.call(module, list(common[[model]]))
    }, rownames = TRUE)

    output$download <- downloadHandler(
      filename = function(){
        glue::glue("MetaInsight_{id}_comparison.csv")
      },
      content = function(file) {
        write.csv(do.call(module, list(common[[model]])), file)
      }
    )
  })
}

covariate_comparison_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {
    metaregression_comparison_module_server("covariate", common, reactive(input$run))
  })
}

metaregression_comparison_module_result <- function(id, class) {
  ns <- NS(id)
  div(align = "center", class = class,
      p(tags$strong("Treatment effects for all studies: comparison of all treatment pairs")),
      tableOutput(ns("table"))
  )
}

covariate_comparison_module_result <- function(id) {
  ns <- NS(id)
  metaregression_comparison_module_result(ns("covariate"), "covariate_comparison_div")
}

covariate_comparison_module_rmd <- function(common) {
  list(covariate_comparison_knit = !is.null(common$meta$covariate_comparison$used))
}

