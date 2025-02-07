summary_char_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    actionButton(ns("run"), "Run module summary_char")
  )
}

summary_char_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {
    # WARNING ####

    # FUNCTION CALL ####

    # LOAD INTO COMMON ####

    # METADATA ####

    # TRIGGER
    trigger("summary_char")
  })

    # Characteristics table of all studies
    output$sumtb <- renderTable({
      watch("setup_define")
      req(common$bugsnetdt)
      common$meta$summary_char$used <- TRUE
      summary_char(common$bugsnetdt, common$metaoutcome)
    })

    # Characteristics table with studies excluded
    output$sumtb_sub <- renderTable({
      watch("summary_exclude")
      watch("setup_define")
      req(common$bugsnetdt)
      summary_char(common$bugsnetdt_sub, common$metaoutcome)
    })

  return(list(
    save = function() {
      # Save any values that should be saved when the current session is saved
    },
    load = function(state) {
      # Load
    }
  ))
})
}

summary_char_module_result <- function(id) {
  ns <- NS(id)

  fluidRow(
  column(
    width = 6,
    h4("Characteristics table of all studies"),
    tableOutput(ns("sumtb"))
  ),
  column(
    width = 6,
    h4("Characteristics table with selected studies excluded"),
    tableOutput(ns("sumtb_sub"))
  )
  )
}

summary_char_module_rmd <- function(common) {
  list(summary_char_knit = !is.null(common$meta$summary_char$used))
}

