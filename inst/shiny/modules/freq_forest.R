freq_forest_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    actionButton(ns("run"), "Run module freq_forest")
  )
}

freq_forest_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {
    # WARNING ####

    # FUNCTION CALL ####

    # LOAD INTO COMMON ####

    # METADATA ####
    # Populate using metadata()

    # TRIGGER
    gargoyle::trigger("freq_forest")
  })

  output$result <- renderText({
    gargoyle::watch("freq_forest")
    # Result
  })

  return(list(
    save = function() {
      # Save any values that should be saved when the current session is saved
      # Populate using save_and_load()
    },
    load = function(state) {
      # Load
      # Populate using save_and_load()
    }
  ))
})
}

freq_forest_module_result <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 6,
      plotOutput(ns("plot_all")),
      fixedRow(
        p("Options to change limits of the x-axis:"),
        column(
          width = 6,
          align = "center",
          numericInput(ns("xmin_all"), label = "Minimum", value = 0, step = 0.1)
        ),
        column(
          width = 6,
          align = "center",
          numericInput(ns("xmax_all"), label = "Maximum", value = 5, step = 1)
        )
      ),
      textOutput(ns("text_all")),
      radioButtons(
        ns("format_all"),
        label = "Document format",
        choices = c("PDF", "PNG"),
        inline = TRUE
      ),
      downloadButton(ns("download_all"))
    ),
    column(
      width = 6,
      plotOutput(ns("plot_sub")),
      fixedRow(
        p("Options to change limits of the x-axis:"),
        column(
          width = 6,
          align = "center",
          numericInput(ns("xmin_sub"), label = "Minimum", value = 0, step = 0.1)
        ),
        column(
          width = 6,
          align = "center",
          numericInput(ns("xmax_sub"), label = "Maximum", value = 5, step = 1)
        )
      ),

      tags$style(
        glue::glue(
          "#{ns(\"ref_change\")} {{
               background-color: #ffd966;
                display:block;
              }}"
        )
      ),
      textOutput(ns("ref_change")),
      br(),
      textOutput(ns("text_sub")),
      radioButtons(
        ns("format_sub"),
        label = "Document format",
        choices = c("PDF", "PNG"),
        inline = TRUE
      ),
      downloadButton(ns("download_sub"))
    )
  )

}

freq_forest_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  # Populate using metadata()
}

