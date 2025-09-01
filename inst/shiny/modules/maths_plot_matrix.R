maths_plot_matrix_module_ui <- function(id) {
  ns <- shiny::NS(id)
  div(
    actionButton(ns("run"), "Generate Plot")
  )
}

maths_plot_matrix_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$run, {
      req(common$maths_number_1, common$maths_number_2)
      
      # WARNING ####
      if (common$maths_number_1 > 20 || common$maths_number_2 > 20) {
        common$logger |>
          shinyscholar::writeLog("Text may not be readable on plot")
      }
      
      # FUNCTION CALL ####

      # LOAD INTO COMMON ####

      # METADATA ####
      common$meta$maths_plot_matrix$used <- TRUE

      # TRIGGER
      gargoyle::trigger("maths_plot_matrix")

      show_results(parent_session)
    })
    
    output$plot <- renderPlot({
      gargoyle::watch("maths_plot_matrix")
      req(common$maths_number_1, common$maths_number_2)
      return(maths_plot_matrix(common$maths_number_1, common$maths_number_2))
    })
    
    return(
      list(
        save = function() {
          list(
            ### Manual save start
            ### Manual save end
          )
        },
        load = function(state) {
          ### Manual load start
          ### Manual load end
        }
      )
    )
  })
}

maths_plot_matrix_module_result <- function(id) {
  ns <- NS(id)
  plotOutput(outputId = ns("plot"))
}

maths_plot_matrix_module_rmd <- function(common){
  list(
    maths_plot_matrix_knit = !is.null(common$meta$maths_plot_matrix$used),
    maths_select_number_1 = common$maths_number_1,
    maths_select_number_2 = common$maths_number_2
  )
}
