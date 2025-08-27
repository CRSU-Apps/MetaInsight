setup_plot_module_ui <- function(id) {
  ns <- shiny::NS(id)
  div(
    actionButton(ns("run"), "Generate Plot")
  )
}

setup_plot_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    plot <- reactiveVal()

    observeEvent(input$run, {
      req(common$number)
      
      # WARNING ####
      if (common$number > 20) {
        common$logger |>
          shinyscholar::writeLog("Text may not be readable on plot")
      }
      
      # FUNCTION CALL ####

      # LOAD INTO COMMON ####

      # METADATA ####
      common$meta$setup_plot$used <- TRUE

      # TRIGGER
      gargoyle::trigger("setup_plot")

      show_results(parent_session)
    })
    
    output$plot <- renderPlot({
      gargoyle::watch("setup_plot")
      req(common$number)
      return(setup_plot(common$number))
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

setup_plot_module_result <- function(id) {
  ns <- NS(id)
  plotOutput(outputId = ns("plot"))
}

setup_plot_module_rmd <- function(common){
  list(
    setup_plot_knit = !is.null(common$meta$setup_plot$used)
  )
}
