maths_multiply_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    h3("Select numbers"),
    numericInput(inputId = ns("number_1"), label = "First Number", value = 1, min = 1, max = 999999, step = 1),
    numericInput(inputId = ns("number_2"), label = "Second Number", value = 1, min = 1, max = 999999, step = 1),
    actionButton(ns("run"), "Multiply Numbers")
  )
}

maths_multiply_module_server <- function(id, common, parent_session) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(
        input$run,
        {
          # WARNING ####
          if (input$number_1 > 999999) {
            common$logger |>
              shinyscholar::writeLog(glue::glue("First number selected ({input$number_1}) is greater than 999999"))
          }
          if (input$number_2 > 999999) {
            common$logger |>
              shinyscholar::writeLog(glue::glue("Second number selected ({input$number_2}) is greater than 999999"))
          }
          
          # FUNCTION CALL ####
          result = maths_multiply(input$number_1, input$number_2)
          
          # LOAD INTO COMMON ####
          common$maths_number_1 <- input$number_1
          common$maths_number_2 <- input$number_2
          common$maths_product <- result
          
          # METADATA ####
          common$meta$maths_multiply$used <- TRUE
          
          # TRIGGER
          gargoyle::trigger("maths_multiply")
          
          show_results(parent_session)
        }
      )
      
      output$number_text_1 <- renderText({
        gargoyle::watch("maths_multiply")
        glue::glue("First number: {common$maths_number_1}")
      })
      
      output$number_text_2 <- renderText({
        gargoyle::watch("maths_multiply")
        glue::glue("Second number: {common$maths_number_2}")
      })
      
      output$plot <- renderPlot({
        gargoyle::watch("maths_multiply")
        req(common$maths_number_1, common$maths_number_2)
        return(maths_plot_matrix(common$maths_number_1, common$maths_number_2))
      })
    
      return(
        list(
          save = function() {
            # Save any values that should be saved when the current session is saved
            # Populate using save_and_load()
          },
          load = function(state) {
            # Load
            # Populate using save_and_load()
          }
        )
      )
    }
  )
}


maths_multiply_module_result <- function(id) {
  ns <- NS(id)

  # Result UI
  verbatimTextOutput(ns("result"))
}


maths_multiply_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  # Populate using metadata()
}

