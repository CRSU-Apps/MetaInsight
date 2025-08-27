setup_square_module_ui <- function(id) {
  ns <- shiny::NS(id)
  div(
    h3("Square a number"),
    numericInput(inputId = ns("number"), label = "Number", value = 1, min = 1, max = 5, step = 1),
    actionButton(ns("run"), "Select Number")
  )
}

setup_square_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$run, {
      # WARNING ####
      if (input$number > 5) {
        common$logger |>
          shinyscholar::writeLog(glue::glue("Number selected ({input$number}) is greater than 5"))
      }
      
      # FUNCTION CALL ####
      result <- setup_square(input$number)

      # LOAD INTO COMMON ####
      common$number <- input$number
      common$square <- result

      # METADATA ####
      common$meta$setup_square$used <- TRUE

      # TRIGGER
      gargoyle::trigger("setup_square")

      show_results(parent_session)
    })
    
    output$number_text <- renderText({
      gargoyle::watch("setup_square")
      glue::glue("{common$number}^2 = {common$square}")
    })
    
    return(
      list(
        save = function() {
          list(
            ### Manual save start
            ### Manual save end
            number = input$number
          )
        },
        load = function(state) {
          ### Manual load start
          ### Manual load end
          updateNumericInput(session, "number", value = state$number)
        }
      )
    )
  })
}

setup_square_module_result <- function(id) {
  ns <- NS(id)
  textOutput(outputId = ns("number_text"))
}

setup_square_module_rmd <- function(common){
  list(
    setup_square_knit = !is.null(common$meta$setup_square$used),
    setup_square_number = common$number,
    setup_square_square = common$square
  )
}
