maths_select_module_ui <- function(id) {
  ns <- shiny::NS(id)
  div(
    h3("Select numbers"),
    numericInput(inputId = ns("number_1"), label = "First Number", value = 1, min = 1, max = 999999, step = 1),
    numericInput(inputId = ns("number_2"), label = "Second Number", value = 1, min = 1, max = 999999, step = 1),
    actionButton(ns("run"), "Select Numbers")
  )
}

maths_select_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$run, {
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

      # LOAD INTO COMMON ####
      common$maths_number_1 <- input$number_1
      common$maths_number_2 <- input$number_2

      # METADATA ####
      common$meta$maths_select$used <- TRUE

      # TRIGGER
      gargoyle::trigger("maths_select")

      show_results(parent_session)
    })
    
    output$number_text_1 <- renderText({
      gargoyle::watch("maths_select")
      glue::glue("First number: {common$maths_number_1}")
    })
    
    output$number_text_2 <- renderText({
      gargoyle::watch("maths_select")
      glue::glue("Second number: {common$maths_number_2}")
    })
    
    return(
      list(
        save = function() {
          list(
            ### Manual save start
            ### Manual save end
            number_1 = input$number_1,
            number_2 = input$number_2
          )
        },
        load = function(state) {
          ### Manual load start
          ### Manual load end
          updateNumericInput(session, "number_1", value = state$number_1)
          updateNumericInput(session, "number_2", value = state$number_2)
        }
      )
    )
  })
}

maths_select_module_result <- function(id) {
  ns <- NS(id)
  div(
    textOutput(outputId = ns("number_text_1")),
    textOutput(outputId = ns("number_text_2"))
  )
}

maths_select_module_rmd <- function(common){
  list(
    maths_select_knit = !is.null(common$meta$maths_select$used),
    maths_select_number_1 = common$maths_number_1,
    maths_select_number_2 = common$maths_number_2
  )
}
