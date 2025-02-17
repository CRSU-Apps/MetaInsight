
#' Module UI for the result details page
#' 
#' @param id ID of the module
#' @param item_names Vector of item names to be shown side-by-side in the page.
#' @return Div for the panel
result_details_page_ui <- function(id, item_names) {
  ns <- NS(id)
  
  # Matrix containing plots for named items
  index <- 0
  
  div(
    # This is the way to get a dynamic number of columns rendered into the row
    do.call(
      fluidRow,
      lapply(
        item_names,
        function(name) {
          col <- column(
            width = 12 / length(item_names),
            result_details_panel_ui(id = ns(as.character(index)), item_name = name)
          )
          # Update the index variable in the outer scope with <<-
          # This updates the variable defined above the `div` call instead of creating a new variable with the same name within this inner function
          index <<- index + 1
          return(col)
        }
      )
    )
  )
}


#' Module server for the result details page.
#' 
#' @param id ID of the module.
#' @param models Vector of reactives containing bayesian meta-analyses.
#' @param models_valid Vector of reactives containing whether each model is valid.
#' @param package "gemtc" (default) or "bnma".
result_details_page_server <- function(id, models, models_valid, package = "gemtc") {
  moduleServer(id, function(input, output, session) {
    # Create server for each model
    sapply(
      1:length(models),
      function(index) {
        serv <- result_details_panel_server(
          id = as.character(index - 1),
          model = models[[index]],
          model_valid = models_valid[[index]],
          package = package
        )
        return(serv)
      }
    )
  })
}

