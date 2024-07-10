
#' Module UI for the covariate regression nodesplit model page.
#' 
#' @param id ID of the module.
#' @param package_name Name of the package bering used.
#' @return Div for the page.
covariate_nodesplit_page_ui <- function(id, package_name) {
  div(
    h2(
      paste0("Due to limitations with the underlying R package {",
      package_name,
      "} it is not currently possible to run a regression nodesplit model within MetaInsight.")
    ),
    h3(
      "When this functionality becomes available, it will be added here."
    )
  )
}


#' Module server for the covariate regression nodesplit model page.
#' 
#' @param id ID of the module
covariate_nodesplit_page_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Placeholder server
  })
}