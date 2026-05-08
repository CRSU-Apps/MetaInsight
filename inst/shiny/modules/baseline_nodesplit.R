baseline_nodesplit_module_ui <- function(id) {
 p(
   "Due to limitations with the underlying R package {bnma} it is not currently possible to run a regression nodesplit model within MetaInsight.
   When this functionality becomes available, it will be added here."
 )
}

baseline_nodesplit_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

})
}


