
reload_app <- function(app, path){
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_reload")
  app$upload_file("setup_reload-load_session" = path)
  app$click("setup_reload-goLoad_session")
  app$wait_for_js("$('.sweet-alert.visible').length > 0")
  app$click(selector = ".confirm")
}
