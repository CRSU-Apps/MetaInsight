
# reload from a save file and close alert
reload_app <- function(app, path){
  app$set_inputs(tabs = "setup")
  app$set_inputs(setupSel = "setup_reload")
  app$upload_file("setup_reload-load_session" = path)
  app$click("setup_reload-goLoad_session")
  app$wait_for_js("$('.sweet-alert.visible').length > 0")
  app$click(selector = ".confirm")
}

# click on the exclusions forest plot
click_setup_exclude <- function(app, study){
  app$run_js(sprintf('
  var elem = document.querySelector(\'[data-study-name="%s"]\');
  if (elem) {
    elem.dispatchEvent(new MouseEvent("click", {
      bubbles: true,
      cancelable: true,
      view: window
    }));
  }
  ', study))
}
