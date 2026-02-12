core_table_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    accordion(
      id = "collapse_table",
      open = FALSE,
      accordion_panel(
        title = "Exclude studies (Click to open / hide this panel)",
        "Click on a study arm in the plot to select studies to exclude in the sensitivity analysis.",
        verbatimTextOutput(ns("selected")),
        uiOutput(ns("plot"))
      )
    )
  )
}

core_table_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    output$plot <- renderUI({
      watch("setup_configure")
      req(common$configured_data)
      tagList(
        svg_container(
          summary_study_interactive(common$configured_data),
          style = "max-width: 1200px; margin: 0 auto;"
        ),
        tags$script(HTML(sprintf('
          $(document).ready(function() {
            var selectedClasses = [];

            $("#summary_exclude_interface g[id^=\'line\']").css("cursor", "pointer");

            $("#summary_exclude_interface g[id^=\'line\']").on("click", function() {
              var clickedClass = $(this).attr("class");

              // Toggle class selection
              var index = selectedClasses.indexOf(clickedClass);
              if (index > -1) {
                selectedClasses.splice(index, 1);
              } else {
                selectedClasses.push(clickedClass);
              }

              // Update rect opacity for all groups with the same class
              $("#summary_exclude_interface g." + clickedClass.replace(/\\s/g, ".")).each(function() {
                var rect = $(this).find("rect");
                if (selectedClasses.includes(clickedClass)) {
                  rect.css("opacity", "0.5");
                } else {
                  rect.css("opacity", "0.0");
                }
              });

              // Send selected classes to Shiny with namespaced id
              Shiny.setInputValue("%s", selectedClasses);
            });
          });
        ', ns("excluded_studies"))))
      )
    })

    output$selected <- renderPrint({
      input$excluded_studies
    })

  })
}
