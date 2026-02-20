baseline_ranking_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bayes_ranking_module_ui("baseline_ranking")
  )
}

baseline_ranking_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    hide_and_show(id, show = FALSE)

    # check that a fitted model exists and error if not
    observeEvent(input$run, {
      if (is.null(common$baseline_model)){
        common$logger |> writeLog(type = "error", go_to = "baseline_model", "Please fit the baseline model first")
        return()
      } else {
        trigger("baseline_ranking")
      }
    })

    # trigger for the main analysis - when run is clicked, but only if there is a valid model
    all_trigger <- reactive({
      if (watch("baseline_ranking") > 0){
        return(list(watch("baseline_ranking"), watch("baseline_model_fit")))
      }
    })

    # put these in an observe so that they are updated whenever the choices change
    observe({
      if (watch("baseline_ranking") > 0){
        # METADATA ####
        common$meta$baseline_ranking$used <- TRUE
        common$meta$baseline_ranking$colourblind <- input$colourblind
        common$meta$baseline_ranking$simple <- input$simple
        common$meta$baseline_ranking$network_style <- input$network_style
        common$meta$baseline_ranking$rank_style <- input$rank_style
      }
    })

    bayes_ranking_submodule_server("all", common, reactive(input$network_style), reactive(input$rank_style), reactive(input$colourblind), reactive(input$simple),
                                   ".baseline_ranking_div", "baseline_model", "baseline_ranking", "configured_data", all_trigger, "baseline_ranking_plot")

    return(list(
      save = function() {list(
        ### Manual save start
        ### Manual save end
        colourblind = input$colourblind,
        simple = input$simple,
        network_style = input$network_style,
        rank_style = input$rank_style)
      },
      load = function(state) {
        ### Manual load start
        ### Manual load end
        updateCheckboxInput(session, "colourblind", value = state$colourblind)
        updateCheckboxInput(session, "simple", value = state$simple)
        updateRadioButtons(session, "network_style", selected = state$network_style)
        updateRadioButtons(session, "rank_style", selected = state$rank_style)
      }
    ))
  })
}


baseline_ranking_module_result <- function(id) {
  ns <- NS(id)
  fluidRow(
    p("If you export and include the Litmus Rank-O-Gram or the Radial SUCRA plot in your work, please cite it as:
        Nevill CR, Cooper NJ, Sutton AJ, A multifaceted graphical display, including treatment ranking, was developed
        to aid interpretation of network meta-analysis, Journal of Clinical Epidemiology (2023)", class = "baseline_ranking_div"),
    bayes_ranking_submodule_result(ns("all"), "Ranking panel for all studies", "baseline_ranking_div")
  )
}


baseline_ranking_module_rmd <- function(common) {list(
  baseline_ranking_knit = !is.null(common$meta$baseline_ranking$used),
  baseline_ranking_colourblind = common$meta$baseline_ranking$colourblind,
  baseline_ranking_simple = common$meta$baseline_ranking$simple,
  baseline_ranking_network_style = common$meta$baseline_ranking$network_style,
  baseline_ranking_rank_style = common$meta$baseline_ranking$rank_style)
}

