bayes_ranking_submodule_ui <- function(id, download_label, class) {
  ns <- NS(id)
  tagList(
    div(class = class,
      div(
        style = "height: 40px; margin-bottom: 10px; display: flex; text-align: center; align-items: flex-end; justify-content: center;",
        tags$label(download_label)
      ),
      div(
        class = "d-grid gap-2",
        downloadButton(ns("download_forest"), "Forest plot", class = "btn-block"),
        downloadButton(ns("download_ranking_plot"), "Ranking plot", class = "btn-block"),
        downloadButton(ns("download_ranking_table"), "Ranking table", class = "btn-block"),
        downloadButton(ns("download_network"), "Network graph", class = "btn-block")
      )
    )
  )
}

bayes_ranking_module_ui <- function(id) {
  ns <- NS(id)

  download_buttons <- function(id){
    class <- paste0(id, "_div")
    if (id == "bayes_ranking"){
      fixedRow(
        column(
          width = 6,
          bayes_ranking_submodule_ui(ns("all"), "All studies:", class)
        ),
        column(
          width = 6,
          bayes_ranking_submodule_ui(ns("sub"), "With selected studies excluded:", class)
        )
      )
    } else {
      bayes_ranking_submodule_ui(ns("all"), "Downloads:", class)
    }

  }

  tagList(
    radioButtons(ns("network_style"),
                 label = "Network plot style",
                 choices = c(
                   "Number of trials shown on the line" = "netgraph",
                   "Number of trials indicated by node size and line thickness" = "netplot"
                 ),
                 inline = TRUE
    ),
    radioButtons(ns("rank_style"),
                 label = "Ranking plot style",
                 choices = c(
                   "Litmus Rank-O-Gram" = "litmus",
                   "Radial SUCRA" = "radial"
                 ),
                 inline = TRUE
    ),
    checkboxInput(ns("colourblind"), "Display colourblind-friendly ranking plot"),
    conditionalPanel(
      condition = "input.rank_style == 'radial'",
      ns = ns,
      checkboxInput(ns("simple"), label = "Display simplified ranking plot", value = FALSE),
    ),
    input_task_button(ns("run"), "Generate plots", type = "default", icon = icon("arrow-turn-down")),
    download_buttons(id)
  )
}

bayes_ranking_submodule_server <- function(id, common, network_style, rank_style, colourblind, simple, class, model, ranking, connected_data, treatments, run, trigger){
  moduleServer(id, function(input, output, session) {

    init(trigger)

    observeEvent(run(),{
      req(common[[model]])

      if (ranking == "covariate_ranking"){
        cov_value <- common$covariate_value
      } else {
        cov_value <- NA
      }
      common[[ranking]] <- bayes_ranking(common[[connected_data]], common[[treatments]], common[[model]], common$ranking_option, cov_value)
      trigger(trigger)
    })

    forest_svg <- reactive({
      req(watch(trigger) > 0)
      req(common[[model]], run())
      tdf <- ifelse(id == "all", "treatment_df", "subsetted_treatment_df")

      if (ranking == "baseline_ranking"){
        baseline_forest(common[[model]],
                        common[[tdf]],
                        common[[paste0("reference_treatment_", id)]],
                        title = "")
      } else {
        bayes_forest(common[[model]],
                     common[[tdf]],
                     common[[paste0("reference_treatment_", id)]],
                     ranking = TRUE)
      }

    })

    output$forest <- renderUI({
      req(forest_svg())
      div(class = "svg_container_ranking",
        forest_svg()
      )
    })

    ranking_svg <- reactive({
      req(watch(trigger) > 0)

      if (rank_style() == "litmus"){
        return(LitmusRankOGram(common[[ranking]], colourblind = colourblind()))
      }
      if (rank_style() == "radial"){
        return(RadialSUCRA(common[[ranking]], original = !simple(), colourblind = colourblind()))
      }

    })

    output$ranking_text <- renderText({
      if (rank_style() == "litmus"){
        return("Litmus Rank-O-Gram: Higher SUCRA (Surface Under the Cumulative Ranking Curve) values and cumulative ranking curves nearer the top left indicate better performance")
      }
      if (rank_style() == "radial"){
        return("Radial SUCRA plot: Higher SUCRA values indicate better treatments; size of nodes represent number of participants and thickness of lines indicate number of trials conducted")
      }
    })

    output$ranking <- renderUI({
      req(ranking_svg())
      on.exit(shinyjs::show(selector = class))

      div(class = "svg_container_ranking",
          ranking_svg()
      )
    })

    # enable shinyjs::show to work
    outputOptions(output, "ranking", suspendWhenHidden = FALSE)

    output$ranking_table <- renderTable({
      ranking_table(common[[ranking]])
      }, digits = 2)

    output$download_ranking_table <- downloadHandler(
      filename = paste0("MetaInsight_bayesian_ranking_table_", id, ".csv"),
      content = function(file) {
        write.csv(
          ranking_table(common[[ranking]]),
          file,
          row.names = FALSE
        )
      }
    )

    network_svg <- reactive({
      req(watch(trigger) > 0)
      summary_network(common[[paste0("freq_", id)]], common[[paste0("bugsnet_", id)]], network_style(), 1, "")
    })

    output$network <- renderUI({
      req(network_svg())
      div(class = "svg_container_ranking",
          network_svg()
      )

    })

    output$download_forest <- downloadHandler(
      filename = function() {
        paste0("MetaInsight_bayesian_forest_plot_", id, ".", common$download_format)
      },
      content = function(file) {
        write_plot(forest_svg(),
                   file,
                   common$download_format
                   )
      }
    )

    output$download_ranking_plot <- downloadHandler(
      filename = function() {
        paste0("MetaInsight_bayesian_ranking_plot_", id, ".", common$download_format)
      },
      content = function(file) {
        write_plot(ranking_svg(),
                   file,
                   common$download_format
        )
      }
    )

    output$download_network <- downloadHandler(
      filename = function() {
        paste0("MetaInsight_network_plot_", id, ".", common$download_format)
      },
      content = function(file) {
        write_plot(network_svg(),
                   file,
                   common$download_format
        )
      }
    )

  })
}

bayes_ranking_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    hide_and_show(id, show = FALSE)

    # check that a fitted model exists and error if not
    observeEvent(input$run, {
      if (is.null(common$bayes_all)){
        common$logger |> writeLog(type = "error", go_to = "bayes_model", "Please fit the Bayesian models first")
        return()
      } else {
        trigger("bayes_ranking")

      }
    })

    # trigger for the main analysis - when run is clicked, but only if there is a valid model
    all_trigger <- reactive({
      if (watch("bayes_ranking") > 0){
        return(list(watch("bayes_ranking"), watch("bayes_model_all")))
      }
    })

    # trigger for the sub analysis - when run is clicked or the model reruns, but only if there is a valid model
    sub_trigger <- reactive({
      if (watch("bayes_ranking") > 0){
        return(list(watch("bayes_ranking"), watch("bayes_model_sub")))
      }
    })

    # update whenever the choices change
    observe({
      if (watch("bayes_ranking") > 0){
        # METADATA ####
        common$meta$bayes_ranking$used <- TRUE
        common$meta$bayes_ranking$colourblind <- input$colourblind
        common$meta$bayes_ranking$simple <- input$simple
        common$meta$bayes_ranking$network_style <- input$network_style
        common$meta$bayes_ranking$rank_style <- input$rank_style
      }
    })

    bayes_ranking_submodule_server("all", common, reactive(input$network_style), reactive(input$rank_style), reactive(input$colourblind), reactive(input$simple),
                                   ".bayes_ranking_div", "bayes_all", "bayes_rank_all", "main_connected_data", "treatment_df", all_trigger, "bayes_ranking_all")
    bayes_ranking_submodule_server("sub", common, reactive(input$network_style), reactive(input$rank_style), reactive(input$colourblind), reactive(input$simple),
                                   ".bayes_ranking_div", "bayes_sub", "bayes_rank_sub", "subsetted_data", "subsetted_treatment_df", sub_trigger, "bayes_ranking_sub")

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


bayes_ranking_submodule_result <- function(id, title, class) {
  ns <- NS(id)
  tagList(
    div(class = class,
      accordion(
        open = TRUE,
        accordion_panel(
          title = title,
          div(style = "display: flex; gap: 16px;",
            div(style = "flex: 0 0 30%; padding: 16px; border: 2px solid #005c8a; white-space: normal;",
              fluidRow(
                align = "center",
                  h4("Relative effects"),
                  uiOutput(ns("forest"))
              )
            ),
            div(style = "flex: 0 0 40%; padding: 16px; border: 2px solid #005c8a; white-space: normal;",
              fluidRow(
                align = "center",
                  h4("Ranking results"),
                  textOutput(ns("ranking_text")),
                  uiOutput(ns("ranking")),
                shinyWidgets::dropMenu(
                  shinyWidgets::dropdownButton(
                    circle = FALSE,
                    status = "default",
                    label = "Ranking probabilities and SUCRA values",
                    inputId = ns("dropdown")
                  ),
                  tableOutput(ns("ranking_table"))
                )
              )
            ),
            div(style = "flex: 0 0 30%; padding: 16px; border: 2px solid #005c8a; white-space: normal;",
              fluidRow(
                align = "center",
                  h4("Summary of evidence"),
                  uiOutput(ns("network"))
              )
            )
          )
        )
      )
    )
  )
}

bayes_ranking_module_result <- function(id) {
  ns <- NS(id)
  tagList(
    p("If you export and include the Litmus Rank-O-Gram or the Radial SUCRA plot in your work, please cite it as:
        Nevill CR, Cooper NJ, Sutton AJ, A multifaceted graphical display, including treatment ranking, was developed
        to aid interpretation of network meta-analysis, Journal of Clinical Epidemiology (2023)", class = "bayes_ranking_div"),
    fluidRow(
      bayes_ranking_submodule_result(ns("all"), "Ranking panel for all studies", "bayes_ranking_div")
    ),
    fluidRow(
      bayes_ranking_submodule_result(ns("sub"), "Ranking panel with selected studies excluded", "bayes_ranking_div")
    )
  )
}


bayes_ranking_module_rmd <- function(common){ list(
  bayes_ranking_knit = !is.null(common$meta$bayes_ranking$used),
  bayes_ranking_colourblind = common$meta$bayes_ranking$colourblind,
  bayes_ranking_simple = common$meta$bayes_ranking$simple,
  bayes_ranking_network_style = common$meta$bayes_ranking$network_style,
  bayes_ranking_rank_style = common$meta$bayes_ranking$rank_style)
}



