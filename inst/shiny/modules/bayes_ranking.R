bayes_ranking_submodule_ui <- function(id, download_label) {
  ns <- NS(id)
  tagList(
    div(class = "bayes_ranking_download",
      div(
        style = "height: 40px; margin-bottom: 10px; display: flex; text-align: center; align-items: flex-end; justify-content: center;",
        tags$label(download_label)
      ),
      div(
        class = "d-grid gap-2",
        downloadButton(ns("download_forest"), "Forest plot", class = "btn-block"),
        downloadButton(ns("download_ranking_plot"), "Ranking plot (PNG only)", class = "btn-block"),
        downloadButton(ns("download_ranking_table"), "Ranking table", class = "btn-block"),
        downloadButton(ns("download_network"), "Network graph", class = "btn-block")
      )
    )
  )
}

bayes_ranking_module_ui <- function(id) {
  ns <- shiny::NS(id)
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
      #p("Radial SUCRA plot: Higher SUCRA values indicate better treatments; size of nodes represent number of participants and thickness of lines indicate number of trials conducted")
    ),
    input_task_button(ns("run"), "Generate plots", type = "default", icon = icon("arrow-turn-down")),
    fixedRow(
      column(
        width = 6,
        bayes_ranking_submodule_ui(ns("all"), "All studies:")
      ),
      column(
        width = 6,
        bayes_ranking_submodule_ui(ns("sub"), "With selected studies excluded:")
      )
    )
  )
}

bayes_ranking_submodule_server <- function(id, common, network_style, rank_style, colourblind, simple, data, treatments, run, trigger){
  moduleServer(id, function(input, output, session) {

    init(trigger)
    shinyjs::hide(selector = ".bayes_ranking_download")

    observeEvent(run(),{
      req(common[[paste0("bayes_", id)]])
      common[[paste0("bayes_rank_", id)]] <- bayes_ranking(common[[data]], common$outcome, common[[treatments]], common[[paste0("bayes_", id)]], common$ranking_option)
      trigger(trigger)
    })

    n_trt <- reactive({
      req(watch(trigger) > 0)
      if (id == "all"){
        return(nrow(common$treatment_df))
      } else {
        return(nrow(common$subsetted_treatment_df))
      }
    })

    # this enables the plot to always fit in the column width
    output$forest <- renderUI({
      req(watch(trigger) > 0)
      req(common[[paste0("bayes_", id)]], run())

      plot_height <- forest_height_pixels(n_trt(), title = TRUE) / 72
      common$meta$bayes_ranking[[paste0("forest_height_", id)]] <- plot_height

      svg_text <- svglite::xmlSVG(
        bayes_forest(common[[paste0("bayes_", id)]]),
        height = plot_height,
        width = 6.5)

      div(class = "svg_container",
        HTML(paste(svg_text, collapse = "\n"))
      )
    })

    regression_text <- reactive("")

    ranking_plots <- eventReactive(watch(trigger), {
      req(watch(trigger) > 0)

      plots <- list(
        litmus = LitmusRankOGram(common[[paste0("bayes_rank_", id)]], colourblind = FALSE, regression_text = regression_text()),
        radial = RadialSUCRA(common[[paste0("bayes_rank_", id)]], colourblind = FALSE, regression_text = regression_text()),
        litmus_blind = LitmusRankOGram(common[[paste0("bayes_rank_", id)]], colourblind = TRUE, regression_text = regression_text()),
        radial_blind = RadialSUCRA(common[[paste0("bayes_rank_", id)]], colourblind = TRUE, regression_text = regression_text())
      )
      return(plots)
    })

    output$ranking <- renderPlot({
      req(watch(trigger) > 0)
      shinyjs::show(selector = ".bayes_ranking_download")
      if (rank_style() == "litmus" && colourblind() == FALSE){
        return(ranking_plots()$litmus)
      }
      if (rank_style() == "radial" && colourblind() == FALSE && simple() == FALSE){
        return(ranking_plots()$radial$Original)
      }
      if (rank_style() == "radial" && colourblind() == FALSE && simple() == TRUE){
        return(ranking_plots()$radial$Alternative)
      }
      if (rank_style() == "litmus" && colourblind() == TRUE){
        return(ranking_plots()$litmus_blind)
      }
      if (rank_style() == "radial" && colourblind() == TRUE && simple() == FALSE){
        return(ranking_plots()$radial_blind$Original)
      }
      if (rank_style() == "radial" && colourblind() == TRUE && simple() == TRUE){
        return(ranking_plots()$radial_blind$Alternative)
      }
    })


    output$ranking_table <- renderTable({
      ranking_table(common[[paste0("bayes_rank_", id)]])
      }, digits = 2)

    output$download_ranking_table <- downloadHandler(
      filename = paste0("MetaInsight_bayesian_ranking_table_", id, ".csv"),
      content = function(file) {
        write.csv(
          ranking_table(common[[paste0("bayes_rank_", id)]]),
          file,
          row.names = FALSE
        )
      }
    )

    output$network <- renderUI({
      req(watch(trigger) > 0)

      svg_text <- svglite::xmlSVG(
        summary_network(common[[paste0("freq_", id)]], common[[paste0("bugsnet_", id)]], network_style()),
        width = 5,
        height = 5
      )

      div(class = "svg_container",
          HTML(paste(svg_text, collapse = "\n"))
      )

    })

    output$download_forest <- downloadHandler(
      filename = function() {
        paste0("MetaInsight_bayesian_forest_plot_", id, ".", common$download_format)
      },
      content = function(file) {

        plot_func <- function(){
          bayes_forest(common[[paste0("bayes_", id)]])
        }

        write_plot(file, common$download_format, plot_func, width = 6.5, height = as.integer(forest_height_pixels(n_trt(), title = TRUE) / 72) + 0.5)
      }
    )

    output$download_ranking_plot <- downloadHandler(
      filename = function() {
        paste0("MetaInsight_bayesian_ranking_plot_", id, ".png")
      },
      content = function(file) {

        if (rank_style() == "litmus" && colourblind() == FALSE){
          ggplot2::ggsave(file, ranking_plots()$litmus, width = 6, height = 6, units = "in")
        }
        if (rank_style() == "radial" && colourblind() == FALSE && simple() == FALSE){
          ggplot2::ggsave(file, ranking_plots()$radial$Original, width = 6, height = 6, units = "in")
        }
        if (rank_style() == "radial" && colourblind() == FALSE && simple() == TRUE){
          ggplot2::ggsave(file, ranking_plots()$radial$Alternative, width = 6, height = 6, units = "in")
        }
        if (rank_style() == "litmus" && colourblind() == TRUE){
          ggplot2::ggsave(file, ranking_plots()$litmus_blind, width = 6, height = 6, units = "in")
        }
        if (rank_style() == "radial" && colourblind() == TRUE && simple() == FALSE){
          ggplot2::ggsave(file, ranking_plots()$radial_blind$Original, width = 6, height = 6, units = "in")
        }
        if (rank_style() == "radial" && colourblind() == TRUE && simple() == TRUE){
          ggplot2::ggsave(file, ranking_plots()$radial_blind$Alternative, width = 6, height = 6, units = "in")
        }
      }
    )

    output$download_network <- downloadHandler(
      filename = function() {
        paste0("MetaInsight_network_plot_", id, ".", common$download_format)
      },
      content = function(file) {

        plot_func <- function(){
          summary_network(common[[paste0("freq_", id)]], common[[paste0("bugsnet_", id)]], network_style())
        }

        write_plot(file, common$download_format, plot_func, width = 5, height = 5)
      }
    )

  })
}

bayes_ranking_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    # check that a fitted model exists and error if not
    observeEvent(input$run, {
      if (is.null(common$bayes_all)){
        common$logger %>% writeLog(type = "error", "Please fit the Bayesian models first")
        return()
      } else {
        trigger("bayes_ranking")
      }
    })

    # listen for the _sub model being refitted and trigger again, but only if the module has already been used
    on("bayes_model_sub", {
      if (watch("bayes_ranking") > 0){
        shinyjs::runjs("Shiny.setInputValue('bayes_ranking-rerun', new Date().getTime());")
      }
    })

    # trigger for the main analysis - when run is clicked, but only if there is a valid model
    all_trigger <- reactive({
      if (watch("bayes_ranking") > 0){
        common$meta$bayes_ranking$used <- TRUE
        return(list(input$run))
      }
    })

    # trigger for the sub analysis - when run is clicked or the model reruns, but only if there is a valid model
    sub_trigger <- reactive({
      if (watch("bayes_ranking") > 0){
        return(list(input$run, input$rerun))
      }
    })

    # put these in an observe so that they are updated whenever the choices change
    observe({
      # METADATA ####
      common$meta$bayes_ranking$colourblind <- input$colourblind
      common$meta$bayes_ranking$simple <- input$simple
      common$meta$bayes_ranking$network_style <- input$network_style
      common$meta$bayes_ranking$rank_style <- input$rank_style
    })

    bayes_ranking_submodule_server("all", common, reactive(input$network_style), reactive(input$rank_style), reactive(input$colourblind), reactive(input$simple),
                                   "main_connected_data", "treatment_df", all_trigger, "bayes_ranking_all")
    bayes_ranking_submodule_server("sub", common, reactive(input$network_style), reactive(input$rank_style), reactive(input$colourblind), reactive(input$simple),
                                   "subsetted_data", "subsetted_treatment_df", sub_trigger, "bayes_ranking_sub")

    return(list(
      save = function() {
        # Save any values that should be saved when the current session is saved
        # Populate using save_and_load()
      },
      load = function(state) {
        # Load
        # Populate using save_and_load()
      }
    ))

  })
}


bayes_ranking_submodule_result <- function(id, title) {
  ns <- NS(id)
  tagList(
    accordion(
      open = TRUE,
      accordion_panel(
        title = title,
        splitLayout(
          cellWidths = c("30%", "40%", "30%"),
          cellArgs = list(style = "height: 500px; padding: 16px; border: 2px solid #005c8a; white-space: normal"),
          fluidRow(
            align = "center",
              uiOutput(ns("forest"))
          ),
          fluidRow(
            align = "center",
              plotOutput(ns("ranking")), # table_label = table_label)
            shinyWidgets::dropMenu(
              shinyWidgets::dropdownButton(
                circle = FALSE,
                status = "default",
                label = "Ranking probabilities and SUCRA values for all treatments"
              ),
              tableOutput(ns("ranking_table"))
            )
          ),
          fluidRow(
            align = "center",
              uiOutput(ns("network"))
          )
        )
      )
    )
  )
}

bayes_ranking_module_result <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      bayes_ranking_submodule_result(ns("all"),
                                     title = "Ranking panel for all studies")
    ),
    fluidRow(
      bayes_ranking_submodule_result(ns("sub"),
                                     title = "Ranking panel with selected studies excluded")
    )
  )
}


bayes_ranking_module_rmd <- function(common){ list(
  bayes_ranking_knit = !is.null(common$meta$bayes_ranking$used),
  bayes_ranking_forest_height_all = common$meta$bayes_ranking$forest_height_all,
  bayes_ranking_forest_height_sub = common$meta$bayes_ranking$forest_height_sub,
  bayes_ranking_colourblind = common$meta$bayes_ranking$colourblind,
  bayes_ranking_simple = common$meta$bayes_ranking$simple,
  bayes_ranking_network_style = common$meta$bayes_ranking$network_style,
  bayes_ranking_rank_style = common$meta$bayes_ranking$rank_style)
}



