bayes_ranking_submodule_ui <- function(id, download_label) {
  ns <- NS(id)
  tagList(
    downloadButton(ns("download_effects"), "Effects graph"),
    downloadButton(ns("download_rank_plot"), "Rank plot"),
    downloadButton(ns("download_rank_table"), "Rank table"),
    downloadButton(ns("download_network"), "Network graph")
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
    checkboxInput(ns("colourblind"), "Disarrow-turn-down colourblind-friendly ranking plot"),
    conditionalPanel(
      condition = "input.rank_style == 1",
      ns = ns,
      checkboxInput(ns("simple"), label = "Disarrow-turn-down simplified ranking plot", value = FALSE),
      #p("Radial SUCRA plot: Higher SUCRA values indicate better treatments; size of nodes represent number of participants and thickness of lines indicate number of trials conducted")
    ),
    actionButton(ns("run"), "Generate plots"),
    fixedRow(
      column(
        width = 6,
        bayes_ranking_submodule_ui(ns("all"), "All studies")
      ),
      column(
        width = 6,
        bayes_ranking_submodule_ui(ns("sub"), "With selected studies excluded")
      )
    )
  )
}

bayes_ranking_submodule_server <- function(id, common, network_style, rank_style, colourblind, simple, data, treatments, run, trigger){
  moduleServer(id, function(input, output, session) {

    init(trigger)

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
      watch(trigger)
      req(common[[paste0("bayes_", id)]], run())

      outfile <- tempfile(fileext = ".svg")
      svglite::svglite(filename = outfile,
                       height = forest_height_pixels(n_trt(), title = TRUE) / 72,
                       width = 6.5)
      bayes_forest(common[[paste0("bayes_", id)]])
      dev.off()

      svg_text <- readLines(outfile)
      file.remove(outfile)
      div(class = "svg_container",
        HTML(paste(svg_text, collapse = "\n"))
      )
    })

    regression_text <- reactive("")

    ranking_plots <- reactive({
      req(watch(trigger) > 0)

      plots <- list(
        litmus = LitmusRankOGram(common[[paste0("bayes_rank_", id)]], colourblind = FALSE, regression_text = regression_text()),
        radial = RadialSUCRA(common[[paste0("bayes_rank_", id)]], colourblind = FALSE, regression_text = regression_text()),
        litmus_blind = LitmusRankOGram(common[[paste0("bayes_rank_", id)]], colourblind = TRUE, regression_text = regression_text()),
        radial_blind = RadialSUCRA(common[[paste0("bayes_rank_", id)]], colourblind = TRUE, regression_text = regression_text())
      )
      shinyjs::hide(selector = ".bayes_sub")
      return(plots)
    })

    output$ranking <- renderPlot({
      req(watch(trigger) > 0)
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

    output$network <- renderPlot({
      req(watch(trigger) > 0)
      summary_network(common[[paste0("freq_", id)]], common[[paste0("bugsnet_", id)]], network_style())
    })

    # output$download <- downloadHandler(
    #   filename = function() {
    #     if (model == "bayes_all"){
    #       name <- "MetaInsight_bayesian_forest_plot_all."
    #     } else {
    #       name <- "MetaInsight_bayesian_forest_plot_sub."
    #     }
    #     paste0(name, common$download_format)
    #   },
    #   content = function(file) {
    #
    #     plot_func <- function(){
    #       bayes_ranking(common[[model]])
    #       title(main = title)
    #     }
    #
    #     write_plot(file, common$download_format, plot_func, width = 9, height = as.integer(forest_height_pixels(n_trt(), title = TRUE) / 72) + 0.5)
    #
    #   }
    # )

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
        shinyjs::show("bayes_ranking_results", asis = TRUE)
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
        return(list(input$run))
      }
    })

    # trigger for the sub analysis - when run is clicked or the model reruns, but only if there is a valid model
    sub_trigger <- reactive({
      if (watch("bayes_ranking") > 0){
        return(list(input$run, input$rerun))
      }
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


bayes_ranking_submodule_result <- function(id, title, output_class) {
  ns <- NS(id)
  tagList(
    accordion(
      open = TRUE,
      accordion_panel(
        title = title,
#
#     shinydashboard::box(
#       title = title,
#       status = 'primary',
#       solidHeader = TRUE,
#       width = 12,
#       collapsible = TRUE,
        splitLayout(
          cellWidths = c("30%", "40%", "30%"),
          cellArgs = list(style = "height: 500px; padding: 16px; border: 2px solid #005c8a; white-space: normal"),
          fluidRow(
            align = "center",
            div(loading_spinner(output_class),
                uiOutput(ns("forest")))
          ),
          fluidRow(
            align = "center",
            div(loading_spinner(output_class),
                plotOutput(ns("ranking")))#, table_label = table_label)
          ),
          fluidRow(
            align = "center",
            div(loading_spinner(output_class),
                  plotOutput(ns("network")))
          )
        )
      )
    )
  )
}

bayes_ranking_module_result <- function(id) {
  ns <- NS(id)
  tagList(
    div(id = "bayes_ranking_results", style = "disarrow-turn-down: none;",
      fluidRow(
        bayes_ranking_submodule_result(ns("all"),
                                       title = "Ranking panel for all studies",
                                       output_class = "all_output bayes_all")
      ),
      fluidRow(
        bayes_ranking_submodule_result(ns("sub"),
                                       title = "Ranking panel with selected studies excluded",
                                       output_class = "sub_output bayes_sub")
      )
    )
  )
}


bayes_ranking_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  # Populate using metadata()
}



