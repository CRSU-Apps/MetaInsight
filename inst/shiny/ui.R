resourcePath <- system.file("shiny", "www", package = "metainsight")
shiny::addResourcePath("resources", resourcePath)

tagList(
  page_navbar(
    theme = bs_theme(version = 5,
                     bootswatch = "spacelab",
                     primary = "#e4042c",
                     secondary = "#005c8a",
                     info = "#005c8a"),
    id = "tabs",
    header = tagList(
      rintrojs::introjsUI(),
      shinyjs::useShinyjs(),
      shinyjs::extendShinyjs(
        script = file.path("resources", "js", "shinyjs-funcs.js"),
        functions = c("scrollLogger", "disableModule", "enableModule", "runOnEnter")
      ),
      tags$link(href = "css/styles.css", rel = "stylesheet"),
      includeHTML("favicon/favicon.html"),
      ),
    title = img(src = "logo.png", height = "40"),
    window_title = "MetaInsight",
    nav_panel("Intro", value = "intro"),
    nav_panel("Setup", value = "setup"),
    nav_panel("Summary", value = "summary"),
    nav_panel("Frequentist", value = "freq"),
    nav_panel("Bayesian", value = "bayes"),
    nav_panel("Baseline risk", value = "baseline"),
    nav_panel("Covariate", value = "covariate"),
    nav_panel("Reproduce", value = "rep"),
    nav_menu("Save", icon = icon("floppy-disk"),
               HTML('<a href="#" id="save-button" class="action-button btn" onclick="Shiny.setInputValue(\'core_save-save\', Math.random())">Save session</a>')),
    nav_menu("Support", icon = icon("life-ring"),
               HTML('<a href="https://github.com/CRSU-Apps/MetaInsight/issues" target="_blank">GitHub Issues</a>'),
               HTML('<a href="mailto: info@crsu.org.uk" target="_blank">Send Email</a>'))
  ),
  layout_sidebar(
    sidebar = sidebar(
      width = 400,
      open = "always",
      conditionalPanel(
        "input.tabs == 'intro'",
        # actionButton("debug_button", "debug"),
        # textOutput("debug"),
        core_intro_module_ui("core_intro"),
        includeMarkdown("Rmd/text_intro_tab.Rmd")
      ),
      insert_modules_ui("setup", "Setup the analysis", exclude = "setup_exclude"),
      insert_modules_ui("summary", "Summarise the data"),
      insert_modules_ui("freq", "Frequentist network meta-analysis"),
      insert_modules_ui("bayes", "Bayesian network meta-analysis"),
      insert_modules_ui("baseline", "Baseline risk analysis"),
      insert_modules_ui("covariate", "Covariate analysis"),
      insert_modules_ui("rep", "Reproduce the analysis")
      ),
      # --- RESULTS WINDOW ---
      conditionalPanel(
        "input.tabs != 'intro' & input.tabs != 'rep'",
          layout_columns(
            col_widths = c(4, 2, 6),
            div(id = "global_options",
            layout_columns(
              col_widths = c(6, 6),
                setup_exclude_module_ui("setup_exclude"),
                radioButtons("download_format",
                             "Plot download format",
                             choices = c("PDF" = "pdf",
                                         "PNG" = "png",
                                         "SVG" = "svg"))
            )),
              div(id = "processing_icon", uiOutput("processing")),
              div(style = "margin-top: -10px",
              strong("Log window"),
              div(style = "margin-top: 5px"),
              div(
                id = "messageLog",
                div(id = "logHeader", div(id = "logContent"))
              ))
        )
      ),
      br(),
      conditionalPanel(
        "input.tabs != 'intro' & input.tabs != 'rep'",
        accordion(
          id = "collapse_table",
          open = FALSE,
          accordion_panel(
            title = "Data table (Click to open / hide this panel)",
            "Use the filter box under each column of heading to select studies to exclude in the sensitivity analysis.",
            DT::dataTableOutput("table")
          )
        ),
        navset_tab(
          id = 'main',
          nav_panel(
            'Results',
            lapply(COMPONENTS, function(component) {
              conditionalPanel(
                glue::glue("input.tabs == '{component}'"),
                insert_modules_results(component)
              )
            }),
            # invisible but contains download button
            core_save_module_ui("core_save")
          ),
          nav_panel(
            'Guidance', icon = icon("circle-info", class = "mod_icon"),
            flex_wrap(uiOutput('gtext_module'))
          )
        )
      ),
      ## save module data END ##
      conditionalPanel(
        "input.tabs == 'rep' & input.repSel == null",
        flex_wrap(includeMarkdown("Rmd/gtext_rep.Rmd"))
      ),
      conditionalPanel(
        "input.tabs == 'rep' & input.repSel == 'rep_markdown'",
        flex_wrap(includeMarkdown("modules/rep_markdown.md"))
      ),
      conditionalPanel(
        "input.tabs == 'rep' & input.repSel == 'rep_renv'",
        flex_wrap(includeMarkdown("modules/rep_renv.md"))
      ),
      conditionalPanel(
        "input.tabs == 'rep' & input.repSel == 'rep_refPackages'",
        flex_wrap(includeMarkdown("modules/rep_refPackages.md"))
      ),
      conditionalPanel(
        "input.tabs == 'intro'",
        flex_wrap(
          navset_tab(
            id = 'introTabs',
            nav_panel(
              'About',
              # prevent warning about logo path
              suppressWarnings(includeMarkdown("Rmd/text_about.Rmd"))
            ),
            nav_panel(
              'Team',
               includeMarkdown("Rmd/text_team.Rmd")
            ),
            nav_panel(
              'How to use',
              includeMarkdown("Rmd/text_how_to_use.Rmd")
            )
          )
        )
      )
    )
)



