resourcePath <- system.file("shiny", "www", package = "metainsight")
shiny::addResourcePath("resources", resourcePath)

tagList(
  bslib::page_navbar(
    theme = bslib::bs_theme(version = 5,
                            bootswatch = "spacelab"),
    id = "tabs",
    header = tagList(
      rintrojs::introjsUI(),
      shinyjs::useShinyjs(),
      shinyjs::extendShinyjs(
        script = file.path("resources", "js", "shinyjs-funcs.js"),
        functions = c("scrollLogger", "disableModule", "enableModule", "runOnEnter")
      ),
      tags$head(tags$link(href = "css/styles.css", rel = "stylesheet"),
      )),
    title = img(src = "logo.png", height = "50", width = "50"),
    window_title = "MetaInsight",
    bslib::nav_panel("Intro", value = "intro"),
    bslib::nav_panel("Setup", value = "setup"),
    bslib::nav_panel("Summary", value = "summary"),
    bslib::nav_panel("Frequentist", value = "freq"),
    bslib::nav_panel("Bayesian", value = "bayes"),
    bslib::nav_panel("Reproduce", value = "rep"),
    bslib::nav_menu("Save", icon = icon("floppy-disk"),
               HTML('<a href="#" id="save-button" class="action-button btn" onclick="Shiny.setInputValue(\'core_save-save\', Math.random())">Save session</a>')),
    bslib::nav_menu("Support", icon = icon("life-ring"),
               HTML('<a href="https://github.com/CRSU-Apps/MetaInsight/issues" target="_blank">GitHub Issues</a>'),
               HTML('<a href="mailto: info@crsu.org.uk" target="_blank">Send Email</a>'))
  ),
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 400,
      open = "always",
      conditionalPanel(
        "input.tabs == 'intro'",
        actionButton("debug_button", "debug"),
        textOutput("debug"),
        includeMarkdown("Rmd/text_intro_tab.Rmd")
      ),
      insert_modules_ui("setup", "Setup"),
      insert_modules_ui("summary", "Summary", exclude = "summary_exclude"),
      insert_modules_ui("freq", "Frequentist NMA"),
      insert_modules_ui("bayes", "Bayesian NMA"),
      insert_modules_ui("rep", "Reproduce")
      ),
      # --- RESULTS WINDOW ---
      conditionalPanel(
        "input.tabs != 'intro' & input.tabs != 'rep'",
        bslib::layout_columns(
          col_widths = c(4, 2, 6),

            bslib::layout_columns(
              col_widths = c(6, 6),
              summary_exclude_module_ui("summary_exclude"),
              radioButtons("download_format",
                           "Plot download format",
                           choices = c("PDF" = "pdf",
                                       "PNG" = "png",
                                       "SVG" = "svg"))
          ),

            uiOutput("processing"),

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
        bslib::accordion(
          id = "collapse_table",
          open = FALSE,
          bslib::accordion_panel(
            title = "Data table (Click to open / hide this panel)",
            "Use the filter box under each column of heading to select studies to exclude in the sensitivity analysis.",
            DT::dataTableOutput("table")
          )
        ),

        bslib::navset_tab(
          id = 'main',

          bslib::nav_panel(
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
          bslib::nav_panel(
            'Component Guidance', icon = icon("circle-info"),
            flex_wrap(uiOutput('gtext_component'))
          ),
          bslib::nav_panel(
            'Module Guidance', icon = icon("circle-info", class = "mod_icon"),
            flex_wrap(uiOutput('gtext_module'))
          ),
          bslib::nav_panel(
            'Code',
            core_code_module_ui("core_code")
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
        "input.tabs == 'rep' & input.repSel == 'rep_refPackages'",
        flex_wrap(includeMarkdown("modules/rep_refPackages.md"))
      ),
      conditionalPanel(
        "input.tabs == 'intro'",
        flex_wrap(
          bslib::navset_tab(
            id = 'introTabs',
            bslib::nav_panel(
              'About',
              core_intro_module_ui("core_intro"),
              # prevent warning about logo path
              suppressWarnings(includeMarkdown("Rmd/text_about.Rmd"))
            ),
            bslib::nav_panel(
              'Team',
               includeMarkdown("Rmd/text_team.Rmd")
            ),
            bslib::nav_panel(
              'How To Use',
              includeMarkdown("Rmd/text_how_to_use.Rmd")
            )
          )
        )
      )
    )
)



