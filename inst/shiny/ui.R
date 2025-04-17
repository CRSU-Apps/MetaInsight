resourcePath <- system.file("shiny", "www", package = "metainsight")
shiny::addResourcePath("resources", resourcePath)

tagList(
  rintrojs::introjsUI(),
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(
    script = file.path("resources", "js", "shinyjs-funcs.js"),
    functions = c("scrollLogger", "disableModule", "enableModule")
  ),
  navbarPage(
    theme = bslib::bs_theme(version = 5,
                            bootswatch = "spacelab"),
    id = "tabs",
    collapsible = TRUE,
    header = tagList(
      tags$head(tags$link(href = "css/styles.css", rel = "stylesheet"),
                # run current module on enter key press
                tags$script(HTML("$(document).on('keydown', function(e) {
                                    if (e.keyCode == 13) {
                                      Shiny.onInputChange('run_module', new Date().getTime());
                                    }
      });"))
      )),
    title = img(src = "logo.png", height = "50", width = "50",
                style = "margin-top: -15px"),
    windowTitle = "MetaInsight",
    tabPanel("Intro", value = "intro"),
    tabPanel("Setup", value = "setup"),
    tabPanel("Summary", value = "summary"),
    tabPanel("Frequentist", value = "freq"),
    tabPanel("Bayesian", value = "bayes"),
    tabPanel("Reproduce", value = "rep"),
    navbarMenu("Save", icon = icon("floppy-disk"),
               HTML('<a href="#" id="save-button" class="action-button btn" onclick="Shiny.setInputValue(\'core_save-save\', Math.random())">Save session</a>')),
    navbarMenu("Support", icon = icon("life-ring"),
               HTML('<a href="https://github.com/CRSU-Apps/MetaInsight/issues" target="_blank">GitHub Issues</a>'),
               HTML('<a href="mailto: info@crsu.org.uk" target="_blank">Send Email</a>'))
  ),
  tags$div(
    class = "container-fluid",
    fluidRow(
      column(
        4,
        wellPanel(
          conditionalPanel(
            "input.tabs == 'intro'",
            actionButton("debug_button", "debug"),
            textOutput("debug"),
            includeMarkdown("Rmd/text_intro_tab.Rmd")
          ),
          # SETUP ####
           conditionalPanel(
          "input.tabs == 'setup'",
          div("Component: Setup", class = "componentName"),
          help_comp_ui("setupHelp"),
          shinyWidgets::radioGroupButtons(
            "setupSel", "",
            choices = insert_modules_options("setup"),
            direction = "vertical",
            status = "outline-secondary",
            width = "100%"
          ),
          tags$hr(),
          insert_modules_ui("setup")
          ),
          # DATA SUMMARY ####
           conditionalPanel(
          "input.tabs == 'summary'",
          div("Component: Summary", class = "componentName"),
          help_comp_ui("summaryHelp"),
          shinyWidgets::radioGroupButtons(
            "summarySel", "",
            choices = insert_modules_options("summary", exclude = "summary_exclude"),
            direction = "vertical",
            status = "outline-secondary",
            width = "100%"
          ),
          tags$hr(),
          insert_modules_ui("summary", exclude = "summary_exclude")
          ),
          # FREQUENTIST ####
          conditionalPanel(
            "input.tabs == 'freq'",
            div("Component: Frequentist NMA", class = "componentName"),
            help_comp_ui("freqHelp"),
            shinyWidgets::radioGroupButtons(
              "freqSel", "",
              choices = insert_modules_options("freq"),
              direction = "vertical",
              status = "outline-secondary",
              width = "100%"
            ),
            tags$hr(),
            insert_modules_ui("freq")
          ),
          # BAYESIAN ####
          conditionalPanel(
            "input.tabs == 'bayes'",
            div("Component: Bayesian NMA", class = "componentName"),
            help_comp_ui("bayesHelp"),
            shinyWidgets::radioGroupButtons(
              "bayesSel", "",
              choices = insert_modules_options("bayes"),
              direction = "vertical",
              status = "outline-secondary",
              width = "100%"
            ),
            tags$hr(),
            insert_modules_ui("bayes")
          ),
          # REPRODUCIBILITY
          conditionalPanel(
            "input.tabs == 'rep'",
            div("Component: Reproduce", class = "componentName"),
            shinyWidgets::radioGroupButtons(
              "repSel", "",
              choices = insert_modules_options("rep"),
              direction = "vertical",
              status = "outline-secondary",
              width = "100%"
            ),
            tags$hr(),
            insert_modules_ui("rep")
          )
        )
      ),
      # --- RESULTS WINDOW ---
      column(
        8,
        conditionalPanel(
          "input.tabs != 'intro' & input.tabs != 'rep'",
          fixedRow(
            column(4,
               fluidRow(
                 column(6, summary_exclude_module_ui("summary_exclude")),
                 column(6, radioButtons("download_format",
                                        "Plot download format", choices = c("PDF" = "pdf",
                                                                            "PNG" = "png",
                                                                            "SVG" = "svg")))
               )
            ),
            column(
              2,
              uiOutput("processing")
            ),
            column(
              2,
              align = "left",
              div(style = "margin-top: -10px"),
              strong("Log window"),
              div(style = "margin-top: 5px"),
              div(
                id = "messageLog",
                div(id = "logHeader", div(id = "logContent"))
              )
            )
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
              "Users can use the filter box under each column of heading to select studies to exclude in the sensitivity analysis.",
              DT::dataTableOutput("table")
            )
          ),

          tabsetPanel(
            id = 'main',

            tabPanel(
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
            tabPanel(
              'Component Guidance', icon = icon("circle-info"),
              uiOutput('gtext_component')
            ),
            tabPanel(
              'Module Guidance', icon = icon("circle-info", class = "mod_icon"),
              uiOutput('gtext_module')
            ),
            tabPanel(
              'Code',
              core_code_module_ui("core_code")
            )
          )
        ),
        ## save module data END ##
        conditionalPanel(
          "input.tabs == 'rep' & input.repSel == null",
          column(8,
                 includeMarkdown("Rmd/gtext_rep.Rmd")
          )
        ),
        conditionalPanel(
          "input.tabs == 'rep' & input.repSel == 'rep_markdown'",
          column(8,
                 includeMarkdown("modules/rep_markdown.md")
          )
        ),
        conditionalPanel(
          "input.tabs == 'rep' & input.repSel == 'rep_refPackages'",
          column(8,
                 includeMarkdown("modules/rep_refPackages.md")
          )
        ),
        conditionalPanel(
          "input.tabs == 'intro'",
          tabsetPanel(
            id = 'introTabs',
            tabPanel(
              'About',
              br(),
              tags$div(
                style="text-align: center;",
                core_intro_module_ui("core_intro")
              ),
              includeMarkdown("Rmd/text_about.Rmd")
            ),
            tabPanel(
              'Team',
              fluidRow(
                column(8, includeMarkdown("Rmd/text_team.Rmd")
                )
              )
            ),
            tabPanel(
              'How To Use',
              includeMarkdown("Rmd/text_how_to_use.Rmd")
            )
          )
        )
      )
    )
  )
)


