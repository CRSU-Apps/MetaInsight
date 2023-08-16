###### MetaInsight ######

dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
  tags$head(
    shinyjs::useShinyjs(),
    # load custom stylesheet
    # To ensure white background and no horizontal scroll bars on ranking panel
    tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),
    includeHTML("www/favicon/favicon.html"),
    tags$meta(name="description", content="A interactive web tool for network meta-analysis (NMA) that leverages established analysis routines"),
    tags$meta(name="keywords", content="MetaInsight, NMA, Network, Meta, Analysis, App"),
    tags$meta(property="og:title", content= paste("Meta Insight: ",version)),
    tags$meta(property="og:description", content="An interactive web tool for network meta-analysis (NMA) that leverages established analysis routines"),
    tags$meta(property="og:image", content="https://raw.githubusercontent.com/CRSU-Apps/MetaInsight/main/www/images/MetaInsightLogo.png")
  ),
  navbarPage(id="meta",
                   "MetaInsight", 
                   header = singleton(tags$head(includeScript("google_analytics2.js"))),
                   tabPanel(id="home", "Home", 
                            tags$head(tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            ')),
                            # tags$style(     # to make the Shiny interface to zoom out 75% as default on Browser when loading as it looks better.
                            #   "
                            #    body {
                            #   -moz-transform: scale(0.75, 0.75); /* Moz-browsers */
                            #   zoom: 0.75; /* Other non-webkit browsers */
                            #   zoom: 75%; /* Webkit browsers */
                            #   }
                            #   "), 

    h2(paste("MetaInsight", version),
      #tags$sup("Beta", style="color:#6CC0ED"), 
      align= "left"),
    fluidRow(
      column(3, prettyRadioButtons("metaoutcome","Please select your outcome type:",
                      c("Continuous (e.g. mean difference) " = "Continuous","Binary (e.g. Odds Ratio)" = "Binary"), 
                      animation = "pulse", status = "info", width = '400px'),
             br(), br(),
            img(src='network2.jpg', width=500, height=400, align = "center")),
      column(2),
      column(5, 
            p(tags$strong("Latest Updates:")),
            p(tags$strong("Major update (15 August 2023 v5.0.0):")),
            p(tags$ul(
            tags$li("MetaInsight has been changed to make it easier for users to upload their own datasets for analysis. 
              Study data and treatment labels can now be uploaded in a single file.
              See the 'load data' tab for more guidance and the option to upgrade data files used in previous versions of MetaInsight "),
            tags$li("Summary forest plots are now available in frequentist analysis tab 2d")
            )),
            p(tags$strong("Minor update (11 July 2023 v4.2.0):")),
            p("A new video tutorial from ESMARConf2023 is available in the User Guide tab"),
            p("Click", tags$a(href = "https://github.com/CRSU-Apps/MetaInsight/wiki/Full-Update-History", "here", target="_blank"), "to view a full update history of MetaInsight"),
            p("The code for MetaInsight is available on", tags$a(href="https://github.com/CRSU-Apps/MetaInsight", "GitHub",target="_blank")),
            br(),
      )),
    br(),
    p("Clareece Nevill, Naomi Bradbury, Yiqiao Xin, Rhiannon K Owen, Ryan Field, Nicola Cooper, and Alex Sutton", align= "left"),
    p("For feedback/questions about this app please email the CRSU team at apps@crsu.org.uk. If you encounter any errors with using the app, please check  
       the",  actionLink("tsp", "trouble shooting page"), "first before contacting us."),
    br(),
    p("If you use the app please cite it as:"),
    p(tags$a(href=" https://doi.org/10.1002/jrsm.1373", "Owen, RK, Bradbury, N, Xin, Y, Cooper, N, Sutton, A. MetaInsight: An interactive web-based tool for analyzing, interrogating, 
      and visualizing network meta-analyses using R-shiny and netmeta. Res Syn Meth. 2019; 10: 569-581.",align="left")),
    br(),
    p("App powered by Rshiny.All frequentist statistical calculations are performed using R package netmeta (Gerta Rücker, Guido Schwarzer, Ulrike Krahn and Jochem König 2017).", 
      tags$a(href="http://CRAN.R-project.org/package=netmeta", "netmeta: Network Meta-Analysis using Frequentist Methods. R package version 0.9-8.",target="_blank"),
      "All Bayesian statistical calculations are performed using R package gemtc (Gert van Valkenhoef, Joel Kuiper 2016)",
      tags$a(href="https://cran.r-project.org/web/packages/gemtc/gemtc.pdf", "gemtc: Network Meta-Analysis Using Bayesian Methods R package version 0.8-2.",target="_blank"),
      "and R package BUGSNET (Audrey Beliveau, Devon J. Boyne, Justin Slater, Darren Brenner & Paul Arora)",
      tags$a(href="https://bugsnetsoftware.github.io/", "BUGSnet: Bayesian inference Using Gibbs Sampling to conduct NETwork meta-analysis version 1.0.3.",target="_blank")),
    p("For users wishing to analyse large treatment networks or fit complex network meta-analysis models, please seek advice from technical experts."),
    br(),
    p("THE SOFTWARE IS PROVIDED AS IS, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
      NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
      IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
      WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
      OR THE USE OR OTHER DEALINGS IN THE SOFTWARE."),
     wellPanel(
       img(src='CRSULogo.png', width = "100%"),
       tags$strong("Funding and Support Acknowledgement:"),
       tags$p("The Complex Reviews Support Unit is funded by the National Institute for Health Research (NIHR) (project number 14/178/29).
       Development of this app is also funded by the NIHR Applied Research Collaboration East Midlands (ARC EM) and the Leicester NIHR Biomedical Research Centre (BRC).
       The views expressed are those of the author(s) and not necessarily those of the NIHR or the Department of Health and Social Care."),
       tags$p("Please click ", tags$a(href="https://www.gla.ac.uk/research/az/evidencesynthesis/apps-materials-guidence/", "here ", target="_blank"), "for more information about the UK NIHR Complex Reviews Support Unit (CRSU).")
     )),
                   
#########################
### Tab 2 - Load data ###
#########################

# Within the load data tab let users select a file to upload, the upload happens in a sidebarPanel on
# the left and the mainPanel will show the data once file uploaded.

load_data_page_ui(id = 'load_data_page'),
                   
                   #############################
                   ### Tab 3 - Data analysis ###
                   #############################
                   
                   
                   tabPanel("Data analysis", id="dtanalysis",
                            htmlOutput("CONBI2"),
                            tags$head(tags$style("#CONBI2{color: white;
               font-size: 20px;
               font-style: bold;
               background-color: #2196c4
               }"
                            )),
                            br(),
                            sidebarLayout(
                              sidebarPanel(
                                conditionalPanel(condition= "input.metaoutcome=='Continuous'",
                                                 radioButtons("outcomeCont", "Outcome for continuous data:", c("Mean Difference (MD)" = "MD","Standardised Mean Difference (SMD)" = "SMD"))
                                ),
                                conditionalPanel(condition = "input.metaoutcome=='Binary'",
                                                 radioButtons("outcomebina", "Outcome for binary data:", c("Odds Ratio (OR)" = "OR","Risk Ratio (RR)" = "RR", "Risk Difference (RD)" = "RD"))
                                ),               
                                uiOutput("RankingPref"), 
                                radioButtons("modelranfix", "Model:", c("Random effect (RE)" = "random", "Fixed effect (FE)" = "fixed")),
                                h3("Select studies to exclude:"),
                                p("Tips: you can use the data table to help find the study that you want to exclude."),
                                actionButton("datatablebutton", "Open the data table"),
                                br(),
                                br(),
                                uiOutput("Choicesexcl"), 
                                h5("NB: If a whole treatment is removed from the analysis the NMA will return an error message. To overcome this, please remove the treatment from the data."), width = 3
                              ),
                              mainPanel(
                                bsCollapse(id = "collapse",
                                           bsCollapsePanel("Data table (Click to open / hide this panel)",
                                                           "Users can use the filter box under each column of heading to select studies to exclude in the sensitivity analysis.",
                                                           DT::dataTableOutput('datatb'),
                                                           style = "warning")),
                                tags$style(HTML("
            .tabbable > .nav > li > a                  {background-color: white;  color:#2196c4}
            .tabbable > .nav > li > a[data-value='1. Data summary'] {background-color: #2196c4;  color:white; font-size: 18px}
            .tabbable > .nav > li > a[data-value='1a. Study Results'] {background-color: white;}
            .tabbable > .nav > li > a[data-value='1b. Network Plot'] {background-color: white;}
            .tabbable > .nav > li > a[data-value='2. Frequentist network meta-analysis'] {background-color: #2196c4;   color:white; font-size: 18px}
            .tabbable > .nav > li > a[data-value='2a. Forest Plot'] {background-color: white}
            .tabbable > .nav > li > a[data-value='2b. Comparison of all treatment pairs'] {background-color: white;}
            .tabbable > .nav > li > a[data-value='2c. Inconsistency'] {background-color: white;}
            .tabbable > .nav > li > a[data-value='3. Bayesian network meta-analysis'] {background-color: #2196c4;   color:white; font-size: 18px}
            .tabbable > .nav > li[class=active]    > a {font-weight:900;font-style: italic;text-decoration: underline }
            ")),
       tabsetPanel(
         tabPanel("1. Data summary", tabsetPanel(
           tabPanel("1a. Data Characteristics", 
                    p("This tab shows a summary of study characteristics."),
                    column(6,
                           h4("Characteristics table of all studies"),
                           tableOutput("sumtb")
                    ),
                    column(6,
                           h4("Characteristics table with studies excluded"),
                           tableOutput("sumtb_sub")
                    )
           ),
            tabPanel("1b. Study Results", 
                     p("If the formatting of the text for this plot needs adjusting please see options at the bottom."), 
                     plotOutput("forestPlot", height = "1000px", width = "800px"), 
                     h5("If the formatting of the text in the above plot needs adjusting (for on screen or download) please use the following options:"),
                     column(4,numericInput(inputId="ForestContent",label="Download text size:", value=12)),
                     column(4,numericInput(inputId="ForestTitle",label="Title text size:",value=1)),
                     column(4,numericInput(inputId="ForestHeader",label="Group headers text size:", value=1)),
                     helpText("The download text size alters the text sizing for all elements of the plot and is integer point sizes. The title and group header options are an additional text sizing option in terms of percentage (e.g. 0.5 indicates half the specified size)."),
                     p("Please note: the formatting is not guaranteed to be identical between what is shown on screen and what is downloaded."),
                     radioButtons('format_freq0', 'Document format', c('PDF', 'SVG'), inline = TRUE),  
                     downloadButton('downloadStudy')),
            tabPanel("1c. Network Plot",
              column(6, plotOutput("netGraphStatic1"),
                     conditionalPanel(condition= "input.networkstyle=='networkp1'",
                                      p("Numbers on the line indicate the number of trials conducted for the comparison. The shaded areas indicate there exist multi-arm trials between the comparisons.")
                     ),
                     conditionalPanel(condition= "input.networkstyle=='networkp2'",
                                      p("The size of the nodes and thickness of edges represent the number of studies that examined a treatment and compared two given treatments respectively.")
                     ),
                     radioButtons("networkstyle", "Please choose a network plot style", c("Number of trials shown on the line" = "networkp1","Number of trials indicated by node size and line thickness" = "networkp2"), selected = "networkp2", width='100%'),
                     column(6, radioButtons('format_freq1', 'Document format', c('PDF', 'PNG'), inline = TRUE)),
                     column(6, numericInput('label_all', 'Label size multiplier', value=1.25)),
                     downloadButton('downloadNetwork'), 
                     br(),
                     br(),
                     br(),
                     verbatimTextOutput("netconnect")),
              column(6, plotOutput("netGraphUpdating"),
                     conditionalPanel(condition= "input.networkstyle_sub=='networkp1'",
                                      p("Numbers on the line indicate the number of trials conducted for the comparison. The shaded areas indicate there exist multi-arm trials between the comparisons.")
                     ),
                     conditionalPanel(condition= "input.networkstyle_sub=='networkp2'",
                                      p("The size of the nodes and thickness of edges represent the number of studies that examined a treatment and compared two given treatments respectively.")
                     ),
                     radioButtons("networkstyle_sub", "Please choose a network plot style", c("Number of trials shown on the line" = "networkp1","Number of trials indicated by node size and line thickness" = "networkp2"), selected = "networkp2", width='100%'),
                     column(6, radioButtons('format_freq2', 'Document format', c('PDF', 'PNG'), inline = TRUE)),
                     column(6, numericInput('label_excluded', 'Label size multiplier', value=1.25)),
                     downloadButton('downloadNetworkUpdate'), 
                     br(),
                     br(),
                     br(),
                     verbatimTextOutput("netconnect_sub"))
            )
  ###### bugsnet new code ########         
            #,
            # tabPanel("1d. Covariates plot", 
            #          p("this is from the TSD rheumatoid arthritis dataset, as the example data do not have covariates "),
            #          plotOutput("covp"))
  ###### finish ########
         )),
         tabPanel("2. Frequentist network meta-analysis", tabsetPanel(
            tabPanel("2a. Forest Plot",
                column(6, uiOutput("FreqForestPlot"), 
                       fixedRow(
                         p("Options to change limits of the x-axis:"),
                         column(6, align = 'center', numericInput('freqmin', label="Minimum", value=0.1)),
                         column(6, align = 'center', numericInput('freqmax', label="Maximum", value=5))
                       ),
                       textOutput("textcomp"), textOutput("ref4"), radioButtons('format_freq3', 'Document format', c('PDF', 'PNG'), inline = TRUE), downloadButton('downloadComp2')
                  ),
                column(6, uiOutput("FreqForestPlot_sub"),
                       fixedRow(
                         p("Options to change limits of the x-axis:"),
                         column(6, align = 'center', numericInput('freqmin_sub', label="Minimum", value=0.1)),
                         column(6, align = 'center', numericInput('freqmax_sub', label="Maximum", value=5))
                       ),
                       
                       tags$style("#ref_change {
               background-color: #ffd966;
               display:block; }"),
                                                    textOutput("ref_change"),
                                                    br(),
                                                    textOutput("text5"), textOutput("ref3"), radioButtons('format_freq4', 'Document format', c('PDF', 'PNG'), inline = TRUE), downloadButton('downloadComp'))
                                    ),
                                    tabPanel("2b. Comparison of all treatment pairs",
                                             helpText("Treatments are ranked from best to worst along the leading diagonal. Above the leading diagonal are estimates from pairwise meta-analyses, below the leading diagonal are estimates from network meta-analyses"),
                                             helpText("Relative treatment effects in ranked order for all studies"), tableOutput("rankChartStatic"), downloadButton('downloadRank', "Download"),
                                             helpText("Relative treatment effects in ranked order with studies excluded"), tableOutput("rankChartUpdating"), downloadButton('downloadRankUpdate')),
                                    tabPanel("2c. Inconsistency", 
                                             helpText("Assessment of inconsistency for all studies"), 
                                             tableOutput("Incon1"), downloadButton('downloadIncon', "Download"),
                                             helpText("Assessment of inconsistency with studies excluded"), 
                                             tableOutput("Incon2"), downloadButton('downloadIncon2', "Download")
                                    ),
                                    tabPanel("2d. Summary Forest Plot", summary_forest_plots_ui(id = '2d.summaryForestPlot')))),
                                  tabPanel("3. Bayesian network meta-analysis", tabsetPanel(id="tab",
            tabPanel("3a. Forest plot",
                    helpText("Baysesian result using the gemtc package.", tags$br(), 
                            "Heterogeneity prior: standard deviation ~ U(0,X), where X represents a ", tags$i("very large"), "difference in the analysis' outcome scale and is determined from the data.", tags$br(), tags$i("Please note the outcome for continuous data has to be "), tags$b("mean difference"), tags$i(" for the Bayesian analysis. 
                     Standardised mean difference cannot be analysed."), tags$br(), tags$i("Please note the outcome for binary data has to be "), tags$b("Odds Ratio or Risk Ratio"), tags$i(" for the Bayesian analysis. 
                     Risk difference cannot be analysed."), tags$strong("Please note each simulation may take 20 seconds.", style="color:#FF0000")),
            fixedRow(
              column(6, align = "center",
                     p(tags$strong("Results for all studies")),
                     p("Please click the button below to run Bayesian analysis for all studies, and after each time when you change the radiobutton selections."),
                     actionButton("baye_do", "Click here to run the main analysis for all studies")
              ),
              column(6, align = "center",
                     p(tags$strong("Results with studies excluded")),
                     p("Please click the button below to run each time after you finish the selection of studies, or change the radiobutton selections."),
                     actionButton("sub_do", "Click here to run the sensitivity analysis")
              )),
            fixedRow(
              column(6, align = "center",
                     uiOutput("BayesianForestPlot"),
                     fixedRow(
                       p("Options to change limits of the x-axis:"),
                       column(6, align = 'center', numericInput('bayesmin', label="Minimum", value=0.1)),
                       column(6, align = 'center', numericInput('bayesmax', label="Maximum", value=5))
                     ),
                     p("Model fit:"),
                     tableOutput("dic"),
                     textOutput("text_gemtc"),
                     br(),
                     br(),
                     radioButtons('format2', 'Document format', c('PDF', 'PNG'), inline = TRUE), 
                     downloadButton('downloadBaye_plot')
              ),
              column(6, align = "center",
                     uiOutput("BayesianForestPlot_sub"),
                     fixedRow(
                       p("Options to change limits of the x-axis:"),
                       column(6, align = 'center', numericInput('bayesmin_sub', label="Minimum", value=0.1)),
                       column(6, align = 'center', numericInput('bayesmax_sub', label="Maximum", value=5))
                     ),
                     tags$style("#ref_change_bay {
                                 background-color: #ffd966;
                                 display:block; }"),
                     textOutput("ref_change_bay"),
                     br(),
                     p("Model fit:"),
                     tableOutput("dic_sub"),
                     textOutput("text_gemtc_sub"),
                     br(),
                     br(),
                     radioButtons('format4', 'Document format', c('PDF', 'PNG'), inline = TRUE), 
                     downloadButton('downloadBaye_plot_sub')
                     ))),
              tabPanel("3b. Comparison of all treatment pairs",
                      helpText("Please note: if you change the selections on the sidebar, 
                               you will need to re-run the primary and/or sensitivity analysis from the 'Forest Plot' page."),
                      p(tags$strong("In contrast to the 'comparison of all treatment pairs' tab in the frequentist NMA results, 
                                    this table only contains the estimates from the network meta analysis, 
                                    i.e. does not contain estimates from pairwise meta-analysis which only contains direct evidence. 
                                    If you would like to obtain the pairwise meta-analysis results, please run 3d. Nodesplit model")),
              br(),
              p(tags$strong("Treatment effects for all studies: comparison of all treatment pairs.")),
              tableOutput("baye_comparison"),
              downloadButton('downloadbaye_comparison'),
              br(),
              br(),
              p(tags$strong("Treatment effects with studies excluded: comparison of all treatment pairs.")),
              tableOutput("baye_comparison_sub"),
              downloadButton('downloadbaye_comparison_sub')
              ),
        tabPanel("3c. Ranking Panel",
                helpText("Please note: if you change the selections on the sidebar, 
                         you will need to re-run the primary and/or sensitivity analysis from the 'Forest Plot' page.", 
                         tags$br(), 
                         tags$strong("Please note it may take up to 5 minutes to load the results.", style="color:#FF0000"),
                         tags$br(),
                         tags$strong("IMPORTANT: If you export and include the Litmus Rank-O-Gram or the Radial SUCRA plot in your work, please cite it as:", style="color:#4863A0"),
                         tags$a(href="https://doi.org/10.1016/j.jclinepi.2023.02.016", "Nevill CR, Cooper NJ, Sutton AJ, A multifaceted graphical display, including treatment ranking, was developed to aid interpretation of network meta-analysis, 
                              Journal of Clinical Epidemiology (2023)")),
                fluidRow(   
                    shinydashboard::box(title="Ranking panel for all studies", status='primary', solidHeader=TRUE, width=12, collapsible=TRUE,
                        splitLayout(cellWidths=c("30%","40%","30%"), cellArgs = list(style="height: 780px; padding: 16px; border: 2px solid gold; white-space: normal"),
                        fluidRow(align = "center", h4("Relative Effects"), shinycssloaders::withSpinner(plotOutput("gemtc2"), type=6),
                                textOutput("relative_rank_text"),
                                radioButtons('rank_forest_choice', 'Document format', c('PDF'='pdf', 'PNG'='png'), inline = TRUE), 
                                downloadButton('download_rank_forest')),
                        fluidRow(align = "center", h4("Ranking Results"), 
                                radioButtons("rank_plot_choice", label="Choice of Rank plot", choices=list("Litmus Rank-O-Gram"=0, "Radial SUCRA"=1), selected=0, inline=TRUE),
                                checkboxInput("Colour_blind", label="Display colour-blind friendly version", value=FALSE),
                                conditionalPanel(condition= "input.rank_plot_choice==0", shinycssloaders::withSpinner(plotOutput("Litmus"), type=6),
                                  p("Litmus Rank-O-Gram: Higher SUCRA (Surface Under the Cumulative Ranking Curve) values and cumulative ranking curves nearer the top left indicate better performance")),
                                conditionalPanel(condition="input.rank_plot_choice==1 && !input.Radial_alt", shinycssloaders::withSpinner(plotOutput("Radial"), type=6)),
                                conditionalPanel(condition="input.rank_plot_choice==1 && input.Radial_alt", shinycssloaders::withSpinner(plotOutput("RadialAlt"), type=6)),
                                conditionalPanel(condition="input.rank_plot_choice==1",
                                  checkboxInput("Radial_alt", label="Display simplified version", value=FALSE),
                                  p("Radial SUCRA plot: Higher SUCRA values indicate better treatments; size of nodes represent number of participants and thickness of lines indicate number of trials conducted")),
                                dropMenu(dropdownButton(circle=FALSE, status='warning', label="Ranking probabilities and SUCRA values for all treatments"),
                                tableOutput("rank_probs")), div(style="margin-bottom:10px"), #some padding between buttons
                                fluidRow(column(6, downloadButton('download_rank_plot', "Download Rank plot (PNG)", style = "width:220px; white-space: normal")), column(6, downloadButton('download_rank_table', label="Download table of rank probablities and SUCRA", style = "width:220px; white-space: normal")))
                                ),
                        fluidRow(align = "center", h4("Summary of evidence"), 
                                 shinycssloaders::withSpinner(plotOutput("netGraphStatic1_rank"), type=6),
                                conditionalPanel(condition= "input.networkstyle_rank=='networkp1'",
                                                p("Numbers on the line indicate number of trials conducted for the comparison. Any shaded areas indicate existence of multi-arm trials between the comparisons.")
                                ),
                                conditionalPanel(condition= "input.networkstyle_rank=='networkp2'",
                                                p("The size of the nodes and thickness of edges represent the number of studies that examined a treatment and compared two given treatments respectively.")
                                ),
                                radioButtons("networkstyle_rank", "Please choose a network plot style", c("Number of trials shown on the line" = "networkp1","Number of trials indicated by node size and line thickness" = "networkp2"), inline=TRUE),
                                radioButtons('network_rank_choice', 'Document format', c('PDF'='pdf', 'PNG'='png'), inline = TRUE),
                                downloadButton('download_network_rank')
                                ))
                )),
                fluidRow(   
                    shinydashboard::box(title="Ranking panel with studies excluded", status='primary', solidHeader=TRUE, width=12, collapsible=TRUE,
                        splitLayout(cellWidths=c("30%","40%","30%"), cellArgs = list(style="height: 780px; padding: 16px; border: 2px solid gold; white-space: normal"),
                        fluidRow(align = "center", h4("Relative Effects"), shinycssloaders::withSpinner(plotOutput("gemtc_sub2"), type=6),
                                textOutput("relative_rank_text_sub"),
                                radioButtons('rank_forest_choice_sub', 'Document format', c('PDF'='pdf', 'PNG'='png'), inline = TRUE), 
                                downloadButton('download_rank_forest_sub')),
                        fluidRow(align = "center", h4("Ranking Results"), 
                                radioButtons("rank_plot_choice_sub", label="Choice of Rank plot", choices=list("Litmus Rank-O-Gram"=0, "Radial SUCRA"=1), selected=0, inline=TRUE),
                                checkboxInput("Colour_blind_sub", label="Display colour-blind friendly version", value=FALSE),
                                conditionalPanel(condition="input.rank_plot_choice_sub==0", shinycssloaders::withSpinner(plotOutput("Litmus_sub"), type=6),
                                 p("Litmus Rank-O-Gram: Higher SUCRA (Surface Under the Cumulative Ranking Curve) values and cumulative ranking curves nearer the top left indicate better performance")),
                                conditionalPanel(condition="input.rank_plot_choice_sub==1 && !input.Radial_alt_sub", shinycssloaders::withSpinner(plotOutput("Radial_sub"), type=6)),
                                conditionalPanel(condition="input.rank_plot_choice_sub==1 && input.Radial_alt_sub", shinycssloaders::withSpinner(plotOutput("RadialAlt_sub"), type=6)),
                                conditionalPanel(condition="input.rank_plot_choice_sub==1",
                                                checkboxInput("Radial_alt_sub", label="Display simplified version", value=FALSE),
                                                p("Radial SUCRA plot: Higher SUCRA values indicate better treatments; size of nodes represent number of participants and thickness of lines indicate number of trials conducted")),
                                dropMenu(dropdownButton(circle=FALSE, status='warning', label="Ranking probabilities and SUCRA values for all treatments"),
                                        tableOutput("rank_probs_sub")), div(style="margin-bottom:10px"), #some padding between buttons
                                        fluidRow(column(6, downloadButton('download_rank_plot_sub', "Download Rank plot (PNG)", style = "width:220px; white-space: normal")), column(6, downloadButton('download_rank_table_sub', label="Download table of rank probablities and SUCRA", style = "width:220px; white-space: normal")))),
                        fluidRow(align = "center", h4("Summary of evidence"), 
                                 shinycssloaders::withSpinner(plotOutput("netGraphStatic1_rank_sub"), type=6),
                                conditionalPanel(condition= "input.networkstyle_rank_sub=='networkp1'",
                                  p("Numbers on the line indicate number of trials conducted for the comparison. Any shaded areas indicate existence of multi-arm trials between the comparisons.")
                                ),
                                conditionalPanel(condition= "input.networkstyle_rank_sub=='networkp2'",
                                 p("The size of the nodes and thickness of edges represent the number of studies that examined a treatment and compared two given treatments respectively.")
                                ),
                                radioButtons("networkstyle_rank_sub", "Please choose a network plot style", c("Number of trials shown on the line" = "networkp1","Number of trials indicated by node size and line thickness" = "networkp2"), inline=TRUE),
                                radioButtons('network_rank_choice_sub', 'Document format', c('PDF'='pdf', 'PNG'='png'), inline = TRUE),
                                downloadButton('download_network_rank_sub')
                        ))
        ))),
        tabPanel("3d. Nodesplit model",
                helpText("Please note: if you change the selections on the sidebar, 
                          you will need to re-run the primary and/or sensitivity analysis from the 'Forest Plot' page."),
                p("Please note: This may take more than 10 minutes depending on the number of treatment options. The node splitting option for
                  the Bayesian analysis is highly numerically intensive and using it on the app can cause the app to disconnect in some circumstances.  We have produced a",
                  tags$a(href="https://github.com/CRSU-Apps/MetaInsight/wiki/Local-User-Guide", "guide",target="_blank"), 
                  "to running MetaInsight locally through RStudio on the user's own machine if they want to make use of this function."),
                  fluidRow(
                    column(6,
                          p(tags$strong("Inconsistency test with notesplitting model for all studies")),
                         actionButton("node", "Click here to run the nodesplitting analysis for all studies"),
                         tableOutput("node_table"),
                         downloadButton('downloadnode')
                    ),
                    column(6,
                          p(tags$strong("Inconsistency test with notesplitting model with studies excluded")),
                          actionButton("node_sub", "Click here to run the nodesplitting analysis with studies excluded"),
                          tableOutput("node_table_sub"),
                          downloadButton('downloadnode_sub')
                ))),
        tabPanel("3e. Bayesian result details",
                helpText("Please note: if you change the selections on the sidebar, 
                        you will need to re-run the primary and/or sensitivity analysis from the 'Forest Plot' page."),
                fluidRow(
                  column(6,
                        p(tags$strong("Results details for all studies")),
                        verbatimTextOutput("gemtc_results"),
                        p(tags$strong("Gelman convergence assessment plot for all studies")),
                        plotOutput("gemtc_gelman")
                  ),
                  column(6,
                        p(tags$strong("Results details with studies excluded")),
                        verbatimTextOutput("gemtc_results_sub"),
                        p(tags$strong("Gelman convergence assessment plot with studies excluded")),
                        plotOutput("gemtc_gelman_sub"))
                  )),        
        tabPanel("3f. Deviance report",
                helpText("Please note: if you change the selections on the sidebar, 
                         you will need to re-run the primary and/or sensitivity analysis from the 'Forest Plot' page."),
          p(tags$strong("Deviance report for all studies and the sensitivity analysis")),
          fluidRow(
           column(6,
                  p(tags$strong("Residual deviance from NMA model and UME inconsistency model for all studies")),
                  plotlyOutput("dev_scat")),
           column(6,
                  p(tags$strong("Residual deviance from NMA model and UME inconsistency model with studies excluded")),
                  plotlyOutput("dev_scat_sub")
           )),
           p("This plot represents each data points' contribution to the residual deviance for the 
          NMA with consistency (horizontal axis) and the unrelated mean effect (ume) inconsistency models 
          (vertical axis) along with the line of equality. The points on the equality line means there is no
          improvement in model fit when using the inconsistency model, suggesting that there is no evidence of inconsistency. 
          Points above the equality line means they have a smaller residual deviance for the consistency model indicating a 
          better fit in the NMA consistency model and points below the equality line
          means they have a better fit in the ume inconsistency model. Please note that the unrelated mean effects model 
          may not handle multi-arm trials correctly. (Further reading: Dias S, Ades AE, Welton NJ, Jansen JP, Sutton AJ. Network meta-anlaysis for 
            decision-making. Chapter 3 Model fit, model comparison and outlier detection. @2018 John Wiley & Sons Ltd.)"),
                                                                                                     br(),
                                                                                                     br(),
                                                                                                     br(),
                                                                                                     fluidRow(
                                                                                                       column(6,
                                                                                                              p(tags$strong("Per-arm residual deviance for all studies")),
                                                                                                              plotlyOutput("dev1")),
                                                                                                       column(6,
                                                                                                              p(tags$strong("Per-arm residual deviance for sensitivity analysis")),
                                                                                                              plotlyOutput("dev1_sub")
                                                                                                       ),
                                                                                                       br(),
                                                                                                       p("This stem plot represents the posterior residual deviance per study arm. The total number of stems equals 
             the total number of data points in the network meta analysis. Going from left to right, the alternating symbols 
             on the stems indicate the different studies. Each stem corresponds to the residual deviance ($dev.ab) associated with each 
             arm in each study. The smaller residual deviance (the shorter stem), dev.ab, the better model fit for each 
             data point. You can identify which stem corresponds to which study arm by hovering on the stem symbols. 
             (Further reading: Dias S, Ades AE, Welton NJ, Jansen JP, Sutton AJ. Network meta-anlaysis for 
             decision-making. Chapter 3 Model fit, model comparison and outlier detection. @2018 John Wiley & Sons Ltd.)"),
                                                                                                       br(),
                                                                                                       br(),
                                                                                                       br(),
                                                                                                       column(6,
                                                                                                              p(tags$strong("Leverage plot for all studies")),
                                                                                                              plotlyOutput("dev2")
                                                                                                       ),
                                                                                                       column(6,
                                                                                                              p(tags$strong("Leverage plot for sensitivity analysis")),
                                                                                                              plotlyOutput("dev2_sub"))
                                                                                                     ),
                                                                                                     br(),
                                                                                                     p("This leverage plot shows the average leverage across the arms for each study ({sum($lev.ab)}/{number of arms} 
          for each study) versus the square root of the average residual deviance across the arms for each study 
          (sqrt({sum($dev.ab)}/{number of arms}) for each study).  
             The leverage for each data point, is calculated as the posterior mean of the residual 
             deviance, minus the deviance at the posterior mean of the fitted values. The leverage plot may be used to 
             identify influential and/or poorly fitting studies and can be used to check how each study is affecting 
             the overall model fit and DIC. Curves of the form x2 + y = c, c = 1, 2, 3, ., where x represents square root 
             of residual deviance, and y represents the leverage, are marked on the plot. Points lying on such parabolas 
             each contribute an amount c to the DIC (Spiegelhalter et al., 2002). Points that lie outside the line with 
             c = 3 can generally be identified as contributing to the model's poor fit. Points with a high leverage are 
             influential, which means that they have a strong influence on the model parameters that generate their fitted 
             values. (Further reading: Dias S, Ades AE, Welton NJ, Jansen JP, Sutton AJ. Network meta-anlaysis for 
             decision-making. Chapter 3 Model fit, model comparison and outlier detection. @2018 John Wiley & Sons Ltd. 
             Spiegelhalter et al. (2002) Bayesian measures of model complexity and fit. J. R. Statist. Soc.B 64, Part4, 
             pp.583-639)"),
                                                                                                     br(),
                                                                                                     br()
                                                                                            ),
                                                                                            tabPanel("3g. Model details",
                                                                                                     helpText("Please note: if you change the selections on the sidebar, 
                               you will need to re-run the primary and/or sensitivity analysis from the 'Forest Plot' page."),
               tabsetPanel(
        tabPanel("3g-1. Model codes",
                p(tags$strong("Model codes for analysis of all studies")),
                downloadButton('download_code'),
                verbatimTextOutput("code")
                ),
        tabPanel("3g-2. Initial values",
                p(tags$strong("Initial values")),
                downloadButton('download_inits_1', "Download initial values for chain 1"),
                downloadButton('download_inits_2', "Download initial values for chain 2"),
                downloadButton('download_inits_3', "Download initial values for chain 3"),
                downloadButton('download_inits_4', "Download initial values for chain 4"),
                verbatimTextOutput("inits")
                ),
        tabPanel("3g-3. Download simulations",
                 p(tags$strong("Download simulated data")),
                 downloadButton('download_data1', "Download data from chain 1"),
                 br(),
                 downloadButton('download_data2', "Download data from chain 2"),
                 br(),
                 downloadButton('download_data3', "Download data from chain 3"),
                 br(),
                 downloadButton('download_data4', "Download data from chain 4")
        ),
        tabPanel("3g-4. Deviance details",
        fluidRow(
         column(6,
                p(tags$strong("Deviance data for all studies")),
                p("NMA (consistency) model"),
                verbatimTextOutput("dev")),
         column(6,
                p(tags$strong("Deviance data for sensitivity analysis")),
                p("NMA (consistency) model"),
               verbatimTextOutput("dev_sub")
         )),
        fluidRow(
               column(6, p("UME (inconsistency) model"), verbatimTextOutput("dev_ume")),
               column(6, p("UME (inconsistency) model"), verbatimTextOutput("dev_ume_sub")
               ))   
)))))), width=9))),

###################################
### Tab 4 - Download report     ###
###################################

tabPanel("Report",
         p("Wait until MetaInsight has completed any analysis before clicking the generate report button"),
         downloadButton("report", "Generate report")
),

###################################
### Tab 5 - User Guide ###
###################################

tabPanel("User Guide",
         h2 (tags$strong("User Guide")),
         h4 (tags$strong("User Guide", style = "color: #2196c4")),
                            p("Click the button below to download a pdf copy of the MetaInsight User Guide."),
                            p(tags$strong("Please note:"), 
                              "the user guide is based on version 3 of MetaInsight. Some elements of the app have been changed since the guide was originally produced."),
                            downloadButton("UG", "Download User Guide"),
                            br(),
                            br(),
         h4 (tags$strong("ESMARConf 2023 Tutorial", style = "color: #2196c4")),
         p(tags$strong(" MetaInsight - an R Shiny web-app for conducting network meta-analysis")),
         p("A tutorial for MetaInsight v4.0.0 produced for ESMARConf 2023"),
         HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/g-RDnQ75Hv4" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>'),
         br(),
         br(),
         h4 (tags$strong("Treatment Ranking Demo", style = "color: #2196c4")),
         p("A short demo video of how to use the Bayesian analysis ranking panel in tab 3c"),
         HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/scbLwTY0kvc" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
         br(),
         br(),
         h4 (tags$strong("Cochrane Training Webinar", style = "color: #2196c4")),
         p(tags$strong("MetaInsight: Background, introduction, demonstration, limitations, and future plans")),
         p("These videos were recorded live in 2019 as part of the ",
           tags$a(href="https://training.cochrane.org/resource/metainsight-complex-review-support-unit-crsu-network-meta-analysis-nma-web-based-app", "Cochrane Training network meta-analysis learning live webinar series.",target="_blank"),
           "They are intended for people who are interested in undertaking a network meta-analysis using MetaInsight.",
           tags$strong("Please note:"), "these videos refer to a past version of MetaInsight and elements of the app have changed since the videos were originally recorded."),
         br(),
         HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/RR_tkICQv_s" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
         HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/b-fYoUdksRo" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
         HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/g0n5yxQ4Z34" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
         )
,


###################################
### Tab 6 - Troublehshooting.   ###
###################################

tabPanel(id="trouble", "Troubleshooting",
#          tags$div(
# includeHTML("troublesh.html")
#          )
tags$iframe(style = "height:1500px; width:100%; scrolling=yes",
            src = "trouble_shooting.pdf")
),

##############################
### Tab 7 - Privacy notice ###
##############################



tabPanel(id="privacy", "Privacy notice",
   tags$iframe(style="height:1500px; width:100%; scrolling=yes",
               src="gdpr.pdf")
)
)
)
)
