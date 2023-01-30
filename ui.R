###### Combined MEtaInsight ######

#install.packages(c("dplyr","metafor", "netmeta","shiny", "shinyAce","rmarkdown", "knitr", "shinydashboard", "gemtc"
#  , "shinyalert", "ggplot2", "plotly"))

# install.packages("pkgbuild")
# pkgbuild::has_build_tools()
# install.packages(c("remotes", "knitr", "devtools"))
# remotes::install_github("audrey-b/BUGSnet@v1.0.4", upgrade = TRUE, build_vignettes = TRUE)
# devtools::install_github("audrey-b/BUGSnet@v1.0.4", upgrade = TRUE, build_vignettes = TRUE)

library(dplyr)
library(metafor)
library(netmeta)
library(shiny)
library(shinyAce)
library(rmarkdown)
library(knitr)
library(shinydashboard)
library(gemtc)
library(shinyalert)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(shinyjs)
library(shinyBS)


source("PlotFunctionsRKO.R", local = TRUE) # Plot functions
load("blank.rds") # Objects to store data for plot functions

shinyUI(navbarPage(id="meta",
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
     h2("MetaInsight (including Bayesian estimates) V3.1.14 **", 
        #tags$sup("Beta", style="color:#6CC0ED"), 
        align= "left"),
     prettyRadioButtons("metaoutcome","Please select your outcome type:",
       c("Continuous (e.g. mean difference) " = "Continuous","Binary (e.g. Odds Ratio)" = "Binary"), 
       animation = "pulse", status = "info", width = '400px'),
     fluidRow(
       column(3, br(), br(),
       img(src='network2.jpg', width=500, height=400, align = "center")),
       column(2),
       column(5, 
       br(),
       p(tags$strong("Latest Updates:")),
       actionLink("history_click", "Click here to view a full update history of MetaInsight"),
       br(),
       tags$a(href="https://github.com/CRSU-Apps/MetaInsight/commits/main", "Click here to view the full version history of the code base for MetaInsight",target="_blank"),
       p(tags$strong("** Annotation correction on 25 October 2022 (v3.1.14) **:")),
       p(tags$ul(tags$li("We apologise that the annotation regarding the second option of network plot on tab 1c has been incorrect.
                         The size of the nodes do not represent the number of participants that were tested with the respective treatment, 
                         but instead represent the number of studies that included the respective treatment. 
                         This has now been corrected and we apologise for any inconvenience caused as a result."))),
       p(tags$strong("** Minor text change on 25 April 2022 (v3.1.13) **:")),
       p(tags$ul(tags$li("Sub headers have been added to deviance details in tab 3g-4."))),
       br(),
       p(tags$strong("Beta version available!", style="color:#6CC0ED; font-size:18px")),
       p("A beta version of MetaInsight is available, containing a ", tags$strong("new ranking panel"), " for Bayesian analyses. Check it out ", tags$a(href="https://crsu.shinyapps.io/MetaInsight_Beta", "here."))
       )),
       br(),
       p("Clareece Nevill, Yiqiao Xin, Rhiannon K Owen,  Naomi Bradbury, Nicola Cooper, and Alex Sutton", align= "left"),
       p("For feedback/questions about this app please contact Professor Alex Sutton", tags$a(href="mailto:ajs22@leicester.ac.uk", "ajs22@leicester.ac.uk", align= "left"), ". If you encounter any errors with using the app, please check  
         the",  actionLink("tsp", "trouble shooting page"), "first before contacting us."),
       br(),
      p("If you use the app please cite it as:"),
      p(tags$a(href=" https://doi.org/10.1002/jrsm.1373", "Owen, RK, Bradbury, N, Xin, Y, Cooper, N, Sutton, A. MetaInsight: An interactive web-based tool for analyzing, interrogating, 
        and visualizing network meta-analyses using R-shiny and netmeta. Res Syn Meth. 2019; 10: 569-581.",align="left")),
     br(),
    
       p("Codes for this app are available on Github. Please click ", tags$a(href="https://github.com/CRSU-Apps/MetaInsight", "here",target="_blank"), "to access."),
       br(),
       p("App powered by Rshiny.All frequentist statistical calculations are performed using R package netmeta (Gerta Rücker, Guido Schwarzer, Ulrike Krahn and Jochem König
        2017).", tags$a(href="http://CRAN.R-project.org/package=netmeta", "netmeta: Network Meta-Analysis using Frequentist Methods. R package version 0.9-8.",target="_blank"),
        "All Bayesian statistical calculations are performed using R package gemtc (Gert van Valkenhoef, Joel Kuiper 2016)",
        tags$a(href="https://cran.r-project.org/web/packages/gemtc/gemtc.pdf", "gemtc: Network Meta-Analysis Using Bayesian Methods R package version 0.8-2.",target="_blank"),
        "and R package BUGSNET (Audrey Beliveau, Devon J. Boyne, Justin Slater, Darren Brenner & Paul Arora)",
        tags$a(href="https://bugsnetsoftware.github.io/", "BUGSnet: Bayesian inference Using Gibbs Sampling to conduct NETwork meta-analysis version 1.0.3.
        ",target="_blank")),
       p("For users wishing to analyse large treatment networks or fit complex network meta-analysis models, please seek advice from technical experts."),
       br(),
       p("THE SOFTWARE IS PROVIDED AS IS, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
         NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
         IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
         WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
         OR THE USE OR OTHER DEALINGS IN THE SOFTWARE."),
     wellPanel(
       fluidRow(
         column(5, img(src='NIHR_Logo4.jpg', width=472, height=125)),
         column(7, tags$div(class="header", checked=NA,
                            tags$strong("Funding and Support Acknowledgement:"),
                            tags$p("The Complex Reviews Support Unit is funded by the National Institute for Health Research (NIHR) (project number 14/178/29).
                                   This app is also supported by the NIHR Applied Research Collaboration East Midlands (ARC EM)."),
                            tags$strong("Disclaimer: "),
                            tags$p("The views and opinions expressed herein are those of the authors and do not necessarily reflect those of the NIHR, NHS or the Department of Health and Social Care."),
                            tags$p("Please click ", tags$a(href="http://www.nihrcrsu.org", "here ",
                                                           target="_blank"), "for more information about the UK NIHR Complex Reviews Support Unit (CRSU).")
         ))))),


#########################
### Tab 2 - Load data ###
#########################

# Within the load data tab let users select a file to upload, the upload happens in a sidebarPanel on
# the left and the mainPanel will show the data once file uploaded.

tabPanel("Load Data",
   htmlOutput("CONBI"),
   tags$head(tags$style("#CONBI{color: white;
                           font-size: 20px;
                           font-style: bold;
                           background-color: #2196c4
                           }"
   )),
   br(),
   sidebarLayout(
     sidebarPanel(
       h4(tags$strong("Step 1 - Please select a data file (.csv) to upload")),
       br(),
       p(tags$strong("Note: Excel files should be saved in 'csv (Comma delimited) (*.csv)' format. Default maximum file size is 5MB.")),
       fileInput(inputId="data", label="", buttonLabel="Select", placeholder="No file selected"),
       br(),
       tags$hr(),
       h4(tags$strong("Step 2 - Please copy and paste the treatment labels")),
       br(),
       p(tags$strong("Note: The first row must be 'Number' tabspace 'Label' as shown in the pre-loaded format, case sensitive.")),
       p(tags$strong("      Treatment names may only contain letters, digits, and underscore (_).")),
       p(tags$strong("Tabspace does not work when directly typing into the texbox. Please copy and paste from a text or Excel file, or copy and paste from one of the pre-loaded rows.")),
       br(),
       conditionalPanel(condition= "input.metaoutcome=='Continuous'",
          aceEditor("listCont", value="Number\tLabel
1\tPlacebo
2\tOrlistat
3\tSibutramine
4\tMetformin
5\tOrli_Sibut
6\tRimonbant", mode="r" , theme="eclipse")   # note: no tabs allowed in front of the treatment labels. all the tabs will be relected as tabs in the input.
       ),
       conditionalPanel(condition = "input.metaoutcome=='Binary'",
          aceEditor("listbina", value="Number\tLabel
1\tNo_contact
2\tSelf_help
3\tIndividual_counselling
4\tGroup_counselling", mode="r", theme="eclipse")
       )
       ),
       mainPanel(
        tabsetPanel(id="instructions",
         tabPanel("Long format upload", 
            h2(tags$strong("Instructions for uploading long format data")),
            br(),
            p(tags$strong("MetaInsight allows data in either long format, or wide format. This tab provides instructions for long format data, where each row contains one treatment arm. 
                          Please follow Steps 1 and 2 to upload the data file and enter the treatment labels. 
                          Instructions are as below.")),
            h4(tags$strong("Step 1:")),
            p(),
            conditionalPanel(condition= "input.metaoutcome=='Continuous'",
              p("The long format data file should contain six columns. Headings of columns are case sensitive."), 
              p(tags$ul(tags$li("The", tags$strong("first"), "column should be labelled", tags$strong("StudyID"), "and contain the study identifier, starting from 1, then 2, 3, 4... etc."))),
              p(tags$ul(tags$li("The", tags$strong("second"), "column should be labelled", tags$strong("Study"), "and contain the name (e.g., author,year) of the study. The study name must be unique for each study."))),
              p(tags$ul(tags$li("The", tags$strong("third"), "column should be labelled", tags$strong("T"), "and contain the numerical treatment code used in each arm of the study.", 
                                tags$strong("If applicable, your reference treatment (e.g. Placebo/Control)"), tags$strong(tags$u("needs to be labelled as 1."))))),
              p(tags$ul(tags$li("The", tags$strong("fourth"), "column should be labelled", tags$strong("N"), "and contain the number of participants in each arm of the study."))),
              p(tags$ul(tags$li("The", tags$strong("fifth"), "column should be labelled", tags$strong("Mean"), "and contain the mean value of the outcome in each arm of the study."))),
              p(tags$ul(tags$li("The", tags$strong("sixth"), "column should be labelled", tags$strong("SD"), "and contain the standard deviation of the outcome in each arm of the study."))),
            ), 
            conditionalPanel(condition = "input.metaoutcome=='Binary'", 
              p("The long format data file should contain five columns. Headings of columns are case sensitive."), 
              p(tags$ul(tags$li("The", tags$strong("first"), "column should be labelled", tags$strong("StudyID"), "and contain the study identifier, starting from 1, then 2, 3, 4... etc."))),
              p(tags$ul(tags$li("The", tags$strong("second"), "column should be labelled", tags$strong("Study"), "and contain the name (e.g., author,year) of the study. The study name must be unique for each study."))),
              p(tags$ul(tags$li("The", tags$strong("third"), "column should be labelled", tags$strong("T"), "and contain the numerical treatment code used in each arm of the study.", 
                        tags$strong("If applicable, your reference treatment (e.g. Placebo/Control)"), tags$strong(tags$u("needs to be labelled as 1."))))),
              p(tags$ul(tags$li("The", tags$strong("fourth"), "column should be labelled", tags$strong("R"), 
                        "and contain the number of participants with the outcome of interest in each arm of the study."))),
              p(tags$ul(tags$li("The", tags$strong("fifth"), "column should be labelled", tags$strong("N"), "and contain the number of participants in each arm of the study."))),
              p("N.B. Continuity corrections will need to be applied to cells containing 0 values"),               
            ),
            p("An example of this structure can be seen in the", tags$strong("'View Data'"), "tab."),
            p("The csv file that is used to produce the example dataset can be downloaded from here:"),
            downloadButton("downloadData", "Download the example dataset in long format"),
            br(),
            h4(tags$strong("Step 2:")),
            p("Enter the labels to match with the numerical treatment codes in the data file. Labels should be short to allow for clear display on figures."),
            p("Data can be copy and pasted from Excel or another tab separated file such as '.txt'"),
            p("The default 'treatment labels' text file can be downloaded from here:"),
            downloadButton("downloadlabel", "Download the example 'treatment labels' text file"),
            br(),
            p(),
            conditionalPanel(condition = "input.metaoutcome=='Continuous'", 
              p(HTML(paste0("This default dataset for continuous outcome data is from Gray, LJ. et al. A systematic review and mixed treatment 
                comparison of pharmacological interventions for the treatment of obesity. Obesity reviews 13.6 (2012): 483-498.
                The continuous outcome used is BMI loss (kg/m",tags$sup("2"),") 3 months from baseline.")))
            ),
            conditionalPanel(condition = "input.metaoutcome=='Binary'", 
              p("This default dataset for binary outcome data is from Hasselblad, V. (1998), Meta-Analysis of Multi-Treatment Studies, 
                Medical Decision Making, 18, 37-43.
                The binary outcome used is smoking cessation."),
            ),
            br(),
            p(tags$strong("Note: The default dataset, pre-loaded on the 'View Data' tab, and its pre-loaded treatment labels will be used for analysis if no file is selected or no treatment labels are pasted. The 'View Data' tab will automatically update once a file is successfully loaded."))
            ),

         tabPanel("Wide format upload",
            h2(tags$strong("Instructions for uploading wide format data")),
            br(),
            p(tags$strong("MetaInsight allows data in either long format, or wide format. This tab provides instructions for wide format data, where each row contains all the treatment arms from one study. Please follow Steps 1 and 2 to upload the data file and enter the treatment labels. 
                          Instructions are as below.")),
            h4(tags$strong("Step 1:")),
            downloadButton("downloadDataWide", "Download the example dataset in wide format"), # Button
            br(),
            p("Your data needs to have exactly the same variable names as in the example data which can be downloaded from here:"),
            p("Headings of columns are case sensitive."),
            p(tags$ul(tags$li(tags$strong("StudyID"), "contains study identifier, starting from 1, then 2, 3, 4... etc."))),
            p(tags$ul(tags$li(tags$strong("Study"), "contains name (e.g., author,year) of the study. The study name must be unique for each study."))),
            p(tags$ul(tags$li(tags$strong("T.1, T.2, ..., up to T.6"), "contains treatment given for study arm 1, 2, ..., up to 6, respectively given as a numerical code"))),
            conditionalPanel(condition= "input.metaoutcome=='Continuous'",
              p(tags$ul(tags$li(tags$strong("N.1, N.2, ..., up to N.6"), "contains number of participants in study arm 1, 2, ..., up to 6, respectively"))),
              p(tags$ul(tags$li(tags$strong("Mean.1, Mean.2, ..., up to Mean.6"), "contains the mean value of the outcome in study arm 1, 2, ..., up to 6, respectively"))),
              p(tags$ul(tags$li(tags$strong("SD.1, SD.2, ..., up to SD.6"), "contains standard deviation of the outcome in study arm 1, 2, ..., up to 6, respectively"))),
            ),
            conditionalPanel(condition = "input.metaoutcome=='Binary'",
              p(tags$ul(tags$li(tags$strong("R.1, R.2, ..., up to R.6"), "contains number of participants with the outcome of interest in study arm 1, 2, ..., up to 6, respectively"))),
              p(tags$ul(tags$li(tags$strong("N.1, N.2, ..., up to N.6"), "contains number of participants in study arm 1, 2, ..., up to 6, respectively"))),
            ),
            p(tags$strong("Note: If applicable, your reference treatment (e.g. Placebo/Control)", 
                          tags$u("needs to be labelled as treatment 1"))),
            p(tags$strong("The maximum number of arms for each trial allowed in the MetaInsight app is 6.")),
            br(),
            h4(tags$strong("Step 2:")),
            p("Enter the labels to match with the numerical treatment codes in the data file. Labels should be short to allow for clear display on figures."),
            p("Data can be copy and pasted from Excel or another tab separated file such as '.txt'"),
            p("The default 'treatment labels' text file can be downloaded from here:"),
            downloadButton("downloadlabel2", "Download the example 'treatment labels' text file"),
            br(),
            p(),
            conditionalPanel(condition= "input.metaoutcome=='Continuous'",
              p("This default dataset is from Gray, LJ. et al. A systematic review and mixed treatment 
              comparison of pharmacological interventions for the treatment of obesity. Obesity reviews 13.6 (2012): 483-498."),
            ),
            conditionalPanel(condition = "input.metaoutcome=='Binary'",
              p("This default dataset for binary outcome data is from Hasselblad, V. (1998), Meta-Analysis of Multi-Treatment Studies, 
                Medical Decision Making, 18, 37-43.")
            ),
            br(),
            p(tags$strong("Note: The default dataset, pre-loaded on the 'View Data' tab, and its pre-loaded treatment labels 
                          will be used for analysis if no file is selected or no treatment labels are pasted. The 'View Data'
                          tab will automatically update once a file is successfully loaded."))
            ),
         tabPanel("View Data", 
                  p("Please double check if the total number of treatments matches the total number of treatment labels, 
                    i.e. make sure each treatment code in the data has a corresponding treatment label, 
                    and there is no additional treatment label which does not exist in the data."),
                  uiOutput("tb"))
    )))),

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
              column(6, plotOutput("netGraphStatic"),
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
            ))),
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
       tabPanel("3c. Ranking table",
                p("The MetaInsight team have developed a new exciting ", tags$strong("treatment ranking panel"), " to replace this ranking tab!", style="color:white; font-size:15px; background-color: #2196c4"),
                p("Access the new features in the Beta version by clicking ", tags$a(href="https://crsu.shinyapps.io/MetaInsight_Beta", "here.")),
                helpText("Please note: if you change the selections on the sidebar, 
                               you will need to re-run the primary and/or sensitivity analysis from the 'Forest Plot' page."),
            fixedRow(
              column(6, align = "center",
                     plotOutput("gemtc_rank", height = "700px")
              ),
              column(6, align = "center",
                     p(tags$strong("Ranking table for all studies - Probability for each treatment to be the best")),
                     div(tableOutput("prob"), style = "font-size:100%"),
                     downloadButton('downloadBaye_rank')
              )),
            fixedRow(
              column(6, align = "center",
                     plotOutput("gemtc_rank_sub", height = "700px")
                     
              ),
              column(6, align = "center",
                     p(tags$strong("Ranking table with studies excluded - Probability for each treatment to be the best")),
                     div(tableOutput("prob_sub"), style = "font-size:100%"),
                     downloadButton('downloadBaye_rank_sub')
              ))),
      tabPanel("3d. Nodesplit model",
               helpText("Please note: if you change the selections on the sidebar, 
                               you will need to re-run the primary and/or sensitivity analysis from the 'Forest Plot' page."),
           p("Please note: This may take more than 10 minutes depending on the number of treatment options. The node splitting option for
           the Bayesian analysis is highly numerically intensive and using it on the app can cause the app to disconnect in some circumstances.  
               We recommend people to download the whole app through",
               tags$a(href="https://github.com/CRSU-Apps/MetaInsight", "Github",target="_blank"), 
               "and run it locally through RStudio on their own machine if they want to make use of this function. 
             If you are not familiar with running ShinyApps in RStudio, please read this", tags$a(href="https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/", "tutorial", target="_blank"), "from Shiny."),
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
))))))))),

)
)
