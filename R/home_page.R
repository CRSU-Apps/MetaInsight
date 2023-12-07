
#' Module UI for the home page.
#' 
#' @param id ID of the module
#' @return Div for the home page
home_page_ui <- function(id) {
  ns <- NS(id)
  div(
    tags$head(
      tags$script('
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
                            ')
    ),
    h2("MetaInsight v5.1.1", align= "left"),
    fluidRow(
      column(
        width = 3,
        prettyRadioButtons(
          inputId = ns("metaoutcome"),
          label = "Please select your outcome type:",
          choices = c(
            "Continuous (e.g. mean difference) " = "Continuous",
            "Binary (e.g. Odds Ratio)" = "Binary"
          ),
          animation = "pulse",
          status = "info",
          width = '400px'
        ),
        br(),
        br(),
        img(src='network2.jpg', width=500, height=400, align = "center")
      ),
      column(width = 2),
      column(
        width = 5, 
        p(tags$strong("Latest Updates:")),
        p(tags$strong("Patch (07 December 2023 v5.1.1):")),
        p("Fixed minor bugs"),
        p(tags$strong("Major update (15 August 2023 v5.0.0):")),
        p(tags$ul(
          tags$li("MetaInsight has been changed to make it easier for users to upload their own datasets for analysis. 
         Study data and treatment labels can now be uploaded in a single file.
         See the 'load data' tab for more guidance and the option to upgrade data files used in previous versions of MetaInsight ")
        )),
        p(
          "The full update history for MetaInsight is available on",
          tags$a(href="https://github.com/CRSU-Apps/MetaInsight/wiki/Full-Update-History", "GitHub",target="_blank")
        ),
        p(
          "The code for MetaInsight is available on",
          tags$a(href="https://github.com/CRSU-Apps/MetaInsight", "GitHub",target="_blank")
        ),
        br(),
      )
    ),
    br(),
    p(
      "Naomi Bradbury, Ryan Field, Tom Morris, Clareece Nevill, Janion Nevill, Yiqiao Xin, Rhiannon K Owen, Nicola Cooper, and Alex Sutton",
      align = "left"
    ),
    p(
      "For feedback/questions about this app please email the CRSU team at apps@crsu.org.uk. If you encounter any errors with using the app, please check the",
      actionLink("tsp", "trouble shooting page"),
      "first before contacting us."
    ),
    br(),
    p("If you use the app please cite it as:"),
    p(
    tags$a(
      href = " https://doi.org/10.1002/jrsm.1373",
      "Owen, RK, Bradbury, N, Xin, Y, Cooper, N, Sutton, A. MetaInsight: An interactive web-based tool for analyzing, interrogating, and visualizing network meta-analyses using R-shiny and netmeta. Res Syn Meth. 2019; 10: 569-581.",
      align="left")
    ),
    br(),
    p(
      "App powered by Rshiny.All frequentist statistical calculations are performed using R package netmeta (Gerta Rücker, Guido Schwarzer, Ulrike Krahn and Jochem König 2017).", 
      tags$a(href="http://CRAN.R-project.org/package=netmeta", "netmeta: Network Meta-Analysis using Frequentist Methods. R package version 0.9-8.",target="_blank"),
      "All Bayesian statistical calculations are performed using R package gemtc (Gert van Valkenhoef, Joel Kuiper 2016)",
      tags$a(href="https://cran.r-project.org/web/packages/gemtc/gemtc.pdf", "gemtc: Network Meta-Analysis Using Bayesian Methods R package version 0.8-2.",target="_blank"),
      "and R package BUGSNET (Audrey Beliveau, Devon J. Boyne, Justin Slater, Darren Brenner & Paul Arora)",
      tags$a(href="https://bugsnetsoftware.github.io/", "BUGSnet: Bayesian inference Using Gibbs Sampling to conduct NETwork meta-analysis version 1.0.3.",target="_blank")
    ),
    p(
      "For users wishing to analyse large treatment networks or fit complex network meta-analysis models, please seek advice from technical experts."
      ),
    br(),
    p("THE SOFTWARE IS PROVIDED AS IS, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
      NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
      IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
      WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
      OR THE USE OR OTHER DEALINGS IN THE SOFTWARE."),
    wellPanel(
      img(src = 'CRSULogo.png', width = "100%"),
      tags$strong("Funding and Support Acknowledgement:"),
      tags$p("The Complex Reviews Support Unit is funded by the National Institute for Health Research (NIHR) (project number 14/178/29).
       Development of this app is also funded by the NIHR Applied Research Collaboration East Midlands (ARC EM) and the Leicester NIHR Biomedical Research Centre (BRC).
       The views expressed are those of the author(s) and not necessarily those of the NIHR or the Department of Health and Social Care."),
      tags$p(
        "More information about the UK NIHR Complex Reviews Support Unit (CRSU) can be found ",
        tags$a(href="https://www.gla.ac.uk/research/az/evidencesynthesis/apps-materials-guidence/", "on our website", target="_blank"),
      )
    )
  )
}


#' Module server for the home page.
#' 
#' @param id ID of the module
home_page_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    return(reactive({ input$metaoutcome }))
  })
}