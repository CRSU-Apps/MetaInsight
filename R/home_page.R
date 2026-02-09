
#' Module UI for the home page.
#'
#' @param id ID of the module
#' @return Div for the home page
home_page_ui <- function(id) {
  ns <- NS(id)
  div(
    tags$head(
      tags$script(
        'var dimension = [0, 0];
        $(document).on("shiny:connected", function(e) {
            dimension[0] = window.innerWidth;
            dimension[1] = window.innerHeight;
            Shiny.onInputChange("dimension", dimension);
        });
        $(window).resize(function(e) {
            dimension[0] = window.innerWidth;
            dimension[1] = window.innerHeight;
            Shiny.onInputChange("dimension", dimension);
        });'
      )
    ),
    fluidRow(
      div(
        h4("A Beta version of MetaInsight v7 featuring improved plots, automatically updating models and other new features is available at: ",
           a("https://crsu.shinyapps.io/MetaInsight_Scholar/",
             href= "https://crsu.shinyapps.io/MetaInsight_Scholar/",
             target = "_blank", style="color: white; text-decoration: underline"),
           ". Please send any feedback to apps@crsu.org.uk.",
        style="color: white; margin: 0; padding: 0;"),
        style="background-color: #e4042c; padding: 20px; width: 100%; text-align: center;"
      ),
      style="margin: 0; padding: 0;"
    ),
    h2("MetaInsight v6.4.0", align = "left"),
    fluidRow(
      column(
        width = 3,
        div(
          tags$h4("Selection of continuous or binary outcomes has moved to the ", tags$i("Load Data"), " tab"),
          style = "color: #aa0000; border: solid; border-color: lightgray; border-width: thin; border-radius: 6pt; padding: 0 10pt;"
        ),
        img(src = "images/MetaInsightLogo.png", height = 400, align = "center")
      ),
      column(width = 2),
      column(
        width = 5,
        p(tags$strong("Latest Updates:")),
        p(tags$strong("Minor update (27 May 2025 v6.4.0):")),
        tags$ul(
          tags$li("Added trace and posterior density plots to Bayesian output."),
          tags$li("Added MCMC details such as prior distributions and number of iterations."),
          tags$li("Set seeds for Bayesian models so that results are reproducible."),
        ),
        p(tags$strong("Major update (10 July 2024 v6.0.0):")),
        p(
          tags$ul(
            tags$li(
              "Meta-regression has been added. One covariate is allowed, which can be a new continuous or binary variable,
              or baseline risk. Two new graphs are available for meta-regression. The first displays the covariate values grouped by
              treatment and study. The second plots the covariate against relative treatment effects, with credible regions and
              study-level contributions."
            )
          )
        ),
        p(
          "The full update history for MetaInsight is available on",
          tags$a(href="https://github.com/CRSU-Apps/MetaInsight/wiki/Full-Update-History", "GitHub",target="_blank")
        ),
        p(
          "The code for MetaInsight is available on",
          tags$a(href="https://github.com/CRSU-Apps/MetaInsight", "GitHub",target="_blank")
        ),
        br()
      )
    ),
    br(),
    p(
      "Naomi Bradbury, Ryan Field, Tom Morris, Clareece Nevill, Janion Nevill, Yiqiao Xin, Rhiannon K Owen, Nicola Cooper, and Alex Sutton",
      align = "left"
    ),
    p(
      "For feedback/questions about this app please email the CRSU team at apps@crsu.org.uk. If you encounter any errors with using the app, please check the",
      tags$a(href = "https://github.com/CRSU-Apps/MetaInsight/wiki/Troubleshooting", "trouble shooting page"),
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
      "App powered by R and Shiny. All frequentist statistical calculations are performed using R package ",
      tags$b("netmeta"),
      " (Gerta Rücker, Guido Schwarzer, Ulrike Krahn and Jochem König 2017)",
      tags$a(href = "http://CRAN.R-project.org/package=netmeta","netmeta: Network Meta-Analysis using Frequentist Methods. R package version 0.9-8.",target = "_blank"),
      "All Bayesian statistical calculations are performed using R packages ",
      tags$b("gemtc"),
      " (Gert van Valkenhoef, Joel Kuiper 2016)",
      tags$a(href = "https://cran.r-project.org/web/packages/gemtc/gemtc.pdf", "gemtc: Network Meta-Analysis Using Bayesian Methods R package version 0.8-2.",target = "_blank"),
      ", ", tags$b("BUGSNET"),
      " (Audrey Beliveau, Devon J. Boyne, Justin Slater, Darren Brenner & Paul Arora)",
      tags$a(href = "https://bugsnetsoftware.github.io/", "BUGSnet: Bayesian inference Using Gibbs Sampling to conduct NETwork meta-analysis version 1.0.3.",target =  "_blank"),
      " , and ", tags$b("bnma"),
      " (Michael Seo, Christopher Schmid 2024)",
      tags$a(href = "https://CRAN.R-project.org/package=bnma", "bnma: Bayesian Network Meta-Analysis using 'JAGS'. R package version 1.6.0.",target = "_blank")
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
      div(style = "display: inline;",
          img(src = 'funded-by-nihr-logo.png', width = "55%")),
      div(style = "display: inline;",
          img(src = 'CRSU_logo.png', width = "40%")),
      div(
        tags$strong("Funding and Support Acknowledgement:"),
        tags$p("MetaInsight is part of the Complex Reviews Synthesis Unit (CRSU) suite of evidence synthesis apps.
        The development of these apps was (majority) funded and overseen by the Evidence Synthesis Group @ CRSU (NIHR153934).
        Further details of other funders and support, current and past, can be found ",
          tags$a(href = "https://github.com/CRSU-Apps/.github/wiki/Detailed-Funding-Statement", "on our GitHub page"),
        ". The views expressed are those of the author(s) and not necessarily those of the NIHR or the Department of Health and Social Care."),
        tags$p(
          "More information about the UK NIHR Complex Reviews Synthesis Unit (CRSU) can be found ",
          tags$a(href = "https://www.gla.ac.uk/research/az/crsu/", "on our website", target = "_blank"),
        )
      )
    )
  )
}


#' Module server for the home page.
#'
#' @param id ID of the module
home_page_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Do nothing
  })
}
