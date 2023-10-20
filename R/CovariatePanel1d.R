#' Module UI for the covariate summary plot tab.
#' 
#' @param id ID of the module
#' @return Tab 1d Covariate summary
load_covariate_tab_ui <- function(id) {
  ns <- NS(id)
  tabPanel("1d. Covariate summary",
    p("Holding text"),
    verbatimTextOutput(ns("text")),
    plotOutput(ns("covariate_plot")),
    p("The covariate is the same for all treatment arms across a study."),
    radioButtons('format_covariate_plot', 'Document format', c('PDF', 'PNG'), inline = TRUE),
    downloadButton('downloadCovariateSummary')
  )
}

#' Module server for the covariate summary plot tab.
#'
#' @param id ID of the module
#' @param BUGSnet_data Study data in BUGSnet format - to be added
#' @param covariate Covariate name - to be added
#' @return Covariate plot from BUGSnet::data.plot
#' 
load_covariate_tab_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Testing text 
    output$text <- renderPrint({
      "foo"
    })
    
    # BUGSnet data.plot used for covariate summary
    # https://rdrr.io/github/audrey-b/BUGSnet/man/data.plot.html
    make_covariate_plot <- function(BUGSnet_data, covariate) {

      return(BUGSnet::data.plot(BUGSnet_data,
                                covariate = covariate, # Covariate name is automatically used for y-axis label
                                # half.length = "age_SD", # Error bars - needs a second covariate, possible future addition
                                by = "treatment", 
                                text.size = 16) # May need to be reactive - test with different size datasets
             )
    }
    
    # Render covariate summary plot
    output$covariate_plot <- renderPlot({

      # Hard-code BUGSnet data prep of diabetes.sim data (will be an input to module when data upload sorted)
      BUGSnet_data <- BUGSnet::data.prep(arm.data = BUGSnet::diabetes.sim,
                                              varname.t = "Treatment",
                                              varname.s = "Study")

      # Hard-coded covariate - will become an input to module
      covariate <- "age"

      make_covariate_plot(BUGSnet_data, covariate)
    })
    
    output$downloadCovariateSummary <- downloadHandler(
      filename = "1d_Covariate_Summary", # New naming convention
      content = function(file) {
        draw_covariate_summary <- function() {
          
          # Hard-code BUGSnet data prep of diabetes.sim data (will be an input to module when data upload sorted)
          BUGSnet_data <- BUGSnet::data.prep(arm.data = BUGSnet::diabetes.sim,
                                             varname.t = "Treatment",
                                             varname.s = "Study")
          
          # Hard-coded covariate - will become an input to module
          covariate <- "age"
          
          make_covariate_plot(BUGSnet_data, covariate)
          
        }
        write_to_pdf_or_png(
          file,
          input$format_covariate_plot,
          draw_covariate_summary
        )
      }
    )
    
  })
}
