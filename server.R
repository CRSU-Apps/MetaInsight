###### Bayesian V4 ######

#if packages not installed, please install them first by running the line below.
#install.packages(c("dplyr","metafor", "netmeta","shiny", "shinyAce","rmarkdown", "knitr", "gemtc", "plyr", data.table"
#  , "shinyalert", "plotly"))

# the data for meta-regression is from: http://nicedsu.org.uk/wp-content/uploads/2016/03/TSD3-Heterogeneity.final-report.08.05.12.pdf

library(metafor)
library(netmeta)
library(shiny) 
library(shinyAce)
library(rmarkdown)
library(knitr)
library(gemtc)
library(plyr)
library(dplyr)
library(data.table)
library(shinyalert)
library(plotly)
library(shinyjs)
library(BUGSnet)
library(shinyBS)
library(patchwork)
library(ggrepel)
library(magick)
library(stringr)
library(ggiraphExtra)
library(tidyr)

# Source files
source("bugsnet_sumtb.R") # bugsnet_sumtb function, req BUGSnet - separate file added by NVB
source("plot.R") # Plot functions - separate file added by NVB
source("util.R") # Utility functions - separate file added by NVB


source("PlotFunctionsRKO.R", local = TRUE)        # Plot functions
load("blank.rds")                                 # Objects to store data for plot functions
source("fn_analysis.R",local = TRUE)              # functions for NMA


shinyServer(function(input, output, session) {
  source("downloadbuttons.R", local = TRUE)   #codes for download buttons for conciseness. This line must be put within the shinyserver as this is purely a code file not functions.
  
  # Create a definable reactive value to allow reloading of data
  reload <- reactiveVal(F)
  
  # Render function for file input dynamically to allow the button to be set to Null
  default_file_input <- 
    renderUI({
      fileInput(inputId="data", label="", buttonLabel="Select", placeholder="No file selected")
    })
  
  # Render function reload button dynamically to allow the button to be set to Null
  default_reload_button <-
    renderUI({
      div(style = "display:inline-block; float:right", actionButton("reload_button", "Delete Data", icon("trash"), 
                   style="color: #fff; background-color: #dc3545; border-color: #dc3545"))
    })
  
  # Make the treatment panel reactive to allow switching between continous and binary more dynamic
  default_trt_panel <- reactive({
    # respond to reload
    reload()
    if (input$metaoutcome=='Continuous') {
      return(
        panel(
          aceEditor(
            "listCont",
            value = paste0(
              "Number\tLabel",
              "\n1\tPlacebo",
              "\n2\tOrlistat",
              "\n3\tSibutramine",
              "\n4\tMetformin",
              "\n5\tOrli_Sibut",
              "\n6\tRimonbant"),
            mode = "r" ,
            theme = "eclipse"
          )
        )
      )
    }
    else{
      return(
        panel(
          aceEditor(
            "listbina",
            value = paste0(
              "Number\tLabel",
              "\n1\tNo_contact",
              "\n2\tSelf_help",
              "\n3\tIndividual_counselling",
              "\n4\tGroup_counselling"),
            mode = "r",
            theme = "eclipse"
          )
        )
      )
    }
  })
  
  # Render the above treatment panel
  output$trt_panel <- renderUI({
    default_trt_panel()
  })
  
  # Render the file input intially
  output$file_input = default_file_input
  
  #####
  # Reactive functions used in various places
  #####
  
  # Define outcome measure (continuous or binary) - NVB
  outcome_measure <- reactive({
    if (input$metaoutcome == "Continuous") {return(input$outcomeCont)}
    else {return(input$outcomebina)}
  })
  
  # Load default data
  defaultD <- reactive({
    if (input$metaoutcome=='Continuous') {
      defaultD <- read.csv("./Cont_long.csv")
    } else {
      defaultD <- read.csv("./Binary_long.csv")
    }
  })
  
  # Make data reactive i.e. default or user uploaded
  data <- reactive({ 
    file1 <- input$data # Name the data file that was uploaded file1
    # if a reload is triggered show the reload the file input and data
    if(reload()){
      output$file_input = default_file_input
      return(defaultD())
    }
    # if data is triggered without reload, only load the default data
    else if(is.null(file1)){return(defaultD())      }
    else
      a <- read.table(file = file1$datapath, sep =",", header=TRUE, stringsAsFactors = FALSE, quote="\"", fileEncoding = 'UTF-8-BOM')
  })
  
  # Make reactive treatment input list selecting correct input 
  # depending on if outcome is continuous or binary - NVB
  
  treatment_list <- reactive({
    if (input$metaoutcome == "Continuous") {return (input$listCont)}
    else {return (input$listbina)}
  })
  
  # Make frequentist function (in fn_analysis.R) reactive - NVB
  freq_all <- reactive({
    return(frequentist(sub = FALSE, data(), input$metaoutcome, treatment_list(), outcome_measure(), input$modelranfix, input$exclusionbox))
  })
  
  # Make frequentist function (in fn_analysis.R) reactive with excluded studies - NVB
  freq_sub <- reactive({
    return(frequentist(sub = TRUE, data(), input$metaoutcome, treatment_list(), outcome_measure(), input$modelranfix, input$exclusionbox))
  })
  
  # Make bugsnetdata function (in fn_analysis.R) reactive - NVB
  bugsnetdt <- reactive({
    return(bugsnetdata(data(), input$metaoutcome, treatment_list()))
  })
   
  # Make ref_alter function (in fn_analysis.R) reactive - NVB
  reference_alter <- reactive({
    return(ref_alter(data(), input$metaoutcome, input$exclusionbox, treatment_list()))
  })
  
  #####
  # observer functions to trigger specific reactions
  #####
  
  # if the outcome is changed, reload the data and labels, reset the file input and hide the reload button
  observeEvent(input$metaoutcome, {
    reload(T)
    output$file_input = default_file_input
    output$reload_button = NULL
  })
  
  # if the data is changed load the new data (reset the labels) and show the reload button
  observeEvent(input$data, {
    reload(F)
    output$reload_button = default_reload_button
  })
  
  # if the reload button is clicked, reload the appropriate default data and labels and hide the reload button
  observeEvent(input$reload_button,{
    reload(T)
    output$file_input = default_file_input
    output$reload_button = NULL
  })
  
  # if the reload labels button is clicked reload the default labels
  observeEvent(input$reload_labels, {
    output$trt_panel <- renderUI({
      default_trt_panel()
    })
  }, ignoreNULL = F)
  
  # Allow the treatment list to be rendered without the data tab being loaded.
  outputOptions(output, "trt_panel", suspendWhenHidden = F)

  ############################################
  ######### Home page - linking pages ########
  ############################################
  
  ### GDPR

    showModal(modalDialog(
       title = "Important message",
        easyClose = FALSE,
        p(tags$strong("In accordance with Data Protection legislation, we would like to inform you of the following before you use our website:
                                 "), "We collect your usage data within the MetaInsight app to perform analytics of usage and improve our app. By clicking",
      tags$i(tags$u("I consent")), "below, you consent to the use of data by us through Google Analytics.
          For details of policy, please check the 'Privacy notice' tab within the app, and ",tags$a(href="https://policies.google.com/privacy?hl=en", "Google Privacy & Terms.",target="_blank") ),
        br(),
        modalButton("I consent"),
        footer = NULL
      ))

  ### View the full update history
  
    observeEvent(input$history_click, {
      newvalue <- "history"
      updateNavbarPage(session,"meta", selected="Full update history")
    })
    
  ### View the trouble shooting page
    
    observeEvent(input$tsp, {
      #newvalue <- "history"
      updateNavbarPage(session,"meta", selected="Troubleshooting")
    })


  
  ############################################
  ############# Load data page ###############
  ############################################
  
  ### Outcome selection
    output$CONBI <- renderText({
      paste("You have selected", "<font color=\"#ffd966\"><b>" , input$metaoutcome,"</b></font>", 
            "outcome on the 'Home' page. The instructions for formatting",
            "<font color=\"#ffd966\"><b>" , input$metaoutcome,"</b></font>", "outcomes are now displayed.")
    })

  ### Downloadable csv and labels of example dataset. download button codes are all in a separate code file
    
  ### Data analysis tab
    # Create a table which displays the raw data just uploaded by the user
    output$tb <- renderTable({       
      if(is.null(data())){return()}
      data()
    })

  
  ############################################
  ########### Data analysis tab ##############
  ############################################
  
  ### Confirmation for continuous / binary data
  
  output$CONBI2 <- renderText({
    paste("You have selected", "<font color=\"#ffd966\"><b>" , input$metaoutcome,"</b></font>", 
          "outcome on the 'Home' page. The analysis page for ",
          "<font color=\"#ffd966\"><b>" , input$metaoutcome,"</b></font>", "outcomes are now displayed.")
  })
  
  ### Ranking defaults
  choice <- reactive({
    RankingOrder(input$metaoutcome,input$data)
  })
  
  output$RankingPref <- renderUI({
    choice2 <- choice()
    radioButtons('rankopts', 'For treatment rankings, smaller outcome values  
                      (e.g. smaller mean values for continuous data, 
                      or ORs less than 1 for binary data) are:', 
                 c("Desirable" = "good", "Undesirable" = "bad"), selected = choice2)
  })
  
  
  
  ### Get studies for check box input

    output$Choicesexcl <- renderUI({
      newData <- data()
      newData1 <- as.data.frame(newData)
      if (ncol(newData1)==6 ||ncol(newData1)==5 ){        # long format data contain exactly 6 columns for continuous and 5 for binary. wide format will contain at least 2+4*2=10 columns.
        newData2<-newData1[order(newData1$StudyID, -newData1$T), ]
        newData2$number<- ave(as.numeric(newData2$StudyID),newData2$StudyID,FUN=seq_along)    # create counting variable for number of arms within each study.
        data_wide <- reshape(newData2, timevar = "number",idvar = c("Study", "StudyID"), direction = "wide")     # reshape
      }
      else {
        data_wide<- newData1
      }
      checkboxGroupInput("exclusionbox",
                         label = NULL,
                         choices = as.character(data_wide$Study))
    })
    
    ### Get data for data table
    

    filtertable <- function(){
      label <- treatment_label(treatment_list())
      dt <- data()
      ntx <- nrow(label)
      dt$T <- factor(dt$T,
                     levels = c(1:ntx),
                     labels = as.character(label$Label))
      dt
    }

  colnames<- function(){
    if (input$metaoutcome=="Continuous") {
      colnames <- c('StudyID', 'Author','Treatment','Number of participants in each arm',
                    'Mean value of the outcome in each arm', 'Standard deviation of the outcome in each arm')
      
    } else{
      colnames <- c('StudyID', 'Author','Treatment','Number of participants with the outcome of interest in each arm','Number of participants in each arm'
      )
    }}

    output$datatb <- DT::renderDataTable(DT::datatable({
      filtertable()
    },editable=TRUE, rownames= FALSE, 
    colnames= colnames(),
      filter = list(
      position = 'top', clear = FALSE, stateSave = TRUE)
    
    ))
    
    observeEvent(input$datatablebutton, ({
      updateCollapse(session, "collapse", open = "Data table (Click to open / hide this panel)")
    }))
    
    
    output$ref_change_bay = output$ref_change <- renderText({
         if (identical(reference_alter()$ref_sub, reference_alter()$ref_all)=="FALSE") {
           paste("Please note that the reference treatment for sensitivity analysis has now been changed to:", reference_alter()$ref_sub, ". This is because the treatment labelled 1 has been removed from the network of sensitivity analysis." )
         }
    })
    

  

    
  #######################
  ### 1. Data Summary ###
  #######################
    
  # 1a. Data Characteristics
  
  # Characteristics table of all studies
  output$sumtb <- renderTable({
    summary_table_plot(bugsnetdt(), input$metaoutcome)
  })
  
  # Characteristics table with studies excluded
  output$sumtb_sub <- renderTable({
    summary_table_plot(filter(bugsnetdt(), !Study %in% input$exclusionbox), input$metaoutcome)
  })
  
  # 1b. Study Results 
  
  # Forest plot
  output$forestPlot <- renderPlot({
    make_netStudy(freq_sub(), outcome_measure(), input$ForestHeader, input$ForestTitle)$fplot
  })
  
  # 1c. Network Plot
  
  # Network plot of all studies
  output$netGraphStatic1 <- renderPlot({
    if (input$networkstyle=='networkp1') {
      # Number of trials on line
      make_netgraph(freq_all(),input$label_all) 
    } else {
      # Number of trials by nodesize and line thickness
      make_netplot(bugsnetdt(), input$label_all) 
    }
    title("Network plot of all studies")
  })
  
  # Network connectivity all studies
  output$netconnect <- renderPrint ({
    make_netconnect(freq_all())
  })
  
  # Network plot with studies excluded
  output$netGraphUpdating <- renderPlot({
    if (input$networkstyle_sub=='networkp1') {
      # Number of trials on line
      make_netgraph(freq_sub(),input$label_excluded) 
    } else {
      # Number of trials by nodesize and line thickness
      make_netplot(filter(bugsnetdt(), !Study %in% input$exclusionbox), input$label_excluded)
    }
    title("Network plot with studies excluded")
  })
  
  # Network connectivity with studies excluded 
  output$netconnect_sub <- renderPrint ({
    make_netconnect(freq_sub())
  })

  
  

  
  
  ############### bugsnet code #################
  
  ### (notification on disconnection when data are uploaded)
  # disconnect_load <- function(){
  #     showModal(modalDialog(
  #       title = "Disconnected network",
  #       easyClose = FALSE,
  #       p(tags$strong("Please note that the network of the uploaded data is disconnected. Two or more networks exist. The disconnected networks are displayed at 'Data analysis' - '1c. Network Plot' tab. Please upload the data for each network separately.")),
  #       br(),
  #       modalButton("Close warning"),
  #       footer = NULL
  #     ))
  # }
  
  ### (notification on disconnection)
  disconnect <- function(){
    showModal(modalDialog(
      title = "Disconnected network",
      easyClose = FALSE,
      p(tags$strong("Please note that the network of sensitivity analysis is disconnected. Two or more networks exist. The disconnected networks are displayed at 'Data analysis' - '1c. Network Plot' tab - 'Network plot with studies excluded'. 
                    Please continue excluding studies until only one network remains, or adding studies back until the network is re-connected, as appropriate.")),
      br(),
      modalButton("Close warning"),
      footer = NULL
    ))
  }
  
  # checklabel <- function(){    
  #   label <- ifelse(input$metaoutcome=="Continuous",input$listCont,input$listbina)
  #   treat_list <- read.csv(text=label, sep = "\t")
  #   
  #   if (input$metaoutcome=="Continuous") {
  #     def_con<-read.delim("./defaultlabels_continuous.txt")
  #   } else {
  #     def_con<-read.delim("./defaultlabels_binary.txt")
  #   }
  #   check <- identical(def_con, treat_list)
  # }
  
  # observeEvent(checklabel()==FALSE,{   # after users uploaded the label, run the disconnection check.
  #     longsort2 <- bugsnetdt() # inputting the data in long form
  #     sumtb<-bugsnet_sumtb(longsort2)
  #     
  #     if (sumtb$Value[6]=="FALSE") {
  #       disconnect_load()
  #     }
  #   
  # })
  
  
  observeEvent(input$exclusionbox,{
    longsort2 <- bugsnetdt()
    longsort2_sub <- filter(bugsnetdt(), !Study %in% input$exclusionbox)  # subgroup
    sumtb_sub <- bugsnet_sumtb(longsort2_sub, input$metaoutcome)
    if (sumtb_sub$Value[6]=="FALSE") {
      disconnect()
    }})
  
  
  bugsnettry<-function(){
    newData<-read.csv("./Binary_wide_cov.csv")
    treat_list <- read.csv("./defaultlabels_baye_reg.txt", sep = "\t")
    newData1 <- as.data.frame(newData)
    num_cov<-1
    CONBI <- 'Binary'
    dataform_reg.df <- function(newData1, treat_list, CONBI, num_cov) {
      if (ncol(newData1)== 6+num_cov | ncol(newData1)==5+num_cov) {
        long <- newData1
      } else {
        data_wide <-newData1
        a<- ifelse(CONBI=='Continuous', 4, 3)
        numbertreat=(ncol(newData1)-2-num_cov)/a
        if (numbertreat < 6) {
          for (k in (numbertreat+1):6) {
            if (CONBI=='Continuous') {
              data_wide[c(paste0("T.",k),paste0("N.",k),paste0("Mean.",k),paste0("SD.",k))]<-NA
            } else {
              data_wide[c(paste0("T.",k),paste0("R.",k),paste0("N.",k))]<-NA
            }
          }
        }
        else {
          data_wide<-newData1
        }
        long_pre <- reshape(data_wide, direction = "long",
                            varying = 4:ncol(data_wide), 
                            times=c(".1", ".2", ".3", ".4", ".5", ".6"), sep=".", idvar= c(1:(2+num_cov)))
        long_pre<-subset(long_pre, select=-time)
        long <- long_pre[!is.na(long_pre$T), ]
      }
      long_sort<-long[order(long$StudyID, -long$T), ]
      if (CONBI=='Continuous') {
        long_sort$se<-long_sort$SD/sqrt(long_sort$N)
      }
      lstx <- treat_list$Label
      treat_list2<-data.frame(treat_list)
      ntx <- nrow(treat_list)
      colnames(treat_list2)[1] <- "T"
      long_sort2<-merge(long_sort, treat_list2, by=c("T"))
      long_sort2<-subset(long_sort2, select=-T)
      names(long_sort2)[names(long_sort2) == 'Label'] <- 'T'
      return(long_sort2)
    }
    long_sort2<-dataform_reg.df(newData1,treat_list,"Binary", 1)
    data.rh<-data.prep(arm.data=long_sort2, varname.t = "T", varname.s="Study")
    p<-data.plot(data = data.rh,
                 covariate = "DiseaseDuration",  # make this to be 
                 #half.length = "age_SD", #comment this line out to remove error bars
                 by = "treatment", # this is not variable name. only two options to selection: "treatment" or "study" (to plot characteristics by study)
                 #fill.str = "age_type",  #comment this line out to remove colors
                 avg.hline=TRUE) #add overall average line?
    
    ##Network characteristic summary tables
    network.char <- net.tab(data = data.rh,
                            outcome = "R",
                            N = "N",
                            type.outcome = "binomial",
                            time = NULL)
    tb<-network.char$network
    list(p=p, tb=tb)
  }
  
  
  
  output$covp <- renderPlot({
    bugsnettry()$p
  })
  
  
  
  
  ################## finish ##############
  
  
  
  ######################
  ### 2. Frequentist ###
  ###################### 
  
  # 2a. Forest Plot
  
  make_refText = function(ref) {
    y <- paste("All outcomes are versus the reference treatment:", ref)
    return(y)
  }
  
  observe({                              # forest min and max values different if continuous/binary
    x <- input$metaoutcome
    if (x=='Binary') {
      updateNumericInput(session = session, inputId = "freqmin", value=0.1)
      updateNumericInput(session = session, inputId = "freqmin_sub", value=0.1)
      updateNumericInput(session = session, inputId = "bayesmin", value=0.1)
      updateNumericInput(session = session, inputId = "bayesmin_sub", value=0.1)
      updateNumericInput(session = session, inputId = "freqmax", value=5)
      updateNumericInput(session = session, inputId = "freqmax_sub", value=5)
      updateNumericInput(session = session, inputId = "bayesmax", value=5)
      updateNumericInput(session = session, inputId = "bayesmax_sub", value=5)
    } else {
      updateNumericInput(session = session, inputId = "freqmin", value=-10)
      updateNumericInput(session = session, inputId = "freqmin_sub", value=-10)
      updateNumericInput(session = session, inputId = "bayesmin", value=-10)
      updateNumericInput(session = session, inputId = "bayesmin_sub", value=-10)
      updateNumericInput(session = session, inputId = "freqmax", value=10)
      updateNumericInput(session = session, inputId = "freqmax_sub", value=10)
      updateNumericInput(session = session, inputId = "bayesmax", value=10)
      updateNumericInput(session = session, inputId = "bayesmax_sub", value=10)
    }
  })
  
  # Forest plot for all studies
  output$Comparison2<- renderPlot({
    make_netComp(freq_all(), input$modelranfix, reference_alter()$ref_all, input$freqmin, input$freqmax)
    title("Results for all studies")
  })
  
  # Text output displayed under forest plot  
  output$textcomp<- renderText({
    texttau(freq_all(), outcome_measure(), input$modelranfix)
  })
  
  output$ref4 <- renderText({
    make_refText(reference_alter()$ref_all)
  })

  
  # Forest plot with studies excluded
  output$SFPUpdatingComp <- renderPlot({
    make_netComp(freq_sub(), input$modelranfix, reference_alter()$ref_sub, input$freqmin_sub, input$freqmax_sub)
    title("Results with studies excluded")
  }) 
  
  # Text output displayed under forest plot
  output$text5<- renderText({ 
    texttau(freq_sub(), outcome_measure(), input$modelranfix)
  })

  output$ref3 <- renderText({
    make_refText(reference_alter()$ref_sub)
  })
  
  ### Interactive UI ###
  
  output$FreqForestPlot <- renderUI({
    plotOutput("Comparison2", height = BayesPixels(as.numeric(bugsnet_sumtb(bugsnetdt(), input$metaoutcome)$Value[1]), title=TRUE), width = "630px")
  })
  
  output$FreqForestPlot_sub <- renderUI({
    plotOutput("SFPUpdatingComp", height = BayesPixels(as.numeric(bugsnet_sumtb(filter(bugsnetdt(), !Study %in% input$exclusionbox), input$metaoutcome)$Value[1]), title=TRUE), width = "630px")
  })
  
  ### 2b. Comparison and rank table
  
  output$rankChartStatic<- renderTable(colnames=FALSE,{
    make_netrank(freq_all(), input$modelranfix, input$rankopts)
  })
  output$rankChartUpdating<- renderTable(colnames=FALSE,{
    make_netrank(freq_sub(), input$modelranfix, input$rankopts)
  })

  ### 2c. Inconsistency

  output$Incon1<- renderTable(colnames=TRUE, {
    make_Incon(freq_all(), input$modelranfix)}
  )
  output$Incon2<- renderTable(colnames=TRUE, {
    make_Incon(freq_sub(), input$modelranfix)}
  )


  #####################
  #### 3. Bayesian ####
  #####################
  
  ### SMD warning alert
  
  observeEvent(list(input$baye_do,input$sub_do, input$node,input$node_sub), {
    if (input$outcomeCont=="SMD") {
      showNotification("Please note: standardised mean difference currently cannot be analysed in Bayesian analysis", type = "error", duration = NULL)
    } 
    else if (input$outcomebina=="RD") {
      showNotification("Please note: Risk difference currently cannot be analysed in Bayesian analysis", type = "error", duration = NULL)
    }
  })
  
  
  
  # Bayesian analysis
  
  model <- eventReactive(input$baye_do, {
    bayesian_model(sub = FALSE, data(), treatment_list(), input$metaoutcome, input$exclusionbox,
                   outcome_measure(), input$modelranfix, reference_alter())
  })
  
  model_sub <- eventReactive(input$sub_do, {
    bayesian_model(sub = TRUE, data(), treatment_list(), input$metaoutcome, input$exclusionbox, 
                   outcome_measure(), input$modelranfix, reference_alter())
  })

  # 3a. Forest plot

  # Forest plot for all studies
  output$gemtc <- renderPlot({    
    make_Forest(model(), input$metaoutcome, input$bayesmin, input$bayesmax)
    title(paste("All studies: 
              Bayesian", model()$a, "consistency model forest plot results"))
  })

  # DIC tabel for all studies
  output$dic <- renderTable ({                  
    model()$dic
  }, digits=3, rownames=TRUE, colnames=FALSE
  )
  
  # Tau all studies
  output$text_gemtc <-renderText({          
    gemtctau(model(), outcome_measure())
  })
  
  # Forest plot with studies excluded
  output$gemtc_sub <- renderPlot({
    make_Forest(model_sub(), input$metaoutcome, input$bayesmin_sub, input$bayesmax_sub)
    title(paste("Results with studies excluded: 
              Bayesian", model_sub()$a,"consistency model forest plot results"))
  })
  
 # DIC table with studies excluded
  output$dic_sub <- renderTable ({
    model_sub()$dic
  }, digits=3, rownames=TRUE, colnames=FALSE)
  
  # Tau with studies excluded
  output$text_gemtc_sub <-renderText({
    gemtctau(model_sub(), outcome_measure())
  })
  
  # Interactive UI 
  output$BayesianForestPlot <- renderUI({
    plotOutput("gemtc", width="630px", height = BayesPixels(as.numeric(bugsnet_sumtb(bugsnetdt(), input$metaoutcome)$Value[1]), title=TRUE))
  })
  output$BayesianForestPlot_sub <- renderUI({
    plotOutput("gemtc_sub", width="630px", height = BayesPixels(as.numeric(bugsnet_sumtb(filter(bugsnetdt(), !Study %in% input$exclusionbox), input$metaoutcome)$Value[1]), title=TRUE))
  })

  
  # 3b. Comparison of all treatment pairs

  # Treatment effects for all studies
  output$baye_comparison <- renderTable ({
    baye_comp(model(), input$metaoutcome, outcome_measure())
  }, rownames=TRUE, colnames = TRUE
  )
  
  # Treatment effects with studies excluded
  output$baye_comparison_sub <- renderTable ({
    baye_comp(model_sub(), input$metaoutcome, outcome_measure())
  }, rownames=TRUE, colnames = TRUE
  )
  
  # 3c. Ranking Panel
  
  # Obtain Data needed for ranking #
  RankingData <- eventReactive(input$baye_do, {
    obtain_rank_data(sub=TRUE, data(), input$metaoutcome, input$exclusionbox, 
                     treatment_list(), model(), input$rankopts)
  })
  
  RankingData_sub <- eventReactive(input$sub_do, {
    obtain_rank_data(sub=FALSE, data(), input$metaoutcome, input$exclusionbox, 
                     treatment_list(), model_sub(), input$rankopts)
  })
  
  # Network plots for ranking panel (Bayesian) (they have slightly different formatting to those on tab1) CRN
  treat_order <- reactive(RankingData()$SUCRA[order(RankingData()$SUCRA$SUCRA),1]) # obtain treatments ordered by SUCRA #
  freq_all_react <- eventReactive(input$baye_do, {  # these two lines are needed in case someone jumped to Bayesian page without running frequentist section, but am aware this can cause frequentist analysis to run twice (CRN)
    freq_all()
  })
  bugsnetdt_react <- eventReactive(input$baye_do, {
    bugsnetdt()
  })
  output$netGraphStatic1_rank <- renderPlot({
    if (input$networkstyle_rank=='networkp1') {
      # Number of trials on line
      make_netgraph_rank(freq_all_react(), treat_order())
    } else {
      # Number of trials by nodesize and line thickness 
      make_netplot(bugsnetdt_react(), order=list(order=treat_order()))
    }
    title("Network plot of all studies")
  })
  # Repeat for excluded studies
  treat_order_sub <- reactive(RankingData_sub()$SUCRA[order(RankingData_sub()$SUCRA$SUCRA),1])
  freq_all_react_sub <- eventReactive(input$sub_do, {
    freq_sub()
  })
  bugsnetdt_react_sub <- eventReactive(input$sub_do, {
    bugsnetdt()
  })
  output$netGraphStatic1_rank_sub <- renderPlot({
    if (input$networkstyle_rank_sub=='networkp1') {
      # Number of trials on line
      make_netgraph_rank(freq_all_react_sub(), treat_order_sub())
    } else {
      # Number of trials by nodesize and line thickness
      make_netplot(filter(bugsnetdt_react_sub(), !Study %in% input$exclusionbox), order=list(order=treat_order_sub()))
    }
    title("Network plot with studies excluded")
  })
  
  # Forest plots for ranking panel (different style due to using 'boxes' in UI) CRN
  # All studies #
  output$gemtc2 <- renderPlot({                  
    png("forest.png")  # initialise image
    forest(model()$mtcRelEffects,digits=3)
    dev.off()
    ForestImg <- magick::image_read('forest.png')
    Img <- cowplot::ggdraw() +
      cowplot::draw_image(ForestImg)
    return(Img)
  })
  # With studies excluded
  output$gemtc_sub2 <- renderPlot({                  
    png("forest_sub.png")
    forest(model_sub()$mtcRelEffects,digits=3)
    dev.off()
    ForestImg <- magick::image_read('forest_sub.png')
    Img <- cowplot::ggdraw() +
      cowplot::draw_image(ForestImg)
    return(Img)
  })

  # All rank plots in one function for easier loading when switching options #
  Rankplots <- reactive({
    plots <- list()
    plots$Litmus <- LitmusRankOGram(CumData=RankingData()$Cumulative, SUCRAData=RankingData()$SUCRA, ColourData=RankingData()$Colour, colourblind=FALSE)
    plots$Radial <- RadialSUCRA(SUCRAData=RankingData()$SUCRA, ColourData=RankingData()$Colour, BUGSnetData=RankingData()$BUGSnetData, colourblind=FALSE)
    plots$Litmus_blind <- LitmusRankOGram(CumData=RankingData()$Cumulative, SUCRAData=RankingData()$SUCRA, ColourData=RankingData()$Colour, colourblind=TRUE)
    plots$Radial_blind <- RadialSUCRA(SUCRAData=RankingData()$SUCRA, ColourData=RankingData()$Colour, BUGSnetData=RankingData()$BUGSnetData, colourblind=TRUE)
    plots
  })
  Rankplots_sub <- reactive({
    plots <- list()
    plots$Litmus <- LitmusRankOGram(CumData=RankingData_sub()$Cumulative, SUCRAData=RankingData_sub()$SUCRA, ColourData=RankingData_sub()$Colour, colourblind=FALSE)
    plots$Radial <- RadialSUCRA(SUCRAData=RankingData_sub()$SUCRA, ColourData=RankingData_sub()$Colour, BUGSnetData=RankingData_sub()$BUGSnetData, colourblind=FALSE)
    plots$Litmus_blind <- LitmusRankOGram(CumData=RankingData_sub()$Cumulative, SUCRAData=RankingData_sub()$SUCRA, ColourData=RankingData_sub()$Colour, colourblind=TRUE)
    plots$Radial_blind <- RadialSUCRA(SUCRAData=RankingData_sub()$SUCRA, ColourData=RankingData_sub()$Colour, BUGSnetData=RankingData_sub()$BUGSnetData, colourblind=TRUE)
    plots
  })
  
  # Litmus Rank-O-Gram
  output$Litmus <- renderPlot({
    if (input$Colour_blind==FALSE) {Rankplots()$Litmus} else {Rankplots()$Litmus_blind}
  })
  output$Litmus_sub <- renderPlot({
    if (input$Colour_blind_sub==FALSE) {Rankplots_sub()$Litmus} else {Rankplots_sub()$Litmus_blind}
  })
  
  # Radial SUCRA 
  output$Radial <- renderPlot({
    if (input$Colour_blind==FALSE) {Rankplots()$Radial$Original} else {Rankplots()$Radial_blind$Original}
  })
  output$Radial_sub <- renderPlot({
    if (input$Colour_blind_sub==FALSE) {Rankplots_sub()$Radial$Original} else {Rankplots_sub()$Radial_blind$Original}
  })
  # Alternative SUCRA plots
  output$RadialAlt <- renderPlot({
    if (input$Colour_blind==FALSE) {Rankplots()$Radial$Alternative} else {Rankplots()$Radial_blind$Alternative}
  })
  output$RadialAlt_sub <- renderPlot({
    if (input$Colour_blind_sub==FALSE) {Rankplots_sub()$Radial$Alternative} else {Rankplots_sub()$Radial_blind$Alternative}
  })
  
  # Table of Probabilities (need to include SUCRA and have it as a collapsable table)
  output$rank_probs <- renderTable(
    {rank_probs_table(RankingData())}, 
    digits=2, rownames=FALSE, colnames=TRUE)
  output$rank_probs_sub <- renderTable(
    {rank_probs_table(RankingData_sub())}, 
    digits=2, rownames=FALSE, colnames=TRUE)
  
  # Text underneath
  output$relative_rank_text <-renderText({          
    relative_rank_text(model())
  })
  output$relative_rank_text_sub <-renderText({          
    relative_rank_text(model_sub())
  })
  

  # 3d. Nodesplit model
  
  # Inconsistency test with notesplitting model for all studies
  model_nodesplit <- eventReactive(input$node, {
    nodesplit(sub = FALSE, data(), treatment_list(), input$metaoutcome, outcome_measure(),
                    input$modelranfix, input$exclusionbox)
  })

  output$node_table<- renderTable(colnames=TRUE, {
    model_nodesplit()
  })

  # Inconsistency test with notesplitting model with studies excluded
  model_nodesplit_sub <- eventReactive(input$node_sub, {
    nodesplit(sub = TRUE, data(), treatment_list(), input$metaoutcome, outcome_measure(),
                    input$modelranfix, input$exclusionbox)
  })

  output$node_table_sub<- renderTable(colnames=TRUE, {
    model_nodesplit_sub()
  })

  # 3e. Bayesian result details 
  
  # Results details for all studies
  output$gemtc_results <- renderPrint ({             
    model()$sumresults
  })
  
  # Results details with studies excluded
  output$gemtc_results_sub <- renderPrint ({
    model_sub()$sumresults
  })
  
  # Gelman plots for all studies
  output$gemtc_gelman <- renderPlot ({              
    gelman.plot(model()$mtcResults)
  })
  
  # Gelman plots with studies excluded
  output$gemtc_gelman_sub <- renderPlot ({
    gelman.plot(model_sub()$mtcResults)
  })

  # 3f. Deviance report
  
  # Residual deviance from NMA model and UME inconsistency model for all studies
  umeplot <- eventReactive(input$baye_do, {      
    scat_plot(model())$p
  })
  
  output$dev_scat <- renderPlotly({
    umeplot()
  })
  
  # Residual deviance from NMA model and UME inconsistency model with studies excluded
  umeplot_sub <- eventReactive(input$sub_do, {
    scat_plot(model_sub())$p
  })
  
  output$dev_scat_sub <- renderPlotly({
    umeplot_sub()
  })

  # Per-arm residual deviance for all studies
  output$dev1 <- renderPlotly({
    stemplot(model())
  })
  
  # Per-arm residual deviance for sensitivity analysis
  output$dev1_sub <- renderPlotly({
    stemplot(model_sub())
  })
  
  # Leverage plot for all studies
  output$dev2 <- renderPlotly({
    levplot(model())
  })
  
  output$dev2_sub <- renderPlotly({
    levplot(model_sub())
  })
  
  # 3g. Model details
  
  # 3g-1 Model codes
  output$code <- renderPrint({
    cat(model()$mtcResults$model$code, fill=FALSE, labels=NULL, append=FALSE)
  })
  
  # 3g-2 Initial values
  output$inits <- renderPrint({
    model()$mtcResults$model$inits
  })
  
  # 3g-3 Download codes are in the download file.
  
  # 3g-4 Output deviance
  
  # NMA consistency model (all studies)
  output$dev <- renderPrint({
    mtc.deviance({model()$mtcResults})
  })
  
  # NMA consistency model (sensitivity)
  output$dev_sub <- renderPrint({
    mtc.deviance({model_sub()$mtcResults})
  })
  
  # UME inconsistency model (all studies)
  output$dev_ume<- renderPrint({
    scat_plot(model())$y
  })
  
  # UME inconsistency model (sensitivity)
  output$dev_ume_sub<- renderPrint({
    scat_plot(model_sub())$y
  })
})
