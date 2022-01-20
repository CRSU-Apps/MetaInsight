###### Bayesian V4 ######

#if packages not installed, please install them first by running the line below.
#install.packages(c("dplyr","metafor", "netmeta","shiny", "shinyAce","rmarkdown", "knitr", "gemtc", "plyr", data.table"
#  , "shinyalert", "plotly"))

# the data for meta-regression is from: http://nicedsu.org.uk/wp-content/uploads/2016/03/TSD3-Heterogeneity.final-report.08.05.12.pdf

library(dplyr)
library(metafor)
library(netmeta)
library(shiny) 
library(shinyAce)
library(rmarkdown)
library(knitr)
library(gemtc)
library(plyr)
library(data.table)
library(shinyalert)
library(plotly)
library(shinyjs)
library(BUGSnet)
library(shinyBS)
library(patchwork)
library(ggrepel)
library(tidyr)
library(magick)
library(reshape2)
library(stringr)



source("PlotFunctionsRKO.R", local = TRUE)        # Plot functions
load("blank.rds")                                 # Objects to store data for plot functions
source("fn_analysis.R",local = TRUE)              # functions for NMA


shinyServer(function(input, output, session) {
  source("downloadbuttons.R", local = TRUE)   #codes for download buttons for conciseness. This line must be put within the shinyserver as this is purely a code file not functions.
  
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
  
  
  
  ### view the full update history
  
  observeEvent(input$history_click, {
    newvalue <- "history"
    updateNavbarPage(session,"meta", selected="Full update history")
  })
  
  ### view the trouble shooting page
  
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
  
  
  ### Load default Data
  
  defaultD <- reactive({
    if (input$metaoutcome=='Continuous') {
      defaultD <- read.csv("./Cont_long.csv")
    } else {
      defaultD <- read.csv("./Binary_long.csv")
    }
  })
  
  
  
  ### Downloadable csv and labels of example dataset. download button codes are all in a separate code file
  
  
  
  ### Make data reactive
  data <- reactive({ 
    file1 <- input$data             # name the data file that was uploaded file1
    if(is.null(file1)){return(defaultD())}
    else
      a <- read.table(file = file1$datapath, sep =",", header=TRUE, stringsAsFactors = FALSE, quote="\"")
  })
  
  
  
  ### Data analysis tab
  output$tb <- renderTable({        # Create a table which displays the raw data just uploaded by the user
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
    lb <- ifelse(input$metaoutcome=="Continuous",input$listCont,input$listbina)
    label <- read.csv(text=lb, sep = "\t") 
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
  
  
  ### Refernce treatment if treatment 1 is removed from the network
  
  ref_alter <- function(){
    newData1 <- as.data.frame(data())
    label <- ifelse(input$metaoutcome=="Continuous",input$listCont,input$listbina)
    treat_list <- read.csv(text=label, sep = "\t")
    lstx <- treat_list$Label
    ref_all <- as.character(lstx[1])
    longsort2 <- dataform.df(newData1,treat_list,input$metaoutcome )
    long_sort2_sub <- filter(longsort2, !Study %in% input$exclusionbox)  # subgroup
    if (((lstx[1] %in% long_sort2_sub$T) ) == "TRUE") {
      ref_sub<- as.character(lstx[1])
    } else {
      ref_sub <- as.character(long_sort2_sub$T[1])
    }
    list(ref_all=ref_all, ref_sub=ref_sub)
  }
  
  
  output$ref_change_bay = output$ref_change <- renderText({
    if (identical(ref_alter()$ref_sub, ref_alter()$ref_all)=="FALSE") {
      paste("Please note that the reference treatment for sensitivity analysis has now been changed to:", ref_alter()$ref_sub, ". This is because the treatment labelled 1 has been removed from the network of sensitivity analysis." )
    }
  })
  
  
  
  
  
  #####################
  #### Frequentist ####
  #####################
  
  
  ### Frequentist analysis
  
  freq_all= function(){
    data_wide <- entry.df(data(),input$metaoutcome)    #transform data to wide form
    label <- ifelse(input$metaoutcome=="Continuous",input$listCont,input$listbina)
    treat_list <- read.csv(text=label, sep = "\t")   #read treatment labels from input
    outc <- ifelse (input$metaoutcome=="Continuous",input$outcomeCont, input$outcomebina)
    freq_wrap(data_wide, treat_list,input$modelranfix,outc,input$metaoutcome, ref_alter()$ref_all)  # use the selfdefined function, freq_wrap
    
  }
  freq_sub= function(){
    data_wide <-  entry.df(data(),input$metaoutcome)   
    data_sub <- filter(data_wide, !Study %in% input$exclusionbox)  # Get subset of data to use
    label <- ifelse(input$metaoutcome=="Continuous",input$listCont,input$listbina)
    treat_list <- read.csv(text=label, sep = "\t")
    outc <- ifelse (input$metaoutcome=="Continuous",input$outcomeCont, input$outcomebina)
    freq_wrap(data_sub, treat_list,input$modelranfix,outc, input$metaoutcome, ref_alter()$ref_sub)
  }
  
  
  
  
  ### 1b. Study results forest plot
  
  make_netStudy = function() {
    freq=freq_sub()
    outc <- ifelse (input$metaoutcome=="Continuous",input$outcomeCont, input$outcomebina)
    groupforest.df(freq$d0, freq$ntx, freq$lstx, outc, input$ForestHeader, input$ForestTitle)
  }
  output$forestPlot <- renderPlot({
    study_plot <- make_netStudy()
    study_plot$fplot
  })
  
  
  
  ### 1c. Network Plot
  make_netgraph = function(freq) {  
    netgraph(freq$net1, labels=str_wrap(sub("_", " ",freq$net1$trts), width=10), lwd=2, number.of.studies = TRUE, plastic=FALSE, points=TRUE, cex=1, cex.points=2, col.points=1, col=8, pos.number.of.studies=0.43,
             col.number.of.studies = "forestgreen", col.multiarm = "white", bg.number.of.studies = "forestgreen"
    )
  }
  
  bugsnetdt <- function(){
    newData1 <- as.data.frame(data())
    label <- ifelse(input$metaoutcome=="Continuous",input$listCont,input$listbina)
    treat_list <- read.csv(text=label, sep = "\t")
    treat_list$Label <- str_wrap(sub("_", " ",treat_list$Label), width=10)
    longsort2 <- dataform.df(newData1,treat_list,input$metaoutcome)    # inputting the data in long form
    return(longsort2)
  }
  
  output$netGraphStatic1 <- renderPlot({
    if (input$networkstyle=='networkp1') {
      make_netgraph(freq_all())
    } else {
      data.rh<-data.prep(arm.data=bugsnetdt(), varname.t = "T", varname.s="Study")
      net.plot(data.rh, node.scale = 3, edge.scale=1.5)  #, flag="Orlistat". 
    }
    title("Network plot of all studies")
  })
  
  output$netGraphUpdating <- renderPlot({
    if (input$networkstyle_sub=='networkp1') {
      make_netgraph(freq_sub())
    } else {
      long_sort2_sub <- filter(bugsnetdt(), !Study %in% input$exclusionbox)  # subgroup
      data.rh<-data.prep(arm.data=long_sort2_sub, varname.t = "T", varname.s="Study")
      net.plot(data.rh, node.scale = 3, edge.scale=1.5)
    }
    title("Network plot with studies excluded")
  })
  
  # network plots for ranking panel (Bayesian)
  treat_order <- reactive(RankingData()$SUCRA[order(RankingData()$SUCRA$SUCRA),1]) # obtain treatments ordered by SUCRA #
  make_netgraph_rank = function(freq, order) {  
    netgraph(freq$net1, labels=str_wrap(sub("_", " ",freq$net1$trts), width=10), lwd=2, number.of.studies = TRUE, plastic=FALSE, points=TRUE, cex=1, cex.points=2, col.points=1, col=8, pos.number.of.studies=0.43,
             col.number.of.studies = "forestgreen", col.multiarm = "white", bg.number.of.studies = "forestgreen", seq=sub(" ", "_", order),
    )
  }
  freq_all_react <- eventReactive(input$baye_do, {
    freq_all()
  })
  bugsnetdt_react <- eventReactive(input$baye_do, {
    bugsnetdt()
  })
  output$netGraphStatic1_rank <- renderPlot({
    if (input$networkstyle_rank=='networkp1') {
      make_netgraph_rank(freq_all_react(), treat_order())
    } else {
      data.rh<-data.prep(arm.data=bugsnetdt_react(), varname.t = "T", varname.s="Study")
      net.plot(data.rh, node.scale = 3, edge.scale=1.5, layout.params=list(order=treat_order()))  
    }
    title("Network plot of all studies")
  })
  treat_order_sub <- reactive(RankingData_sub()$SUCRA[order(RankingData_sub()$SUCRA$SUCRA),1])
  freq_all_react_sub <- eventReactive(input$sub_do, {
    freq_sub()
  })
  bugsnetdt_react_sub <- eventReactive(input$sub_do, {
    bugsnetdt()
  })
  output$netGraphStatic1_rank_sub <- renderPlot({
    if (input$networkstyle_rank_sub=='networkp1') {
      make_netgraph_rank(freq_all_react_sub(), treat_order_sub())
    } else {
      long_sort2_sub <- filter(bugsnetdt_react_sub(), !Study %in% input$exclusionbox)
      data.rh<-data.prep(arm.data=long_sort2_sub, varname.t = "T", varname.s="Study")
      net.plot(data.rh, node.scale = 3, edge.scale=1.5, layout.params=list(order=treat_order_sub()))  
    }
    title("Network plot with studies excluded")
  })
  

  make_netconnect = function(freq) {    # network connectivity
    d1 <- freq$d1
    nc1 <- netconnection(d1$treat1,d1$treat2,d1$studlab, data=NULL)
    print(nc1)
  }
  output$netconnect <- renderPrint ({
    make_netconnect(freq_all())
  })
  output$netconnect_sub <- renderPrint ({
    make_netconnect(freq_sub())
  })
  
  
  ############### bugsnet code #################
  
  ### 1a. Data characteristics
  
  bugsnet_sumtb <- function(data){
    data.rh<-data.prep(arm.data=data, varname.t = "T", varname.s="Study")
    if (input$metaoutcome=="Continuous") {
      outcome = "Mean"
      typeO= "continuous"
    } else {
      outcome = "R"
      typeO = "binomial"
    }
    network.char <- net.tab(data = data.rh,
                            outcome = outcome,
                            N = "N",
                            type.outcome = typeO,
                            time = NULL)
    return(network.char$network)
  }
  
  output$sumtb <- renderTable({
    longsort2 <- bugsnetdt()    # inputting the data in long form
    bugsnet_sumtb(longsort2)
  })
  
  output$sumtb_sub <- renderTable({
    longsort2 <- bugsnetdt()    # inputting the data in long form
    longsort2_sub <- filter(bugsnetdt(), !Study %in% input$exclusionbox)  # subgroup
    bugsnet_sumtb(longsort2_sub)
  })
  
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
    sumtb_sub <- bugsnet_sumtb(longsort2_sub)
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
  
  
  
  
  
  
  ### 2a. Forest Plot
  
  make_netComp = function(freq, ref,min,max) {    # forest plot
    forest.df(freq$net1,input$modelranfix,freq$lstx, ref,min,max)
  }
  make_refText = function(ref) {
    #output$ref4<- renderText({"All outcomes are versus the reference treatment"})
    y <- paste("All outcomes are versus the reference treatment:", ref)
    return(y)
  }
  
  output$Comparison2<- renderPlot({
    make_netComp(freq_all(), ref_alter()$ref_all, input$freqmin, input$freqmax)
    title("Results for all studies")
  })
  output$SFPUpdatingComp <- renderPlot({
    make_netComp(freq_sub(), ref_alter()$ref_sub, input$freqmin_sub, input$freqmax_sub)
    title("Results with studies excluded")
  })
  
  texttau = function(freq){      # Tau
    tau<- round(freq$net1$tau,2)
    outc <- ifelse (input$metaoutcome=="Continuous",input$outcomeCont, input$outcomebina)
    tau.df(tau, freq$net1$k, freq$net1$n, input$modelranfix, outc)
  }
  output$textcomp<- renderText({
    texttau(freq_all())
  })
  output$text5<- renderText({ 
    texttau(freq_sub())
  })
  output$ref4 <- renderText({
    make_refText(ref_alter()$ref_all)
  })
  output$ref3 <- renderText({
    make_refText(ref_alter()$ref_sub)
  })
  
  
  
  
  ### 2b. Comparison and rank table
  
  make_netrank = function(freq) {
    model <- input$modelranfix
    league <- netleague(freq$net1, comb.random=(model=="random"), comb.fixed = (model=="fixed"), digits =2, seq= netrank(freq$net1, small = input$rankopts))
    if (model=="random"){
      leaguedf<- as.data.frame(league$random)
    }
    else {
      leaguedf<- as.data.frame(league$fixed)
    }
    leaguedf
  }
  output$rankChartStatic<- renderTable(colnames=FALSE,{
    make_netrank(freq_all())
  })
  output$rankChartUpdating<- renderTable(colnames=FALSE,{
    make_netrank(freq_sub())
  })
  
  
  
  ### 2c. Inconsistency
  
  make_Incon = function(freq) {
    incona<- netsplit(freq$net1)
    make_Incon<- netsplitresult.df(incona, input$modelranfix)
  }
  output$Incon1<- renderTable(colnames=TRUE, {
    make_Incon(freq_all())}
  )
  output$Incon2<- renderTable(colnames=TRUE, {
    make_Incon(freq_sub())}
  )
  
  
  
  
  
  #####################
  #### 3. Bayesian ####
  #####################
  
  
  
  ### SMD warninig alert
  
  observeEvent(list(input$baye_do,input$sub_do, input$node,input$node_sub), {
    if (input$outcomeCont=="SMD") {
      showNotification("Please note: standardised mean difference currently cannot be analysed in Bayesian analysis", type = "error", duration = NULL)
    } 
    else if (input$outcomebina=="RD") {
      showNotification("Please note: Risk difference currently cannot be analysed in Bayesian analysis", type = "error", duration = NULL)
    }
  })
  
  
  
  ### Bayesian analysis
  
  model <- eventReactive(input$baye_do, {
    newData1 <- as.data.frame(data())
    label <- ifelse(input$metaoutcome=="Continuous",input$listCont,input$listbina)
    treat_list <- read.csv(text=label, sep = "\t")
    longsort2 <- dataform.df(newData1,treat_list,input$metaoutcome)    # inputting the data in long form
    outc <- ifelse (input$metaoutcome=="Continuous",input$outcomeCont, input$outcomebina)
    baye(longsort2,treat_list,input$modelranfix, outc,input$metaoutcome, ref_alter()$ref_all )
  })
  
  model_sub <- eventReactive(input$sub_do, {
    newData1 <- as.data.frame(data())
    label <- ifelse(input$metaoutcome=="Continuous",input$listCont,input$listbina)
    treat_list <- read.csv(text=label, sep = "\t")
    longsort2 <- dataform.df(newData1,treat_list,input$metaoutcome )
    long_sort2_sub <- filter(longsort2, !Study %in% input$exclusionbox)  # subgroup
    outc <- ifelse (input$metaoutcome=="Continuous",input$outcomeCont, input$outcomebina)
    baye(long_sort2_sub,treat_list,input$modelranfix, outc,input$metaoutcome, ref_alter()$ref_sub)
  })
  
  
  ### 3a. Forest plot
  
  output$gemtc <- renderPlot({                  # forest plot
    forest(model()$mtcRelEffects,digits=3,xlim=c(log(input$bayesmin), log(input$bayesmax)))
    title(paste("All studies: 
              Bayesian", model()$a, "consistency model forest plot results"))
  })
  output$gemtc2 <- renderPlot({                  # forest plot for ranking panel (different style needed due to using 'boxes' in UI)
    png("forest.png")
    forest(model()$mtcRelEffects,digits=3)
    dev.off()
    ForestImg <- image_read('forest.png')
    Img <- ggdraw() +
      draw_image(ForestImg)
    return(Img)
  })
  output$gemtc_sub <- renderPlot({
    forest(model_sub()$mtcRelEffects,digits=3,xlim=c(log(input$bayesmin_sub), log(input$bayesmax_sub)))
    title(paste("Results with studies excluded: 
              Bayesian", model_sub()$a,"consistency model forest plot results"))
  })
  output$gemtc_sub2 <- renderPlot({                  
    png("forest_sub.png")
    forest(model_sub()$mtcRelEffects,digits=3)
    dev.off()
    ForestImg <- image_read('forest_sub.png')
    Img <- ggdraw() +
      draw_image(ForestImg)
    return(Img)
  })
  
  texttauB = function(results){      # Tau
    outc <- ifelse (input$metaoutcome=="Continuous",input$outcomeCont, input$outcomebina)
    gemtctau(results,outc)
  }
  
  output$text_gemtc <-renderText({          # tau
    texttauB(model())
  })
  output$text_gemtc_sub <-renderText({
    texttauB(model_sub())
  })
  
  output$dic <- renderTable ({                  # DIC table
    model()$dic
  }, digits=3, rownames=TRUE, colnames=FALSE
  )
  output$dic_sub <- renderTable ({
    model_sub()$dic
  }, digits=3, rownames=TRUE, colnames=FALSE)
  
  
  
  
  ### 3b. comparison of all treatment pairs
  
  baye_comp <- function(baye){
    tbl <- relative.effect.table(baye$mtcResults)
    if ((input$metaoutcome=="Binary") & (input$outcomebina!="RD")) {
      tbl<-exp(tbl)
    } 
    as.data.frame(round(tbl, digits=2))
  }
  
  output$baye_comparison <- renderTable ({
    baye_comp(model())
  }, digits=2, rownames=TRUE, colnames = TRUE
  )
  output$baye_comparison_sub <- renderTable ({
    baye_comp(model_sub())
  }, digits=2, rownames=TRUE, colnames = TRUE
  )
  
  
  ### 3c. Litmus Rank-o-gram & Radial SUCRA

  # Obtain Data needed for ranking #
  RankingData <- eventReactive(input$baye_do, {
    newData1 <- as.data.frame(data())
    label <- ifelse(input$metaoutcome=="Continuous",input$listCont,input$listbina)
    treat_list <- as.data.frame(read.csv(text=label, sep = "\t"))
    longsort2 <- dataform.df(newData1,treat_list,input$metaoutcome)
    data_wide <- entry.df(data(),input$metaoutcome)    #transform data to wide form
    rankdata(NMAdata=model()$mtcResults, rankdirection=input$rankopts, 
             longdata=longsort2, widedata=data_wide, netmeta=freq_all())
  })
  RankingData_sub <- eventReactive(input$sub_do, {
    newData1 <- as.data.frame(data())
    label <- ifelse(input$metaoutcome=="Continuous",input$listCont,input$listbina)
    treat_list <- data.frame(read.csv(text=label, sep = "\t"))
    longsort2 <- dataform.df(newData1,treat_list,input$metaoutcome)
    long_sort2_sub <- filter(longsort2, !Study %in% input$exclusionbox)  # subgroup
    data_wide <- entry.df(data(),input$metaoutcome)
    data_wide_sub <- filter(data_wide, !Study %in% input$exclusionbox)  # Get subset of data to use
    rankdata(NMAdata=model_sub()$mtcResults, rankdirection=input$rankopts,
             longdata=long_sort2_sub, widedata=data_wide_sub, netmeta=freq_sub())
  })
  
  # All rank plots in one function for easier loading when switching options #
  Rankplots <- reactive({
    plots <- list()
    plots$Litmus <- LitmusRankOGram(CumData=RankingData()$Cumulative, SUCRAData=RankingData()$SUCRA, ColourData=RankingData()$Colour, colourblind=FALSE)
    plots$Radial <- RadialSUCRA(SUCRAData=RankingData()$SUCRA, ColourData=RankingData()$Colour, NetmetaObj=RankingData()$NetmetaObj$net1, colourblind=FALSE)
    plots$Litmus_blind <- LitmusRankOGram(CumData=RankingData()$Cumulative, SUCRAData=RankingData()$SUCRA, ColourData=RankingData()$Colour, colourblind=TRUE)
    plots$Radial_blind <- RadialSUCRA(SUCRAData=RankingData()$SUCRA, ColourData=RankingData()$Colour, NetmetaObj=RankingData()$NetmetaObj$net1, colourblind=TRUE)
    plots
  })
  Rankplots_sub <- reactive({
    plots <- list()
    plots$Litmus <- LitmusRankOGram(CumData=RankingData_sub()$Cumulative, SUCRAData=RankingData_sub()$SUCRA, ColourData=RankingData_sub()$Colour, colourblind=FALSE)
    plots$Radial <- RadialSUCRA(SUCRAData=RankingData_sub()$SUCRA, ColourData=RankingData_sub()$Colour, NetmetaObj=RankingData_sub()$NetmetaObj$net1, colourblind=FALSE)
    plots$Litmus_blind <- LitmusRankOGram(CumData=RankingData_sub()$Cumulative, SUCRAData=RankingData_sub()$SUCRA, ColourData=RankingData_sub()$Colour, colourblind=TRUE)
    plots$Radial_blind <- RadialSUCRA(SUCRAData=RankingData_sub()$SUCRA, ColourData=RankingData_sub()$Colour, NetmetaObj=RankingData_sub()$NetmetaObj$net1, colourblind=TRUE)
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
  output$rank_probs <- renderTable({
    Probs <- setDT(RankingData()$Probabilities, keep.rownames = "Treatment")
    Probs$Treatment <- str_wrap(sub("_", " ", Probs$Treatment), width=10)
    Probs <- Probs %>% right_join(RankingData()$SUCRA[,1:2], by="Treatment")
    Probs[order(-Probs$SUCRA),]
  }, digits=2, rownames=FALSE, colnames=TRUE)
  output$rank_probs_sub <- renderTable({
    Probs <- setDT(RankingData_sub()$Probabilities, keep.rownames = "Treatment")
    Probs$Treatment <- str_wrap(sub("_", " ", Probs$Treatment), width=10)
    Probs <- Probs %>% right_join(RankingData_sub()$SUCRA[,1:2], by="Treatment")
    Probs[order(-Probs$SUCRA),]
  }, digits=2, rownames=FALSE, colnames=TRUE)
  
  # Text underneath
  output$relative_rank_text <-renderText({          
    relative_rank_text(model())
  })
  output$relative_rank_text_sub <-renderText({          
    relative_rank_text(model_sub())
  })
  
  
  
  ### 3d. nodesplit model
  
  model_nodesplit <- eventReactive(input$node, {
    newData1 <- as.data.frame(data())
    label <- ifelse(input$metaoutcome=="Continuous",input$listCont,input$listbina)
    treat_list <- read.csv(text=label, sep = "\t")
    longsort2 <- dataform.df(newData1,treat_list,input$metaoutcome)
    outc <- ifelse (input$metaoutcome=="Continuous",input$outcomeCont, input$outcomebina)
    bayenode(longsort2,treat_list, input$modelranfix, outc,input$metaoutcome )
  })
  output$node_table<- renderTable(colnames=TRUE, {
    model_nodesplit()
  })
  
  model_nodesplit_sub <- eventReactive(input$node_sub, {
    newData1 <- as.data.frame(data())
    label <- ifelse(input$metaoutcome=="Continuous",input$listCont,input$listbina)
    treat_list <- read.csv(text=label, sep = "\t")
    longsort2 <- dataform.df(newData1,treat_list,input$metaoutcome)
    longsort2_sub <- filter(longsort2, !Study %in% input$exclusionbox)
    outc <- ifelse (input$metaoutcome=="Continuous",input$outcomeCont, input$outcomebina)
    bayenode(longsort2_sub,treat_list, input$modelranfix, outc,input$metaoutcome)
  })
  output$node_table_sub<- renderTable(colnames=TRUE, {
    model_nodesplit_sub()
  })
  
  
  
  ### 3e. Bayesian result details and gelman
  
  output$gemtc_results <- renderPrint ({             # Results details
    model()$sumresults
  })
  output$gemtc_results_sub <- renderPrint ({
    model_sub()$sumresults
  })
  
  output$gemtc_gelman <- renderPlot ({              # Gelman plots
    gelman.plot(model()$mtcResults)
  })
  output$gemtc_gelman_sub <- renderPlot ({
    gelman.plot(model_sub()$mtcResults)
  })
  
  
  
  
  ### 3f. Deviance 
  
  scat_plot = function(baye){   # ume scatter plot
    mod_list=baye
    x<-mtc.deviance({mod_list$mtcResults})
    c <- data.frame(x$dev.ab)
    outc <- ifelse (input$metaoutcome=="Continuous",input$outcomeCont, input$outcomebina)
    umeplot.df(c,mod_list$mtcNetwork, mod_list$model, mod_list$outcome)
  }
  umeplot <- eventReactive(input$baye_do, {      # to prevent 
    scat_plot(model())$p
  })
  
  output$dev_scat <- renderPlotly({
    umeplot()
  })
  
  umeplot_sub <- eventReactive(input$sub_do, {
    scat_plot(model_sub())$p
  })
  output$dev_scat_sub <- renderPlotly({
    umeplot_sub()
  })
  
  stemplot <- function(baye) {   # stemplot
    mod_list=baye
    x<-mtc.deviance({mod_list$mtcResults})
    c <- data.frame(x$dev.ab)
    c$names <- rownames(c)
    p<-stemplot.df(c,x)
  }
  output$dev1 <- renderPlotly({
    stemplot(model())
  })
  output$dev1_sub <- renderPlotly({
    stemplot(model_sub())
  })
  
  levplot <- function(baye) {    # leverage plot
    mod_list=baye
    x<-mtc.deviance({mod_list$mtcResults})
    p<-levplot.df(x)
  }
  output$dev2 <- renderPlotly({
    levplot(model())
  })
  output$dev2_sub <- renderPlotly({
    levplot(model_sub())
  })
  
  
  ### 3g.1 Model codes
  output$code <- renderPrint({
    cat(model()$mtcResults$model$code, fill=FALSE, labels=NULL, append=FALSE)
  })
  
  
  ### 3g.2 initial values
  output$inits <- renderPrint({
    model()$mtcResults$model$inits
  })
  
  ### 3g.3 download codes are in the download file.
  
  ### 3g.4 output deviance
  output$dev <- renderPrint({
    mtc.deviance({model()$mtcResults})
  })
  output$dev_sub <- renderPrint({
    mtc.deviance({model_sub()$mtcResults})
  })
  output$dev_ume<- renderPrint({
    scat_plot(model())$y
  })
  output$dev_ume_sub<- renderPrint({
    scat_plot(model_sub())$y
  })
})
