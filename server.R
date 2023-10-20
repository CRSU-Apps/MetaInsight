###### Bayesian V4 ######

#if packages not installed, please install them first by running the line below.
#install.packages(c("dplyr","metafor", "netmeta","shiny", "shinyAce","rmarkdown", "knitr", "gemtc", "plyr", data.table"
#  , "shinyalert", "plotly"))

# the data for meta-regression is from: http://nicedsu.org.uk/wp-content/uploads/2016/03/TSD3-Heterogeneity.final-report.08.05.12.pdf


shinyServer(function(input, output, session) {
  
  #####
  # Reactive functions used in various places
  #####
  
  # Define outcome measure (continuous or binary) - NVB
  outcome_measure <- reactive({
    if (input$metaoutcome == "Continuous") {
      return(input$outcomeCont)
    } else {
      return(input$outcomebina)
    }
  })
  
  
  ############################################
  ############# Load data page ###############
  ############################################
  
  data_reactives <- load_data_page_server(id = 'load_data_page',
                                          metaoutcome = function() {
                                            return(input$metaoutcome)
                                          })
  data <- data_reactives$data
  treatment_df <- data_reactives$treatment_df
  
  #####
  # Reactive functions used in various places, based on the data
  #####
  
  # Make frequentist function (in fn_analysis.R) reactive - NVB
  freq_all <- reactive({
    return(frequentist(data(), input$metaoutcome, treatment_df(), outcome_measure(), input$modelranfix))
  })
  
  exclusions <- debounce(reactive({input$exclusionbox}), 1500)
  
  # Make frequentist function (in fn_analysis.R) reactive with excluded studies - NVB
  freq_sub <- reactive({
    return(frequentist(data(), input$metaoutcome, treatment_df(), outcome_measure(), input$modelranfix, exclusions()))
  })
  
  # Make bugsnetdata function (in fn_analysis.R) reactive - NVB
  bugsnetdt <- reactive({
    return(bugsnetdata(data(), input$metaoutcome, treatment_df()))
  })
   
  # Make ref_alter function (in fn_analysis.R) reactive - NVB
  reference_alter <- reactive({
    return(ref_alter(data(), input$metaoutcome, exclusions(), treatment_df()))
  })

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
    

    filtertable <- function() {
      label <- treatment_df()
      dt <- data()
      ntx <- nrow(label)
      dt$T <- factor(dt$T,
                     levels = c(1:ntx),
                     labels = as.character(label$Label))
      return(dt)
    }

  colnames <- function(){
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
    summary_table_plot(filter(bugsnetdt(), !Study %in% exclusions()), input$metaoutcome)
  })
  
  # 1b. Study Results 
  
  # Forest plot
  output$forestPlot <- renderPlot({
    make_netStudy(freq_sub(), outcome_measure(), input$ForestHeader, input$ForestTitle)$fplot
  })
  
  output$downloadStudy <- downloadHandler(
    filename = function() {
      paste0('StudyResults.', input$format_freq0)
    },
    content = function(file) {
      if (input$format_freq0 == "PDF") {
        pdf(file = file, pointsize = input$ForestContent, width = 8, height = make_netStudy(freq_sub(), outcome_measure(), input$ForestHeader, input$ForestTitle)$size)
      } else {
        svg(file = file, pointsize = input$ForestContent, width = 8, height = make_netStudy(freq_sub(), outcome_measure(), input$ForestHeader, input$ForestTitle)$size)
      }
      make_netStudy(freq_sub(), outcome_measure(), input$ForestHeader, input$ForestTitle)
      dev.off()
    }
  )
  
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
      make_netplot(filter(bugsnetdt(), !Study %in% exclusions()), input$label_excluded)
    }
    title("Network plot with studies excluded")
  })
  
  # Network connectivity with studies excluded 
  output$netconnect_sub <- renderPrint ({
    make_netconnect(freq_sub())
  })
  
  output$downloadNetwork <- downloadHandler(
    filename = function() {
      paste0('Network.', input$format_freq1)
    },
    content = function(file) {
      draw_network <- function() {
        if (input$networkstyle == 'networkp1') {
          make_netgraph(freq_all(), input$label_all)
        } else {
          data.rh <- data.prep(arm.data = bugsnetdt(), varname.t = "T", varname.s = "Study")
          net.plot(data.rh, node.scale = 3, edge.scale = 1.5, node.lab.cex = input$label_all) 
        }
        title("Network plot of all studies")
      }
      write_to_pdf_or_png(
        file,
        input$format_freq1,
        draw_network
      )
    }
  )
  
  output$downloadNetworkUpdate <- downloadHandler(
    filename = function() {
      paste0('Network_sen.', input$format_freq2)
    },
    content = function(file) {
      draw_network <- function() {
        if (input$networkstyle_sub == 'networkp1') {
          make_netgraph(freq_sub(), input$label_excluded)
        } else {
          long_sort2_sub <- filter(bugsnetdt(), !Study %in% exclusions())  # subgroup
          data.rh <- data.prep(arm.data = long_sort2_sub, varname.t = "T", varname.s = "Study")
          net.plot(data.rh, node.scale = 3, edge.scale=1.5, node.lab.cex = input$label_excluded)
        }
        title("Network plot with studies excluded")
      }
      write_to_pdf_or_png(
        file,
        input$format_freq2,
        draw_network
      )
    }
  )

  
  ############### bugsnet code #################
  
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
  
  
  observeEvent(exclusions(),{
    longsort2 <- bugsnetdt()
    longsort2_sub <- filter(bugsnetdt(), !Study %in% exclusions())  # subgroup
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
    plotOutput("SFPUpdatingComp", height = BayesPixels(as.numeric(bugsnet_sumtb(filter(bugsnetdt(), !Study %in% exclusions()), input$metaoutcome)$Value[1]), title=TRUE), width = "630px")
  })
  
  output$downloadComp2 <- downloadHandler(
    filename = function() {
      paste0('All_studies.', input$format_freq3)
    },
    content = function(file) {
      if (input$format_freq3 == "PDF"){
        pdf(file= file, width = 9, height = BayesInch(as.numeric(bugsnet_sumtb(bugsnetdt(), input$metaoutcome)$Value[1])))
      } else {
        png(file = file, width = 610, height = BayesPixels(as.numeric(bugsnet_sumtb(bugsnetdt(), input$metaoutcome)$Value[1])))
      }
      make_netComp(freq_all(), input$modelranfix, reference_alter()$ref_all, input$freqmin, input$freqmax)
      dev.off()
    },
    contentType = "image/pdf"
  )
  
  output$downloadComp<- downloadHandler(
    filename = function() {
      paste0('Excluded_studies.', input$format_freq4)
    },
    content = function(file) {
      if (input$format_freq4 == "PDF") {
        pdf(file = file, width = 9, height=BayesInch(as.numeric(bugsnet_sumtb(filter(bugsnetdt(), !Study %in% exclusions()), input$metaoutcome)$Value[1])))
      } else {
        png(file = file, width = 610, height = BayesPixels(as.numeric(bugsnet_sumtb(filter(bugsnetdt(), !Study %in% exclusions()), input$metaoutcome)$Value[1])))
      }
      make_netComp(freq_sub(), input$modelranfix, reference_alter()$ref_sub, input$freqmin_sub, input$freqmax_sub)
      dev.off()
    }
  )
  
  ### 2b. Comparison and rank table
  
  output$rankChartStatic<- renderTable(colnames=FALSE,{
    make_netrank(freq_all(), input$modelranfix, input$rankopts)
  })
  output$rankChartUpdating<- renderTable(colnames=FALSE,{
    make_netrank(freq_sub(), input$modelranfix, input$rankopts)
  })
  
  output$downloadRank <- downloadHandler(
    filename = 'Rank.csv',
    content = function(file) {
      write.csv(make_netrank(freq_all(), input$modelranfix, input$rankopts), file)
    }
  )
  
  output$downloadRankUpdate <- downloadHandler(
    filename = 'RankUpdate.csv',
    content = function(file) {
      write.csv(make_netrank(freq_sub(), input$modelranfix, input$rankopts), file)
    }
  )

  ### 2c. Inconsistency

  output$Incon1 <- renderTable(colnames=TRUE, make_Incon(freq_all(), input$modelranfix))
  output$Incon2 <- renderTable(colnames=TRUE, make_Incon(freq_sub(), input$modelranfix))
  
  output$downloadIncon <- downloadHandler(
    filename = 'Inconsistency.csv',
    content = function(file) {
      write.csv(make_Incon(freq_all(), input$modelranfix), file)
    }
  )
  
  output$downloadIncon2 <- downloadHandler(
    filename = 'Inconsistency_sub.csv',
    content = function(file) {
      write.csv(make_Incon(freq_sub(), input$modelranfix), file)
    }
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
    bayesian_model(sub = FALSE, data(), treatment_df(), input$metaoutcome, exclusions(),
                   outcome_measure(), input$modelranfix, reference_alter())
  })
  
  model_sub <- eventReactive(input$sub_do, {
    bayesian_model(sub = TRUE, data(), treatment_df(), input$metaoutcome, exclusions(), 
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
    plotOutput("gemtc_sub", width="630px", height = BayesPixels(as.numeric(bugsnet_sumtb(filter(bugsnetdt(), !Study %in% exclusions()), input$metaoutcome)$Value[1]), title=TRUE))
  })
  
  output$downloadBaye_plot <- downloadHandler(
    filename = function() {
      paste0('All_studies.', input$format2)
    },
    content = function(file) {
      if (input$format2 == "PDF") {
        pdf(file = file, width = 9, height = BayesInch(as.numeric(bugsnet_sumtb(bugsnetdt(), input$metaoutcome)$Value[1])))
      } else {
        png(file = file, width = 610, height = BayesPixels(as.numeric(bugsnet_sumtb(bugsnetdt(), input$metaoutcome)$Value[1])))
      }
      if (input$metaoutcome == "Binary") {
        gemtc::forest(model()$mtcRelEffects, digits = 3, xlim = c(log(input$bayesmin), log(input$bayesmax)))
      }
      if (input$metaoutcome == "Continuous") {
        gemtc::forest(model()$mtcRelEffects, digits = 3, xlim = c(input$bayesmin, input$bayesmax))
      }
      dev.off()
    }
  )
  
  output$downloadBaye_plot_sub <- downloadHandler(
    filename = function() {
      paste0('Excluded_studies.', input$format4)
    },
    content = function(file) {
      if (input$format4 == "PDF") {
        pdf(file = file, width = 9, height = BayesInch(as.numeric(bugsnet_sumtb(filter(bugsnetdt(), !Study %in% exclusions()), input$metaoutcome)$Value[1])))
      } else {
        png(file = file, width = 610, height = BayesPixels(as.numeric(bugsnet_sumtb(filter(bugsnetdt(), !Study %in% exclusions()), input$metaoutcome)$Value[1])))
      }
      if (input$metaoutcome == "Binary") {
        gemtc::forest(model_sub()$mtcRelEffects, digits = 3, xlim = c(log(input$bayesmin_sub), log(input$bayesmax_sub)))
      }
      if (input$metaoutcome == "Continuous") {
        gemtc::forest(model_sub()$mtcRelEffects, digits = 3, xlim = c(input$bayesmin_sub, input$bayesmax_sub))
      }
      dev.off()
    }
  )

  
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
  
  output$downloadbaye_comparison <- downloadHandler(
    filename = 'baye_comparison.csv',
    content = function(file) {
      write.csv(baye_comp(model(), input$metaoutcome, outcome_measure()), file)
    }
  )
  
  output$downloadbaye_comparison_sub <- downloadHandler(
    filename = 'baye_comparison_sub.csv',
    content = function(file) {
      write.csv(baye_comp(model_sub(), input$metaoutcome, outcome_measure()), file)
    }
  )
  
  # 3c. Ranking Panel
  
  # Obtain Data needed for ranking #
  RankingData <- eventReactive(input$baye_do, {
    obtain_rank_data(data(), input$metaoutcome, 
                     treatment_df(), model(), input$rankopts)
  })
  
  RankingData_sub <- eventReactive(input$sub_do, {
    obtain_rank_data(data(), input$metaoutcome, treatment_df(),
                     model_sub(), input$rankopts, exclusions())
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
      make_netplot(filter(bugsnetdt_react_sub(), !Study %in% exclusions()), order=list(order=treat_order_sub()))
    }
    title("Network plot with studies excluded")
  })
  
  output$download_network_rank <- downloadHandler(  
    filename = function() {
      paste0('Network.', input$network_rank_choice)
    },
    content = function(file) {
      draw_network_rank <- function() {
        if (input$networkstyle_rank =='networkp1') {
          make_netgraph_rank(freq_all_react(), treat_order())
        } else {
          make_netplot(bugsnetdt_react(), order = list(order = treat_order()))
        }
        title("Network plot of all studies")
      }
      write_to_pdf_or_png(
        file,
        input$network_rank_choice,
        draw_network_rank
      )
    }
  )
  
  output$download_network_rank_sub <- downloadHandler(
    filename = function() {
      paste0('Network_sen.', input$network_rank_choice_sub)
    },
    content = function(file) {
      draw_network_rank <- function() {
        if (input$networkstyle_rank_sub == 'networkp1') {
          make_netgraph_rank(freq_all_react_sub(), treat_order_sub())
        } else {
          make_netplot(filter(bugsnetdt_react_sub(), !Study %in% exclusions()), order = list(order=treat_order_sub())) 
        }
        title("Network plot with studies excluded")
      }
      write_to_pdf_or_png(
        file,
        input$network_rank_choice_sub,
        draw_network_rank
      )
    }
  )
  
  # Forest plots for ranking panel (different style due to using 'boxes' in UI) CRN
  # All studies #
  output$gemtc2 <- renderPlot({                  
    png("forest.png")  # initialise image
    gemtc::forest(model()$mtcRelEffects,digits=3)
    dev.off()
    ForestImg <- magick::image_read('forest.png')
    Img <- cowplot::ggdraw() +
      cowplot::draw_image(ForestImg)
    
    file.remove('forest.png')
    
    return(Img)
  })
  # With studies excluded
  output$gemtc_sub2 <- renderPlot({                  
    png("forest_sub.png")
    gemtc::forest(model_sub()$mtcRelEffects,digits=3)
    dev.off()
    ForestImg <- magick::image_read('forest_sub.png')
    Img <- cowplot::ggdraw() +
      cowplot::draw_image(ForestImg)
    
    file.remove('forest_sub.png')
    
    return(Img)
  })
  
  output$download_rank_forest <- downloadHandler(  
    filename = function() {
      paste0('All_studies.', input$rank_forest_choice)
    },
    content = function(file) {
      draw_forest <- function() {
        gemtc::forest(model()$mtcRelEffects, digits = 3)
        title(paste("All studies: 
                Bayesian", model()$a, "consistency model forest plot results"), cex.main = 0.85)
      }
      write_to_pdf_or_png(
        file,
        input$rank_forest_choice,
        draw_forest
      )
    }
  )
  
  output$download_rank_forest_sub <- downloadHandler(  
    filename = function() {
      paste0('Subgroup.', input$rank_forest_choice_sub)
    },
    content = function(file) {
      draw_forest <- function() {
        gemtc::forest(model_sub()$mtcRelEffects,digits=3)
        title(paste("Results with studies excluded: 
                Bayesian", model()$a, "consistency model forest plot results"), cex.main = 0.85)
      }
      write_to_pdf_or_png(
        file,
        input$rank_forest_choice_sub,
        draw_forest
      )
    }
  )

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
  
  output$download_rank_plot <- downloadHandler(
    filename = function() {
      paste0('Ranking_Allstudies.png')
    },
    content = function(file) {
      if (input$rank_plot_choice == 0) { #Litmus Rank-O-Grams
        if (input$Colour_blind == FALSE) {
          ggsave(file, Rankplots()$Litmus, width = 6, height = 6, units = "in")
        } else {
          ggsave(file, Rankplots()$Litmus_blind, width = 6, height = 6, units = "in")
        }
      } else {  # Radial SUCRA plots
        if (input$Radial_alt == FALSE) { #Default plot
          if (input$Colour_blind == FALSE) {
            ggsave(file, Rankplots()$Radial$Original)
          } else {
            ggsave(file, Rankplots()$Radial_blind$Original)
          }
        } else { # Alternative plot
          if (input$Colour_blind == FALSE) {
            ggsave(file, Rankplots()$Radial$Alternative)
          } else {
            ggsave(file, Rankplots()$Radial_blind$Alternative)
          }
        }
      }
    }
  )
  
  output$download_rank_plot_sub <- downloadHandler(
    filename = function() {
      paste0('Ranking_Excludedstudies.png')
    },
    content = function(file) {
      if (input$rank_plot_choice_sub == 0) { #Litmus Rank-O-Grams
        if (input$Colour_blind_sub == FALSE) {
          ggsave(file, Rankplots_sub()$Litmus, width = 6, height = 6, units = "in")
        } else {
          ggsave(file, Rankplots_sub()$Litmus_blind, width = 6, height = 6, units = "in")
        }
      } else {  # Radial SUCRA plots
        if (input$Radial_alt_sub == FALSE) { #Default plot
          if (input$Colour_blind_sub == FALSE) {
            ggsave(file, Rankplots_sub()$Radial$Original)
          } else {
            ggsave(file, Rankplots_sub()$Radial_blind$Original)
          }
        } else { # Alternative plot
          if (input$Colour_blind_sub == FALSE) {
            ggsave(file, Rankplots_sub()$Radial$Alternative)
          } else {
            ggsave(file, Rankplots_sub()$Radial_blind$Alternative)
          }
        }
      }
    }
  )
  
  # Table of Probabilities (need to include SUCRA and have it as a collapsable table)
  output$rank_probs <- renderTable(
    {rank_probs_table(RankingData())}, 
    digits=2, rownames=FALSE, colnames=TRUE)
  output$rank_probs_sub <- renderTable(
    {rank_probs_table(RankingData_sub())}, 
    digits=2, rownames=FALSE, colnames=TRUE)
  
  output$download_rank_table <- downloadHandler(
    filename = 'RankingTable.csv',
    content = function(file) {
      write.csv(
        RankingData()$Probabilities %>% right_join(RankingData()$SUCRA[,1:2], by = "Treatment"),
        file,
        row.names=FALSE,
        col.names=TRUE
      )
    }
  )
  
  output$download_rank_table_sub <- downloadHandler(
    filename = 'RankingTable_Excluded.csv',
    content = function(file) {
      write.csv(
        RankingData_sub()$Probabilities %>% right_join(RankingData_sub()$SUCRA[,1:2], by = "Treatment"),
        file,
        row.names=FALSE,
        col.names=TRUE
      )
    }
  )
  
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
    nodesplit(sub = FALSE, data(), treatment_df(), input$metaoutcome, outcome_measure(),
                    input$modelranfix, exclusions())
  })

  output$node_table<- renderTable(colnames=TRUE, {
    model_nodesplit()
  })

  # Inconsistency test with notesplitting model with studies excluded
  model_nodesplit_sub <- eventReactive(input$node_sub, {
    nodesplit(sub = TRUE, data(), treatment_df(), input$metaoutcome, outcome_measure(),
                    input$modelranfix, exclusions())
  })

  output$node_table_sub<- renderTable(colnames=TRUE, {
    model_nodesplit_sub()
  })
  
  output$downloadnode <- downloadHandler(
    filename = 'Nodesplit.csv',
    content = function(file) {
      write.csv(model_nodesplit(), file)
    }
  )
  
  output$downloadnode_sub <- downloadHandler(
    filename = 'Nodesplit_sen.csv',
    content = function(file) {
      write.csv(model_nodesplit_sub(), file)
    }
  )

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
  
  output$download_code <- downloadHandler(
    filename = "code.txt",
    content = function(file){
      file.copy("./codes.txt", file)
    }
  )
  
  # 3g-2 Initial values
  output$inits <- renderPrint({
    model()$mtcResults$model$inits
  })
  
  #' Create a download handler for the initial values for a given chain
  #'
  #' @param index the Index of the chain
  #' @return The created download handler
  create_chain_initial_data_download_handler <- function(index) {
    filename <- paste0("initialvalues_chain", index, ".txt")
    return(
      downloadHandler(
        filename = filename,
        content = function(file) {
          lapply(
            model()$mtcResults$model$inits[[index]],
            write,
            file,
            append = TRUE,
            ncolumns=1000
          )
        }
      )
    )
  }
  
  output$download_inits_1 <- create_chain_initial_data_download_handler(1)
  output$download_inits_2 <- create_chain_initial_data_download_handler(2)
  output$download_inits_3 <- create_chain_initial_data_download_handler(3)
  output$download_inits_4 <- create_chain_initial_data_download_handler(4)
  
  # 3g-3 Chain data.
  
  #' Create a download handler for the data for a given chain
  #'
  #' @param index the Index of the chain
  #' @return The created download handler
  create_chain_data_download_handler <- function(index) {
    return(
      downloadHandler(
        filename = paste0("data_for_chain_", index, ".csv"),
        content = function(file) {
          data <- as.data.frame(model()$mtcResults$samples[[index]])
          write.csv(data, file)
        }
      )
    )
  }
  
  output$download_data1 <- create_chain_data_download_handler(1)
  output$download_data2 <- create_chain_data_download_handler(2)
  output$download_data3 <- create_chain_data_download_handler(3)
  output$download_data4 <- create_chain_data_download_handler(4)
  
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
  
  MetaRegressionTabServer(
    id = "meta_regression",
    all_data = data
  )
  
    
  shiny::observeEvent(
    data,
    {
      # Check if any covariates in data
      # if (length(FindCovariateNames(data)) == 0) {
      shiny::hideTab(
        inputId = "main_tabs",
        target = "4. Meta-regression"
      )
      # } else {
      #   shiny::showTab(
      #     inputId = "main_tabs",
      #     target = "4. Meta-regression"
      #   )
      # }
    }
  )
  
  output$UG <- downloadHandler(
    filename = "MetaInsightUserGuide.pdf",
    content = function(file) {
      file.copy("www/MetaInsightUserGuide.pdf", file)
    }
  )
  
})
