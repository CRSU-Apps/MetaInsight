## MEtaInsight - downloadable files for data uploading instructions - code.


#########################
### Tab 2 - Load data ###
#########################

##### in the 'upload long data' tab

output$downloadData <- downloadHandler(
  filename <- function() {
    paste("MetaInsightdataLONG","csv", sep = ".")
  },
  content <- function(file){
    if (input$metaoutcome=='Continuous') {
      file.copy("./Cont_long.csv", file)
    } else {
      file.copy("./Binary_long.csv", file)
    }
  }
)

output$downloadlabel <- downloadHandler(
  filename <- function() {
    paste("treatmentlabels","txt", sep = ".")
  },
  content <- function(file){
    if (input$metaoutcome=='Continuous') {
      file.copy("./defaultlabels_continuous.txt", file)
    } else {
      file.copy("./defaultlabels_binary.txt", file)
    }
  }
)

##### in the 'UPload wide data' tab

output$downloadDataWide <- downloadHandler(
  filename <- function() {
    paste("MetaInsightdataWIDE","csv", sep = ".")
  },
  content <- function(file){
    if (input$metaoutcome=='Continuous') {
      file.copy("./Cont_wide.csv", file)
    } else {
      file.copy("./Binary_wide.csv", file)
  }}
)

output$downloadlabel2 <- downloadHandler(
  filename <- function() {
    paste("treatmentlabels","txt", sep = ".")
  },
  content <- function(file){
    if (input$metaoutcome=='Continuous') {
      file.copy("./defaultlabels_continuous.txt", file)
    } else {
      file.copy("./defaultlabels_binary.txt", file)
    }
  }
)

#############################
### Tab 3 - Data analysis ###
#############################

##### 1a. Study Results download the grouped forest plot
output$downloadStudy <- downloadHandler(
  filename = function() {
    paste0('StudyResults.', input$format_freq0)
  },
  content = function(file) {
    if (input$format_freq0=="PDF"){pdf(file=file, pointsize = input$ForestContent, height=make_netStudy()$size, width=8)}
    else {svg(file=file, pointsize = input$ForestContent, height=make_netStudy()$size, width=8)}
    make_netStudy()
    dev.off()
  }
)

##### 1b. Network plot
output$downloadNetwork <- downloadHandler(
  filename = function() {
    paste0('Network.', input$format_freq1)
  },
  content = function(file) {
    if (input$format_freq1=="PDF"){pdf(file=file)}
    else {png(file=file)}
    if (input$networkstyle=='networkp1') {
      make_netgraph(freq_all(),input$label_all)
    } else {
      data.rh<-data.prep(arm.data=bugsnetdt(), varname.t = "T", varname.s="Study")
      net.plot(data.rh, node.scale = 3, edge.scale=1.5, node.lab.cex=input$label_all) 
    }
    title("Network plot of all studies")
    dev.off()
  }
)
output$downloadNetworkUpdate <- downloadHandler(
  filename = function() {
    paste0('Network_sen.', input$format_freq2)
  },
  content = function(file) {
    if (input$format_freq2=="PDF"){pdf(file=file)}
    else {png(file=file)}
    if (input$networkstyle_sub=='networkp1') {
      make_netgraph(freq_sub(),input$label_excluded)
    } else {
      long_sort2_sub <- filter(bugsnetdt(), !Study %in% input$exclusionbox)  # subgroup
      data.rh<-data.prep(arm.data=long_sort2_sub, varname.t = "T", varname.s="Study")
      net.plot(data.rh, node.scale = 3, edge.scale=1.5, node.lab.cex=input$label_excluded)
    }
    title("Network plot with studies excluded")
    dev.off()
  }
)


##### 2a. forest plot
output$downloadComp2 <- downloadHandler(
  filename = function() {
    paste0('All_studies.', input$format_freq3)
  },
  content = function(file) {
    if (input$format_freq3=="PDF"){pdf(file=file)}
    else {png(file=file)}
    make_netComp(freq_all(), ref_alter()$ref_all, input$freqmin, input$freqmax)
    #title("All studies")
    #title(paste("All studies: 
    #          Frequentist", model()$a, "model forest plot results"))
    dev.off()
  },
  #contentType = function() {
  #  if (input$format_freq3=="PDF") {"image/pdf"}
  #  else {"image/png"}
  #}
  contentType = "image/pdf"
)
output$downloadComp<- downloadHandler(
  filename = function() {
    paste0('Excluded_studies.', input$format_freq4)
  },
  content = function(file) {
    if (input$format_freq4=="PDF"){pdf(file=file)}
    else {png(file=file)}
    make_netComp(freq_sub(), ref_alter()$ref_sub, input$freqmin_sub, input$freqmax_sub)
    #title(paste("Results with studies excluded: 
    #          Frequentist", model()$a, "model forest plot results"))
    dev.off()
  }
)








##### 2b. comparison rank table
output$downloadRank <- downloadHandler(
  filename = function() {
    paste('Rank.', '.csv', sep='')
  },
  content = function(file) {
    write.csv({
      make_netrank(freq_all())
      }, file)
  })
output$downloadRankUpdate <- downloadHandler(
  filename = function() {
    paste('RankUpdate.', '.csv', sep='')
  },
  content = function(file) {
    write.csv({
      make_netrank(freq_sub())
      }, file)
  })


##### 2c. inconsistency
output$downloadIncon <- downloadHandler(
  filename = function() {
    paste('Inconsistency.', '.csv', sep='')
  },
  content = function(file) {
    write.csv({make_Incon(freq_all())}, file)
  }
)
output$downloadIncon2 <- downloadHandler(
  filename = function() {
    paste('Inconsistency_sub.', '.csv', sep='')
  },
  content = function(file) {
    write.csv({make_Incon(freq_sub())}, file)
  }
)


#####################
#### 3. Bayesian ####
#####################

##### 3a. forest plot
output$downloadBaye_plot <- downloadHandler(
  filename = function() {
    paste0('All_studies.', input$format2)
  },
  content = function(file) {
    if (input$format2=="PDF"){pdf(file=file)}
    else {png(file=file)}
    if (input$metaoutcome=="Binary") {forest(model()$mtcRelEffects,digits=3,xlim=c(log(input$bayesmin), log(input$bayesmax)))}
    if (input$metaoutcome=="Continuous") {forest(model()$mtcRelEffects,digits=3,xlim=c(input$bayesmin, input$bayesmax))}
    #title(paste("All studies: 
    #          Bayesian", model()$a, "consistency model forest plot results"))
    dev.off()
  }
)

output$downloadBaye_plot_sub<- downloadHandler(
  filename = function() {
    paste0('Excluded_studies.', input$format4)
  },
  content = function(file) {
    if (input$format4=="PDF"){pdf(file=file)}
    else {png(file=file)}
    if (input$metaoutcome=="Binary") {forest(model_sub()$mtcRelEffects,digits=3,xlim=c(log(input$bayesmin_sub), log(input$bayesmax_sub)))}
    if (input$metaoutcome=="Continuous") {forest(model_sub()$mtcRelEffects,digits=3,xlim=c(input$bayesmin_sub, input$bayesmax_sub))}
    #title(paste("Results with studies excluded: 
    #          Bayesian", model()$a, "consistency model forest plot results"))
    dev.off()
  }
)



##### 3b. comparison of all treatment pairs
output$downloadbaye_comparison <- downloadHandler(
  filename = function() {
    paste('baye_comparison', '.csv', sep='')
  },
  content = function(file) {
    write.csv({baye_comp(model())}, file)
  }
)

output$downloadbaye_comparison_sub <- downloadHandler(
  filename = function() {
    paste('baye_comparison_sub', '.csv', sep='')
  },
  content = function(file) {
    write.csv({baye_comp(model_sub())}, file)
  }
)



##### 3c. Ranking table
output$downloadBaye_rank <- downloadHandler(
  filename = function() {
    paste('Rank_allstudies', '.csv', sep='')
  },
  content = function(file) {
    write.csv({
      prob <- as.data.frame(print(rank.probability(model()$mtcResults,
                                                   preferredDirection=(if (input$rankopts=="good") -1 else 1))))  
      names(prob)[1:ncol(prob)] <- paste("Rank ", 1:(ncol(prob)), sep="")
      prob
      }, file)
  }
)
output$downloadBaye_rank_sub <- downloadHandler(
  filename = function() {
    paste('Rank_subgroup', '.csv', sep='')
  },
  content = function(file) {
    write.csv({
      prob <- as.data.frame(print(rank.probability(model_sub()$mtcResults,preferredDirection=
                                                     (if (input$rankopts=="good") -1 else 1)))) 
      names(prob)[1:ncol(prob)] <- paste("Rank ", 1:(ncol(prob)), sep="")
      prob
      }, file)
  }
)



##### 3d. nodesplit model
output$downloadnode <- downloadHandler(
  filename = function() {
    paste('Nodesplit', '.csv', sep='')
  },
  content = function(file) {
    write.csv({model_nodesplit()}, file)
  }
)
output$downloadnode_sub <- downloadHandler(
  filename = function() {
    paste('Nodesplit_sen', '.csv', sep='')
  },
  content = function(file) {
    write.csv({model_nodesplit_sub()}, file)
  }
)


##### 3g.1 model code
output$download_code <- downloadHandler(
  filename <- function() {
    paste("code","txt", sep = ".")
  },
  content <- function(file){
    file.copy("./codes.txt", file)
  }
)


##### 3g.2 initial values
output$download_inits_1 <- downloadHandler(
  filename <- function() {
    paste("intialvalues_chain1","txt", sep = ".")
  },
  content <- function(file){
    lapply(model()$mtcResults$model$inits[[1]], write,"initialvalues_chain1.txt", append=TRUE,ncolumns=1000)
    file.copy("./initialvalues_chain1.txt", file)
  }
)

output$download_inits_2 <- downloadHandler(
  filename <- function() {
    paste("intialvalues_chain2","txt", sep = ".")
  },
  content <- function(file){
    lapply(model()$mtcResults$model$inits[[2]], write,"initialvalues_chain2.txt", append=TRUE,ncolumns=1000)
    file.copy("./initialvalues_chain2.txt", file)
  }
)

output$download_inits_3 <- downloadHandler(
  filename <- function() {
    paste("intialvalues_chain3","txt", sep = ".")
  },
  content <- function(file){
    lapply(model()$mtcResults$model$inits[[3]], write,"initialvalues_chain3.txt", append=TRUE,ncolumns=1000)
    file.copy("./initialvalues_chain3.txt", file)
  }
)

output$download_inits_4 <- downloadHandler(
  filename <- function() {
    paste("intialvalues_chain4","txt", sep = ".")
  },
  content <- function(file){
    lapply(model()$mtcResults$model$inits[[4]], write,"initialvalues_chain4.txt", append=TRUE,ncolumns=1000)
    file.copy("./initialvalues_chain4.txt", file)
  }
)


##### 3g.3 download data
output$download_data1 <- downloadHandler(
  filename = function() {
    paste('Data for chain 1', '.csv', sep='')
  },
  content = function(file) {
    data1<-as.data.frame(model()$mtcResults$samples[[1]])
    write.csv(data1, file)
  }
)

output$download_data2 <- downloadHandler(
  filename = function() {
    paste('Data for chain 2', '.csv', sep='')
  },
  content = function(file) {
    data2<-as.data.frame(model()$mtcResults$samples[[2]])
    write.csv(data2, file)
  }
)

output$download_data3 <- downloadHandler(
  filename = function() {
    paste('Data for chain 3', '.csv', sep='')
  },
  content = function(file) {
    data3<-as.data.frame(model()$mtcResults$samples[[3]])
    write.csv(data3, file)
  }
)

output$download_data4 <- downloadHandler(
  filename = function() {
    paste('Data for chain 4', '.csv', sep='')
  },
  content = function(file) {
    data4<-as.data.frame(model()$mtcResults$samples[[4]])
    write.csv(data4, file)
  }
)


######## User Guide ########

output$UG <- downloadHandler(
  filename <- function() {
    paste("MetaInsightUserGBayv0.1","pdf", sep = ".")
  },
  content <- function(file){
    file.copy("./MetaInsightUserGBayv0_1.pdf", file)
  }
)


