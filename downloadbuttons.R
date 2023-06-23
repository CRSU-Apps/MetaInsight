## MetaInsight - downloadable files for data uploading instructions - code.


## Sizing functions for forest plots ##
BayesPixels <- function(notrt, title=FALSE) {    # input is total number of treatments and whether title is included in plot
  if (title==FALSE) {
    if (notrt <= 25) {
      height <- 420        # default for 25 or less treatments
    } else {
      height <- 15*(notrt-1) + 60
    }
  } else {
    if (notrt <= 25) {
      height <- 420 + 100        # default for 25 or less treatments
    } else {
      height <- 15*(notrt-1) + 60 + 100
    }
  }
  return(height)
}
BayesInch <- function(notrt) {
  if (notrt <= 25) {
    height <- 6
  } else {
    height <- 6 + 0.2*(notrt-25)
  }
  return(height)
}

write_to_pdf_or_png <- function(file, type, renderFunction) {
  if (tolower(type) == "pdf") {
    pdf(file = file)
  } else {
    png(file = file)
  }
  renderFunction()
  dev.off()
}


setup_dowload_handlers <- function(input, output) {
  #########################
  ### Tab 2 - Load data ###
  #########################
  
  create_raw_data_download_handler <- function(filename, continuousFile, bindaryFile) {
    return(
      downloadHandler(
        filename = filename,
        content = function(file){
          if (input$metaoutcome=='Continuous') {
            file.copy(continuousFile, file)
          } else {
            file.copy(bindaryFile, file)
          }
        }
      )
    )
  }
  
  ##### in the 'upload long data' tab
  output$downloadData <- create_raw_data_download_handler("MetaInsightdataLONG.csv", "Cont_long.csv", "Binary_long.csv")
  output$downloadlabel <- create_raw_data_download_handler("treatmentlabels.txt", "defaultlabels_continuous.txt", "defaultlabels_binary.txt")
  
  ##### in the 'UPload wide data' tab
  output$downloadDataWide <- create_raw_data_download_handler("MetaInsightdataWIDE.csv", "Cont_wide.csv", "Binary_wide.csv")
  output$downloadlabel2 <- create_raw_data_download_handler("treatmentlabels.txt", "defaultlabels_continuous.txt", "defaultlabels_binary.txt")
  
  
  #############################
  ### Tab 3 - Data analysis ###
  #############################
  
  ##### 1b. Study Results - download the grouped forest plot
  
  output$downloadStudy <- downloadHandler(
    filename = function() {
      paste0('StudyResults.', input$format_freq0)
    },
    content = function(file) {
      if (input$format_freq0 == "PDF"){
        pdf(file = file, pointsize = input$ForestContent, width = 8, height = make_netStudy(freq_sub(), outcome_measure(), input$ForestHeader, input$ForestTitle)$size)
      } else {
        svg(file = file, pointsize = input$ForestContent, width = 8, height = make_netStudy(freq_sub(), outcome_measure(), input$ForestHeader, input$ForestTitle)$size)
      }
      make_netStudy(freq_sub(), outcome_measure(), input$ForestHeader, input$ForestTitle)
      dev.off()
    }
  )
  
  
  ##### 1c. Network plot
  
  output$downloadNetwork <- downloadHandler(
    filename = function() {
      paste0('Network.', input$format_freq1)
    },
    content = function(file) {
      draw_network <- function() {
        if (input$networkstyle == 'networkp1') {
          make_netgraph(freq_all(), input$label_all)
        } else {
          data.rh <- data.prep(arm.data=bugsnetdt(), varname.t = "T", varname.s = "Study")
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
          long_sort2_sub <- filter(bugsnetdt(), !Study %in% input$exclusionbox)  # subgroup
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
  
  
  ##### 2a. Forest plot
  
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
        pdf(file = file, width = 9, height=BayesInch(as.numeric(bugsnet_sumtb(filter(bugsnetdt(), !Study %in% input$exclusionbox), input$metaoutcome)$Value[1])))
      } else {
        png(file = file, width = 610, height = BayesPixels(as.numeric(bugsnet_sumtb(filter(bugsnetdt(), !Study %in% input$exclusionbox), input$metaoutcome)$Value[1])))
      }
      make_netComp(freq_sub(), input$modelranfix, reference_alter()$ref_sub, input$freqmin_sub, input$freqmax_sub)
      dev.off()
    }
  )
  
  
  ##### 2b. Comparison of all treatment pairs - comparison rank table
  
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
  
  
  ##### 2c. Inconsistency
  
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
  
  #### 3a. Forest plot
  
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
        forest(model()$mtcRelEffects, digits = 3, xlim = c(log(input$bayesmin), log(input$bayesmax)))
      }
      if (input$metaoutcome == "Continuous") {
        forest(model()$mtcRelEffects, digits = 3, xlim = c(input$bayesmin, input$bayesmax))
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
        pdf(file = file, width = 9, height = BayesInch(as.numeric(bugsnet_sumtb(filter(bugsnetdt(), !Study %in% input$exclusionbox), input$metaoutcome)$Value[1])))
      } else {
        png(file = file, width = 610, height = BayesPixels(as.numeric(bugsnet_sumtb(filter(bugsnetdt(), !Study %in% input$exclusionbox), input$metaoutcome)$Value[1])))
      }
      if (input$metaoutcome == "Binary") {
        forest(model_sub()$mtcRelEffects, digits = 3, xlim = c(log(input$bayesmin_sub), log(input$bayesmax_sub)))
        }
      if (input$metaoutcome == "Continuous") {
        forest(model_sub()$mtcRelEffects, digits = 3, xlim = c(input$bayesmin_sub, input$bayesmax_sub))
      }
      dev.off()
    }
  )
  
  
  ##### 3b. Comparison of all treatment pairs
  
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
  
  
  ##### 3c. Ranking panel CRN
  
  # Network plots #
  
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
          make_netplot(filter(bugsnetdt_react_sub(), !Study %in% input$exclusionbox), order = list(order=treat_order_sub())) 
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
  
  
  # Forest plots #
  
  output$download_rank_forest <- downloadHandler(  
    filename = function() {
      paste0('All_studies.', input$rank_forest_choice)
    },
    content = function(file) {
      draw_forest <- function() {
        forest(model()$mtcRelEffects, digits = 3)
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
        forest(model_sub()$mtcRelEffects,digits=3)
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
  
  
  # Rank plots #
  
  output$download_rank_plot <- downloadHandler(
    filename = function() {
      paste0('Ranking_Allstudies.png')
    },
    content = function(file) {
      if (input$rank_plot_choice==0) { #Litmus Rank-O-Grams
        if (input$Colour_blind==FALSE) {ggsave(file,Rankplots()$Litmus,width=6,height=6,units="in")} else {ggsave(file,Rankplots()$Litmus_blind,width=6,height=6,units="in")}
      } else {  # Radial SUCRA plots
        if (input$Radial_alt==FALSE) { #Default plot
          if (input$Colour_blind==FALSE) {ggsave(file,Rankplots()$Radial$Original)} else {ggsave(file,Rankplots()$Radial_blind$Original)}
        } else { # Alternative plot
          if (input$Colour_blind==FALSE) {ggsave(file,Rankplots()$Radial$Alternative)} else {ggsave(file,Rankplots()$Radial_blind$Alternative)}
        }
      }
    }
  )
  
  output$download_rank_plot_sub <- downloadHandler(
    filename = function() {
      paste0('Ranking_Excludedstudies.png')
    },
    content = function(file) {
      if (input$rank_plot_choice_sub==0) { #Litmus Rank-O-Grams
        if (input$Colour_blind_sub==FALSE) {ggsave(file,Rankplots_sub()$Litmus,width=6,height=6,units="in")} else {ggsave(file,Rankplots_sub()$Litmus_blind,width=6,height=6,units="in")}
      } else {  # Radial SUCRA plots
        if (input$Radial_alt_sub==FALSE) { #Default plot
          if (input$Colour_blind_sub==FALSE) {ggsave(file,Rankplots_sub()$Radial$Original)} else {ggsave(file,Rankplots_sub()$Radial_blind$Original)}
        } else { # Alternative plot
          if (input$Colour_blind_sub==FALSE) {ggsave(file,Rankplots_sub()$Radial$Alternative)} else {ggsave(file,Rankplots_sub()$Radial_blind$Alternative)}
        }
      }
    }
  )
  
  output$download_rank_table <- downloadHandler(
    filename = 'RankingTable.csv',
    content = function(file) {
      write.csv(
        RankingData()$Probabilities %>% right_join(RankingData()$SUCRA[,1:2], by="Treatment"),
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
        RankingData_sub()$Probabilities %>% right_join(RankingData_sub()$SUCRA[,1:2], by="Treatment"),
        file,
        row.names=FALSE,
        col.names=TRUE
      )
    }
  )
  
  
  ##### 3d. nodesplit model
  
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
  
  
  ##### 3g.1 model code
  
  output$download_code <- downloadHandler(
    filename = "code.txt",
    content = function(file){
      file.copy("./codes.txt", file)
    }
  )
  
  
  ##### 3g.2 initial value
  
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
            append=TRUE,
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
  
  
  ##### 3g.3 download data
  
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
  
  
  ######## User Guide ########
  
  output$UG <- downloadHandler(
    filename = "MetaInsightUserGBayv0.1.pdf",
    content = function(file) {
      file.copy("MetaInsightUserGBayv0_1.pdf", file)
    }
  )
}
