#####
# Plot functions used in both app and report - NVB
#####

# 1a Summary table plot 
summary_table_plot <- function(bugsnetdt, metaoutcome) {
  return(bugsnet_sumtb(bugsnetdt, metaoutcome))
}

# 1b Forest plot
make_netStudy <- function(freq, outcome_measure, ForestHeader, ForestTitle) {
  return(groupforest.df(freq$d0, freq$ntx, freq$lstx, outcome_measure, ForestHeader, ForestTitle))
}

# 1c Network plot - number of trials on line
make_netgraph <- function(freq, label_size) {  
  return(netgraph(freq$net1, lwd=2, number.of.studies = TRUE, plastic=FALSE, points=TRUE, cex=label_size, cex.points=2, col.points=1, col=8, pos.number.of.studies=0.43,
                  col.number.of.studies = "forestgreen", col.multiarm = "white", bg.number.of.studies = "forestgreen"))
}

# 1c Network plot - number of trials by nodesize and line thickness
make_netplot <- function(bugsnetdt, label_size=1, order=NULL) {    # added default values and extra option for ordering the nodes (CRN)  
  data.rh<-data.prep(arm.data=bugsnetdt, varname.t = "T", varname.s="Study")
  return(net.plot(data.rh, node.scale = 3, edge.scale=1.5, node.lab.cex=label_size, layout.params=order))
}

# 1c Creates network connectivity info displayed under network plots 
make_netconnect <- function(freq) {   
  d1 <- freq$d1
  nc1 <- netconnection(d1$treat1,d1$treat2,d1$studlab, data=NULL)
  print(nc1)
}

# 2a. Forest Plot
make_netComp <- function(freq, modelranfix, ref, min, max) {    
  return(forest.df(freq$net1, modelranfix, freq$lstx, ref, min, max))
}

# 2a. Creates text displayed under forest plots 
texttau <- function(freq, outcome_measure, modelranfix){      
  tau <- round(freq$net1$tau,2)
  return(tau.df(tau, freq$net1$k, freq$net1$n, modelranfix, outcome_measure))
}

make_refText = function(ref) {
  y <- paste("All outcomes are versus the reference treatment:", ref)
  return(y)
}

# 2b Treatment comparison and rank table
make_netrank <- function(freq, modelranfix, rankopts) {
  league <- netleague(freq$net1, 
                      comb.random=(modelranfix=="random"), comb.fixed = (modelranfix=="fixed"), 
                      digits =2, seq= netrank(freq$net1, small = rankopts))
  if (modelranfix=="random"){
    leaguedf<- as.data.frame(league$random)
  }
  else {
    leaguedf<- as.data.frame(league$fixed)
  }
  return(leaguedf)
}

# 2c Inconsistency

make_Incon <- function(freq, modelranfix) {
  incona <- netsplit(freq$net1)
  return(netsplitresult.df(incona, modelranfix))
}

# 3a Forest plot 
CreateForestPlot <- function(model, metaoutcome, bayesmin, bayesmax) {
  if (metaoutcome=="Binary") {
    return(gemtc::forest(model$mtcRelEffects, digits=3, xlim=c(log(bayesmin), log(bayesmax))))
  } else if (metaoutcome=="Continuous") {
    return(gemtc::forest(model$mtcRelEffects, digits=3, xlim=c(bayesmin, bayesmax)))
  }
}

# 3b Comparison of all treatment pairs
baye_comp <- function(model, outcome_measure){
  if (outcome_measure %in% c('OR', 'RR')) {
    return(as.data.frame(round(exp(model$rel_eff_tbl), digits = 2)))
  } else if (outcome_measure %in% c('RD', 'MD', 'SMD')) {
    return(as.data.frame(round(model$rel_eff_tbl, digits = 2)))
  } else {
    stop("outcome_measure has to be 'OR', 'RR', 'RD', 'MD', or 'SMD'.")
  }
}

# 3c Ranking Panel redesign by CRN

# Network plot - number of trials on line
make_netgraph_rank = function(freq, order) {  
  return(netmeta::netgraph(freq$net1, labels=str_wrap(gsub("_", " ",freq$net1$trts), width=10), lwd=2, number.of.studies = TRUE, plastic=FALSE, points=TRUE, cex=1, cex.points=2, col.points=1, col=8, pos.number.of.studies=0.43,
                           col.number.of.studies = "forestgreen", col.multiarm = "white", bg.number.of.studies = "forestgreen", seq=gsub(" ", "_", str_wrap(order, width=1000)),  #freq$net1$trts has not been formatted but 'order' has
  ))
}

# Litmus Rank-O-Gram #
LitmusRankOGram <- function(CumData, SUCRAData, ColourData, colourblind=FALSE, regression_text="") {    #CumData needs Treatment, Rank, Cumulative_Probability and SUCRA; SUCRAData needs Treatment & SUCRA; COlourData needs SUCRA & colour; colourblind friendly option; regression annotation text
  # Basic Rankogram #
  Rankogram <- ggplot(CumData, aes(x=Rank, y=Cumulative_Probability, group=Treatment)) +
    geom_line(aes(colour=SUCRA)) + theme_classic() + theme(legend.position = "none", aspect.ratio=1) +
    labs(x = "Rank", y = "Cumulative Probability") + scale_x_continuous(expand = c(0, 0), breaks = seq(1,nrow(SUCRAData)))
  if (colourblind==FALSE) {
    A <- Rankogram + scale_colour_gradient2(low = "red",
                                            mid = "yellow",
                                            high = "green", midpoint=50, limits=c(0,100))
  } else {
    A <- Rankogram + scale_colour_gradientn(colours=c("#7b3294","#c2a5cf","#a6dba0", "#008837"), values=c(0, 0.33, 0.66, 1), limits=c(0,100))
  }
  # Litmus SUCRA Scale #
  Litmus_SUCRA <- ggplot(SUCRAData, aes(x=rep(0.45,times=nrow(SUCRAData)), y=SUCRA)) +
    geom_segment(data = ColourData,
                 aes(x = -Inf, xend = 0.5,
                     y = SUCRA, yend = SUCRA, colour = colour),
                 show.legend = FALSE) +
    geom_point() + labs(y="SUCRA (%)") +
    ggrepel::geom_text_repel(aes(label=Treatment), box.padding = 0, direction="y", hjust=0, nudge_x=0.05, size=3) + scale_x_continuous(limits=c(0.4,0.8)) +
    theme_classic() + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank(), aspect.ratio=4)
  if (colourblind==FALSE) {
    B <- Litmus_SUCRA + scale_colour_gradient2(low = "red",
                                               mid = "yellow",
                                               high = "green", midpoint=50, limits=c(0,100))
  } else {
    B <- Litmus_SUCRA + scale_colour_gradientn(colours=c("#7b3294","#c2a5cf","#a6dba0", "#008837"), values=c(0, 0.33, 0.66, 1), limits=c(0,100))
  }
  # Combo! #
  if (regression_text != "") {
    Combo <- A + B + patchwork::plot_annotation(caption = regression_text)
  } else {
    Combo <- A + B    # '+' functionality from {patchwork}
  }
  Combo + theme(plot.margin = margin(t=0,r=0,b=0,l=0))
}

# Radial SUCRA Plot #
RadialSUCRA <- function(SUCRAData, ColourData, BUGSnetData, colourblind=FALSE, regression_text="") {      # SUCRAData needs Treatment & Rank; ColourData needs SUCRA & colour; colourblind friendly option; regression annotation text
  
  n <- nrow(SUCRAData) # number of treatments
  # Add values to angle and adjust radial treatment labels
  SUCRAData <- SUCRAData[order(-SUCRAData$SUCRA),]
  SUCRAData$Angle <- rev(90 + seq(180/n, 360-180/n, len=n)) - c(rep(360,ceiling(n/2)), rep(180,floor(n/2)))
  SUCRAData$Adjust <- c(rep(0,ceiling(n/2)),rep(1,floor(n/2)))
  # Background #
  Background <- ggplot(SUCRAData, aes(x=reorder(Treatment, -SUCRA), y=SUCRA, group=1)) +
    geom_segment(data = ColourData, aes(x = -Inf, xend = Inf, y = SUCRA, yend = SUCRA, colour = colour), show.legend = FALSE, alpha=0.05) +
    theme_classic() + 
    theme(panel.grid.major.y = element_line(colour = c(rep("black",6),"white")), axis.title = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), axis.line = element_blank(), 
          aspect.ratio = 1, axis.text.x = element_blank()) +
    coord_polar() +
    geom_text(aes(label=reorder(Treatment, -SUCRA), y=110, angle=Angle, hjust=Adjust),
              size=3, family="sans")
  if (colourblind==FALSE) {
    Background <- Background + scale_colour_gradient2(low = "red", mid = "yellow", high = "green", midpoint=50, limits=c(0,100)) +
      scale_fill_gradient2(low = "red", mid = "yellow", high = "green", midpoint=50, limits=c(0,100))
  } else {
    Background <- Background + scale_colour_gradientn(colours=c("#7b3294","#c2a5cf","#a6dba0", "#008837"), values=c(0, 0.33, 0.66, 1), limits=c(0,100)) +
      scale_fill_gradientn(colours=c("#7b3294","#c2a5cf","#a6dba0", "#008837"), values=c(0, 0.33, 0.66, 1), limits=c(0,100))
  }
  
  Background +
    geom_point(aes(fill=SUCRA),size=1, shape=21,show.legend=FALSE) +  
    scale_y_continuous(breaks=c(0,20,40,60,80,100), limits=c(-40,115)) +
    annotate("text",x = rep(0.5,7), y = c(-3,17,37,57,77,97,115), label = c("0","20","40","60","80","100","SUCRA (%)"), size=2.5, family="sans") # annotate has to be after geoms
  ggsave(filename = 'BackgroundO.png', device = 'png', bg = 'transparent', width = 5, height = 5)
  
  Background +
    geom_segment(aes(xend=Treatment, y = -20, yend=110), linetype="dashed") +
    geom_point(aes(fill=SUCRA),size=3, shape=21,show.legend=FALSE) +
    scale_y_continuous(breaks=c(0,20,40,60,80,100), limits=c(-80,115)) +
    annotate("text",x = rep(0.5,7), y = c(-3,17,37,57,77,97,115), label = c("0","20","40","60","80","100","SUCRA (%)"), size=2.5, family="sans") # annotate has to be after geoms
  ggsave(filename = 'BackgroundA.png', device = 'png', bg = 'transparent', width = 5, height = 5)
  
  
  # Create my own network plot using ggplot polar coords #
  SUCRA <- SUCRAData %>% dplyr::arrange(-SUCRA)
  edges <- network.structure(BUGSnetData, my_order = SUCRA$Treatment)  # from file 'network_structure.R'
  dat.edges <- data.frame(pairwiseID = rep(NA, nrow(edges)*2),
                          treatment = "",
                          n.stud = NA,
                          SUCRA = NA,
                          adj = NA,
                          col = "",
                          lwd = NA)
  lwd.maxO <- 4
  lwd.maxA <- 3
  lwd.minO <- 0.5
  lwd.minA <- 0.25
  lwd_rangeO <- lwd.maxO - lwd.minO
  lwd_rangeA <- lwd.maxA - lwd.minA
  study_min <- min(edges$edge.weight)
  study_range <- max(max(edges$edge.weight) - study_min, 1)
  comp.i <- 1
  ID <- 1
  for (i in 1:nrow(edges)) {
    dat.edges$pairwiseID[comp.i] <- ID
    dat.edges$pairwiseID[comp.i+1] <- ID
    dat.edges$treatment[comp.i] <- edges$from[i]
    dat.edges$treatment[comp.i+1] <- edges$to[i]
    dat.edges$n.stud[comp.i] <- edges$edge.weight[i]
    dat.edges$n.stud[comp.i+1] <- edges$edge.weight[i]
    dat.edges$SUCRA[comp.i] <- SUCRA$SUCRA[SUCRA$Treatment == edges$from[i]]
    dat.edges$SUCRA[comp.i+1] <- SUCRA$SUCRA[SUCRA$Treatment == edges$to[i]]
    dat.edges$lwdO[comp.i] <- lwd.minO + (edges$edge.weight[i] - study_min)*(lwd_rangeO/study_range)
    dat.edges$lwdA[comp.i] <- lwd.minA + (edges$edge.weight[i] - study_min)*(lwd_rangeA/study_range)
    dat.edges$lwdO[comp.i+1] <- lwd.minO + (edges$edge.weight[i] - study_min)*(lwd_rangeO/study_range)
    dat.edges$lwdA[comp.i+1] <- lwd.minA + (edges$edge.weight[i] - study_min)*(lwd_rangeA/study_range)
    comp.i <- comp.i + 2
    ID <- ID + 1
  }
  # add lines #
  CreateNetwork <- function(Type) {
    if (Type=='Original') {
      g <- ggplot(dat.edges, aes(x=reorder(treatment,-SUCRA), y=SUCRA, group=pairwiseID)) +
        geom_line(linewidth=dat.edges$lwdO,show.legend = FALSE) +
        scale_y_continuous(limits=c(-40,115))
    } else {
      g <- ggplot(dat.edges, aes(x=reorder(treatment,-SUCRA), y=-20, group=pairwiseID)) +
        geom_line(linewidth=dat.edges$lwdA,show.legend = FALSE) +
        scale_y_continuous(limits=c(-80,115))
    }
    g +
      ggiraphExtra::coord_radar() + 
      theme(panel.background = element_rect(fill = "transparent"), plot.background = element_rect(fill = "transparent", color = NA), 
            axis.title = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), 
            axis.line = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio = 1, 
            axis.text.x = element_blank()) +
      annotate("text",x = rep(0.5,7), y = c(-3,17,37,57,77,97,115), label = c("0","20","40","60","80","100","SUCRA (%)"), size=2.5, family="sans")
  }
  Network <- CreateNetwork(Type='Original')
  ggsave(filename = 'NetworkO.png', device = 'png', bg = 'transparent', width=5, height=5)
  
  Network <- CreateNetwork(Type='Alternative')
  ggsave(filename = "NetworkA.png", device = 'png', bg = 'transparent', width=5, height=5)
  
  
  # Plot of just points to go on the very top #
  CreatePoints <- function(Type, colourblind=FALSE) {
    if (Type=='Original') {
      g <- ggplot(SUCRAData, aes(x=reorder(Treatment, -SUCRA), y=SUCRA, group=1)) +
        geom_point(aes(fill=SUCRA, size=SizeO), size=SUCRAData$SizeO, shape=21,show.legend=FALSE) +
        scale_y_continuous(limits=c(-40,115))
    } else {
      g <- ggplot(SUCRAData, aes(x=reorder(Treatment, -SUCRA), y=-20, group=1)) +
        geom_point(aes(fill=SUCRA, size=SizeA), size=SUCRAData$SizeA, shape=21,show.legend=FALSE) +
        scale_y_continuous(limits=c(-80,115))
    }
    if (colourblind==FALSE) {
      g <- g + scale_fill_gradient2(low = "red", mid = "yellow", high = "green", midpoint=50, limits=c(0,100))
    } else {
      g <- g + scale_fill_gradientn(colours=c("#7b3294","#c2a5cf","#a6dba0", "#008837"), values=c(0, 0.33, 0.66, 1), limits=c(0,100))
    }
    g +
      theme(panel.background = element_rect(fill = "transparent"), plot.background = element_rect(fill = "transparent", color = NA), 
            axis.title = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), 
            axis.line = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio = 1,
            axis.text.x = element_blank()) +
      coord_polar() +
      annotate("text",x = rep(0.5,7), y = c(-3,17,37,57,77,97,115), label = c("0","20","40","60","80","100","SUCRA (%)"), size=2.5, family="sans")
  }
  
  Points <- CreatePoints(Type='Original', colourblind=colourblind)
  ggsave(filename = 'PointsO.png', device = 'png', bg = 'transparent', width=5, height=5)
  
  Points <- CreatePoints(Type='Alternative', colourblind=colourblind)
  ggsave(filename = 'PointsA.png', device = 'png', bg = 'transparent', width=5, height=5)
  
  # Overlay #
  Background <- magick::image_read('BackgroundO.png')
  Network <- magick::image_read('NetworkO.png')
  Points <- magick::image_read('PointsO.png')
  Final <- magick::image_composite(Background,Network)
  Final <- magick::image_composite(Final,Points)
  Finalplot <- cowplot::ggdraw() +
    cowplot::draw_image(Final)
  if (regression_text != "") {
    Finalplot <- Finalplot + 
      cowplot::draw_label(regression_text, x = 0.95, y = 0.05, hjust = 1, size = 10)
  }
  Background <- magick::image_read('BackgroundA.png')
  Network <- magick::image_read('NetworkA.png')
  Points <- magick::image_read('PointsA.png')
  Final <- magick::image_composite(Background,Network)
  Final <- magick::image_composite(Final,Points)
  Finalalt <- cowplot::ggdraw() +
    cowplot::draw_image(Final)
  if (regression_text != "") {
    Finalalt <- Finalalt + 
      cowplot::draw_label(regression_text, x = 0.95, y = 0.05, hjust = 1, size = 10)
  }
  
  file.remove('BackgroundO.png')
  file.remove('NetworkO.png')
  file.remove('PointsO.png')
  file.remove('BackgroundA.png')
  file.remove('NetworkA.png')
  file.remove('PointsA.png')
  
  return(list(Original=Finalplot, Alternative=Finalalt))
}

rank_probs_table = function(data) {
  Probs <- data$Probabilities %>% dplyr::right_join(data$SUCRA[,1:2], by="Treatment")
  Probs[order(-Probs$SUCRA),]
  return(Probs)
}

# 3f Deviance report

# UME scatter plot
scat_plot = function(model) {
  x <- mtc.deviance(model$mtcResults)
  c <- data.frame(x$dev.ab)
  umeplot.df(c, model$mtcNetwork, model$model, model$outcome)
}

# Stemplot
stemplot <- function(model) {
  x <- mtc.deviance(model$mtcResults)
  c <- data.frame(x$dev.ab)
  c$names <- rownames(c)
  return(stemplot.df(c, x))
}

# Leverage plot
levplot <- function(model) {
  x <- mtc.deviance(model$mtcResults)
  return(levplot.df(x))
}
