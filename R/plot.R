#####
# Plot functions used in both app and report - NVB
#####

#' 1a Summary table plot
#' 
#' @param bugsnetdt Long format data.
#' @param metaoutcome "Continuous" or "Binary".
#' @return Network created by BUGSnet::net.tab().
summary_table_plot <- function(bugsnetdt, metaoutcome) {
  return(bugsnet_sumtb(data = bugsnetdt, metaoutcome = metaoutcome))
}



#' 1b Forest plot
#' 
#' @param freq List of NMA results created by freq_wrap().
#' @param outcome_measure "MD", "SMD", "OR", "RR", or "RD".
#' @param ForestHeader Multiplier for size of text in treatment contrast headers.
#' @param ForestTitle Multiplier for size of title.
#' @return Plot created by groupforest.df().
make_netStudy <- function(freq, outcome_measure, ForestHeader, ForestTitle) {
  return(groupforest.df(d1 = freq$d0, ntx = freq$ntx, lstx = freq$lstx, outcome = outcome_measure,
                        HeaderSize = ForestHeader, TitleSize = ForestTitle
                        )
         )
}



#' 1c Network plot - number of trials on line
#'
#' @param freq List of NMA results created by freq_wrap().
#' @param label_size Label size multiplier.
#' @return Network plot created by netmeta::netgraph().
make_netgraph <- function(freq, label_size) {  
  return(netmeta::netgraph(freq$net1, lwd = 2, number.of.studies = TRUE, plastic = FALSE, points = TRUE,
                           cex = label_size, cex.points = 2, col.points = 1, col = 8, pos.number.of.studies = 0.43,
                           col.number.of.studies = "forestgreen", col.multiarm = "white",
                           bg.number.of.studies = "forestgreen"
                           )
         )
}



#' 1c Network plot - number of trials by nodesize and line thickness
#' 
#' @param bugsnetdt Long format data.
#' @param label_size Node label size (default = 1).
#' @param order Order of nodes (default = NULL).
#' @return Network plot created by BUGSnet::net.plot().
make_netplot <- function(bugsnetdt, label_size = 1, order = NULL) {    # added default values and extra option for ordering the nodes (CRN)  
  data.rh <- BUGSnet::data.prep(arm.data = bugsnetdt, varname.t = "T", varname.s = "Study")
  return(BUGSnet::net.plot(data.rh, node.scale = 3, edge.scale = 1.5, node.lab.cex = label_size,
                           layout.params = order
                           )
         )
}



#' 1c Creates network connectivity info displayed under network plots 
#' 
#' @param freq List of NMA results created by freq_wrap().
#' @return Network connectivity created by netmeta::netconnection().
make_netconnect <- function(freq) {   
  d1 <- freq$d1
  nc1 <- netmeta::netconnection(treat1 = d1$treat1, treat2 = d1$treat2, studLab = d1$studlab, data = NULL)
  print(nc1)
}



#' 2a. Frequentist forest Plot
#' 
#' @param freq List of NMA results created by freq_wrap().
#' @param modelranfix "fixed" or "random".
#' @param ref Reference treatment.
#' @param min Minimum x-axis limit.
#' @param max Maximum x-axis limit.
#' @return Forest plot created by forest.df().
make_netComp <- function(freq, modelranfix, ref, min, max) {    
  return(forest.df(netresult = freq$net1, model = modelranfix, lstx = freq$lstx, ref = ref,
                   min = min, max = max
                   )
         )
}



#' 2a. Creates text displayed under frequentist forest plots
#' 
#' @param freq List of NMA results created by freq_wrap().
#' @param outcome_measure "MD", "SMD", "OR", "RR", or "RD".
#' @param modelranfix "fixed" or "random".
#' @return Text created by tau.df().
texttau <- function(freq, outcome_measure, modelranfix){      
  tau <- round(freq$net1$tau, 2)
  return(tau.df(tau = tau, k = freq$net1$k, n = freq$net1$n, model = modelranfix, outcome = outcome_measure))
}



#' Create text to say outcomes are compared to the reference treatment.
#'
#' @param ref Reference treatment.
#' @return Text as above.
make_refText = function(ref) {
  return(paste0("All outcomes are versus the reference treatment: ", ref))
}



#' 2b Treatment comparison and rank table
#' 
#' @param freq List of NMA results created by freq_wrap().
#' @param modelranfix "fixed" or "random".
#' @param rankopts "good" or "bad", referring to smaller outcome values.
#' @return Ranking table created by netmeta::netleague().
make_netrank <- function(freq, modelranfix, rankopts) {
  league <- netmeta::netleague(freq$net1, comb.random = (modelranfix == "random"),
                               comb.fixed = (modelranfix == "fixed"), digits = 2,
                               seq = netmeta::netrank(freq$net1, small = rankopts)
                               )
  if (modelranfix == "random") {
    leaguedf <- as.data.frame(league$random)
  } else if (modelranfix == "fixed") {
    leaguedf <- as.data.frame(league$fixed)
  } else {
    stop("modelranfix must be 'fixed' or 'random'") 
  }
  return(leaguedf)
}



#' 2c Creates a data frame of inconsistency data obtained from netmeta::netsplit().
#'
#' @param freq List of NMA results created by freq_wrap().
#' @param modelranfix "fixed" or "random".
#' @return Data frame of inconsistency data created by netsplitresult.df().
make_Incon <- function(freq, modelranfix) {
  incona <- netmeta::netsplit(freq$net1)
  return(netsplitresult.df(incona, modelranfix))
}



#' 3a Bayesian forest plot 
#' 
#' @param model Various model output created by baye().
#' @param metaoutcome "Continuous" or "Binary".
#' @param bayesmin x-axis limit minimum.
#' @param bayesmax x-axis limit maximum.
#' @return Forest plot created by gemtc::forest().
CreateForestPlot <- function(model, metaoutcome, bayesmin, bayesmax) {
  if (metaoutcome == "Binary") {
    return(gemtc::forest(model$mtcRelEffects, digits = 3, xlim = c(log(bayesmin), log(bayesmax))))
  } else if (metaoutcome == "Continuous") {
    return(gemtc::forest(model$mtcRelEffects, digits = 3, xlim = c(bayesmin, bayesmax)))
  } else {
    stop("metaoutcome must be 'Continuous' or 'Binary'") 
  }
}



#' 3b Creates a table of comparisons of all treatment pairs
#' 
#' @param model Various model output created by baye().
#' @param metaoutcome "Continuous" or "Binary".
#' @param outcome_measure "MD", "SMD", "OR", "RR", or "RD".
#' @return Relative effects table created by gemtc::relative.effect.table().
baye_comp <- function(model, outcome_measure){
  if (outcome_measure %in% c('OR', 'RR')) {
    return(as.data.frame(round(exp(model$rel_eff_tbl), digits = 2)))
  } else if (outcome_measure %in% c('RD', 'MD', 'SMD')) {
    return(as.data.frame(round(model$rel_eff_tbl, digits = 2)))
  } else {
    stop("outcome_measure has to be 'OR', 'RR', 'RD', 'MD', or 'SMD'.")
  }
}



#' 3c Create network plot for the ranking panel
#' 
#' @param freq List of NMA results created by freq_wrap().
#' @param order Vector of treatments in SUCRA order.
#' @return Network plot created by netmeta::netgraph() with each line containing the number of trials.
make_netgraph_rank = function(freq, order) {  
  return(netmeta::netgraph(freq$net1, labels = str_wrap(gsub("_", " ", freq$net1$trts), width = 10),
                           lwd=2, number.of.studies = TRUE, plastic = FALSE, points = TRUE, cex = 1,
                           cex.points = 2, col.points = 1, col = 8, pos.number.of.studies = 0.43,
                           col.number.of.studies = "forestgreen", col.multiarm = "white",
                           bg.number.of.studies = "forestgreen",
                           seq = gsub(" ", "_", str_wrap(order, width = 1000))
                           )#freq$net1$trts has not been formatted but 'order' has
         )
  
}



#' Litmus Rank-O-Gram
#' 
#' @param CumData Cumulative ranking probabilities, created by rankdata().
#' @param SUCRAData SUCRA data, created by rankdata().
#' @param ColourData Colour data, created by rankdata().
#' @param colourblind TRUE for colourblind friendly colours (default = FALSE).
#' @param regression_text Text to show for regression (default = "").
#' @return Litmus rank-o-gram.
LitmusRankOGram <- function(CumData, SUCRAData, ColourData, colourblind=FALSE, regression_text="") {    #CumData needs Treatment, Rank, Cumulative_Probability and SUCRA; SUCRAData needs Treatment & SUCRA; COlourData needs SUCRA & colour; colourblind friendly option; regression annotation text
  # Basic Rankogram #
  Rankogram <- ggplot(CumData, aes(x = Rank, y = Cumulative_Probability, group = Treatment)) +
    geom_line(aes(colour = SUCRA)) + theme_classic() + theme(legend.position = "none", aspect.ratio = 1) +
    labs(x = "Rank", y = "Cumulative Probability") + scale_x_continuous(expand = c(0, 0), breaks = seq(1, nrow(SUCRAData)))
  if (colourblind == FALSE) {
    A <- Rankogram + scale_colour_gradient2(low = "red",
                                            mid = "yellow",
                                            high = "green", midpoint = 50, limits = c(0,100))
  } else {
    A <- Rankogram + scale_colour_gradientn(colours = c("#7b3294","#c2a5cf","#a6dba0", "#008837"), values = c(0, 0.33, 0.66, 1), limits = c(0,100))
  }
  # Litmus SUCRA Scale #
  Litmus_SUCRA <- ggplot(SUCRAData, aes(x = rep(0.45, times = nrow(SUCRAData)), y = SUCRA)) +
    geom_segment(data = ColourData,
                 aes(x = -Inf, xend = 0.5,
                     y = SUCRA, yend = SUCRA, colour = colour),
                 show.legend = FALSE) +
    geom_point() + labs(y = "SUCRA (%)") +
    ggrepel::geom_text_repel(aes(label = str_wrap(gsub("_", " ", Treatment), width = 10)), box.padding = 0, direction="y", hjust = 0, nudge_x = 0.05, size = 3) + scale_x_continuous(limits = c(0.4,0.8)) +
    theme_classic() + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank(), aspect.ratio = 4)
  if (colourblind == FALSE) {
    B <- Litmus_SUCRA + scale_colour_gradient2(low = "red",
                                               mid = "yellow",
                                               high = "green", midpoint = 50, limits = c(0,100))
  } else {
    B <- Litmus_SUCRA + scale_colour_gradientn(colours = c("#7b3294", "#c2a5cf", "#a6dba0", "#008837"),
                                               values = c(0, 0.33, 0.66, 1), limits = c(0,100))
  }
  # Combo! #
  if (regression_text != "") {
    Combo <- A + B + patchwork::plot_annotation(caption = regression_text)
  } else {
    Combo <- A + B    # '+' functionality from {patchwork}
  }
  Combo + theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0))
}



#' Radial SUCRA Plot
#' 
#' @param SUCRAData SUCRA data, created by rankdata().
#' @param ColourData Colour data, created by rankdata().
#' @param BUGSnetData Output created by BUGSnet functions in rankdata().
#' @param colourblind TRUE for colourblind friendly colours (default = FALSE).
#' @param regression_text Text to show for regression (default = "").
#' @return Radial SUCRA plot.
RadialSUCRA <- function(SUCRAData, ColourData, BUGSnetData, colourblind=FALSE, regression_text="") {      # SUCRAData needs Treatment & Rank; ColourData needs SUCRA & colour; colourblind friendly option; regression annotation text
  # Tempory Directory
  temp_dir <- tempdir()
  n <- nrow(SUCRAData) # number of treatments
  # Add values to angle and adjust radial treatment labels
  SUCRAData <- SUCRAData[order(-SUCRAData$SUCRA), ]
  SUCRAData$Angle <- rev(90 + seq(180 / n, 360 - 180 / n, len = n)) - c(rep(360, ceiling(n / 2)), rep(180,floor(n / 2)))
  SUCRAData$Adjust <- c(rep(0, ceiling(n / 2)), rep(1, floor(n / 2)))
  # Background #
  Background <- ggplot(SUCRAData, aes(x = reorder(Treatment, -SUCRA), y = SUCRA, group = 1)) +
    geom_segment(data = ColourData, aes(x = -Inf, xend = Inf, y = SUCRA, yend = SUCRA, colour = colour), show.legend = FALSE, alpha=0.05) +
    theme_classic() + 
    theme(panel.grid.major.y = element_line(colour = c(rep("black", 6), "white")), axis.title = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), axis.line = element_blank(), 
          aspect.ratio = 1, axis.text.x = element_blank()) +
    coord_polar() +
    geom_text(aes(label=reorder(str_wrap(gsub("_", " ", Treatment), width = 10), -SUCRA), y = 110, angle = Angle, hjust = Adjust),
              size = 3, family = "sans")
  if (colourblind == FALSE) {
    Background <- Background + scale_colour_gradient2(low = "red", mid = "yellow", high = "green", midpoint=50, limits=c(0,100)) +
      scale_fill_gradient2(low = "red", mid = "yellow", high = "green", midpoint = 50, limits = c(0, 100))
  } else {
    Background <- Background + scale_colour_gradientn(colours = c("#7b3294", "#c2a5cf", "#a6dba0", "#008837"), values = c(0, 0.33, 0.66, 1), limits = c(0, 100)) +
      scale_fill_gradientn(colours = c("#7b3294", "#c2a5cf", "#a6dba0", "#008837"), values = c(0, 0.33, 0.66, 1), limits = c(0, 100))
  }
  
  Background +
    geom_point(aes(fill = SUCRA), size = 1, shape = 21, show.legend = FALSE) +  
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100), limits = c(-40, 115)) +
    annotate("text", x = rep(0.5, 7), y = c(-3, 17, 37, 57, 77, 97, 115), label = c("0", "20", "40", "60", "80", "100", "SUCRA (%)"), size = 2.5, family = "sans") # annotate has to be after geoms
  ggsave(filename = file.path(temp_dir, 'BackgroundO.png'), device = 'png', bg = 'transparent', width = 5, height = 5)
  
  Background +
    geom_segment(aes(xend = Treatment, y = -20, yend = 110), linetype = "dashed") +
    geom_point(aes(fill = SUCRA), size = 3, shape = 21, show.legend = FALSE) +
    scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100), limits = c(-80, 115)) +
    annotate("text", x = rep(0.5, 7), y = c(-3, 17, 37, 57, 77, 97, 115), label = c("0", "20", "40", "60", "80", "100", "SUCRA (%)"), size = 2.5, family = "sans") # annotate has to be after geoms
  ggsave(filename = file.path(temp_dir, 'BackgroundA.png'), device = 'png', bg = 'transparent', width = 5, height = 5)
  
  
  # Create my own network plot using ggplot polar coords #
  SUCRA <- SUCRAData %>% dplyr::arrange(-SUCRA)
  edges <- network.structure(BUGSnetData, my_order = SUCRA$Treatment)  # from file 'network_structure.R'
  dat.edges <- data.frame(pairwiseID = rep(NA, nrow(edges) * 2),
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
    dat.edges$lwdO[comp.i] <- lwd.minO + (edges$edge.weight[i] - study_min) * (lwd_rangeO / study_range)
    dat.edges$lwdA[comp.i] <- lwd.minA + (edges$edge.weight[i] - study_min) * (lwd_rangeA / study_range)
    dat.edges$lwdO[comp.i+1] <- lwd.minO + (edges$edge.weight[i] - study_min) * (lwd_rangeO / study_range)
    dat.edges$lwdA[comp.i+1] <- lwd.minA + (edges$edge.weight[i] - study_min) * (lwd_rangeA / study_range)
    comp.i <- comp.i + 2
    ID <- ID + 1
  }
  
  #' Creates the network part of the radical SUCRA plot, excluding the nodes.
  #' 
  #' @param Type "Original" or "Alternative.
  #' @return ggplot object.
  CreateNetwork <- function(Type) {
    if (Type == 'Original') {
      g <- ggplot(dat.edges, aes(x = reorder(treatment, -SUCRA), y = SUCRA, group = pairwiseID)) +
        geom_line(linewidth = dat.edges$lwdO, show.legend = FALSE) +
        scale_y_continuous(limits = c(-40, 115))
    } else {
      g <- ggplot(dat.edges, aes(x = reorder(treatment, -SUCRA), y = -20, group = pairwiseID)) +
        geom_line(linewidth = dat.edges$lwdA, show.legend = FALSE) +
        scale_y_continuous(limits = c(-80, 115))
    }
    g +
      ggiraphExtra::coord_radar() + 
      theme(panel.background = element_rect(fill = "transparent"), plot.background = element_rect(fill = "transparent", color = NA), 
            axis.title = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), 
            axis.line = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio = 1, 
            axis.text.x = element_blank()) +
      annotate("text", x = rep(0.5, 7), y = c(-3, 17, 37, 57, 77, 97, 115), label = c("0", "20", "40", "60", "80", "100", "SUCRA (%)"), size = 2.5, family = "sans")
  }
  
  Network <- CreateNetwork(Type = 'Original')
  ggsave(filename = file.path(temp_dir, 'NetworkO.png'), device = 'png', bg = 'transparent', width = 5, height = 5)
  
  Network <- CreateNetwork(Type = 'Alternative')
  ggsave(filename = file.path(temp_dir, "NetworkA.png"), device = 'png', bg = 'transparent', width = 5, height = 5)
  
  
  #' Creates the nodes for the network part of the radial SUCRA plot.
  #' 
  #' @param Type "Original" or "Alternative".
  #' @param colourblind TRUE for colourblind-friendly colours (default = FALSE).
  #' @return ggplot object.
  CreatePoints <- function(Type, colourblind = FALSE) {
    if (Type == 'Original') {
      g <- ggplot(SUCRAData, aes(x = reorder(Treatment, -SUCRA), y = SUCRA, group = 1)) +
        geom_point(aes(fill = SUCRA, size = SizeO), size = SUCRAData$SizeO, shape = 21, show.legend = FALSE) +
        scale_y_continuous(limits = c(-40, 115))
    } else {
      g <- ggplot(SUCRAData, aes(x = reorder(Treatment, -SUCRA), y = -20, group = 1)) +
        geom_point(aes(fill = SUCRA, size = SizeA), size = SUCRAData$SizeA, shape = 21, show.legend = FALSE) +
        scale_y_continuous(limits = c(-80, 115))
    }
    if (colourblind == FALSE) {
      g <- g + scale_fill_gradient2(low = "red", mid = "yellow", high = "green", midpoint = 50, limits = c(0, 100))
    } else {
      g <- g + scale_fill_gradientn(colours = c("#7b3294", "#c2a5cf", "#a6dba0", "#008837"), values = c(0, 0.33, 0.66, 1), limits = c(0, 100))
    }
    g +
      theme(panel.background = element_rect(fill = "transparent"), plot.background = element_rect(fill = "transparent", color = NA), 
            axis.title = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), 
            axis.line = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio = 1,
            axis.text.x = element_blank()) +
      coord_polar() +
      annotate("text", x = rep(0.5, 7), y = c(-3, 17, 37, 57, 77, 97, 115), label = c("0", "20", "40", "60", "80", "100", "SUCRA (%)"), size = 2.5, family = "sans")
  }
  
  Points <- CreatePoints(Type = 'Original', colourblind = colourblind)
  ggsave(filename = file.path(temp_dir, 'PointsO.png'), device = 'png', bg = 'transparent', width = 5, height = 5)
  
  Points <- CreatePoints(Type = 'Alternative', colourblind = colourblind)
  ggsave(filename = file.path(temp_dir, 'PointsA.png'), device = 'png', bg = 'transparent', width = 5, height = 5)
  
  # Overlay #
  Background <- magick::image_read(file.path(temp_dir, 'BackgroundO.png'))
  Network <- magick::image_read(file.path(temp_dir, 'NetworkO.png'))
  Points <- magick::image_read(file.path(temp_dir, 'PointsO.png'))
  Final <- magick::image_composite(Background,Network)
  Final <- magick::image_composite(Final,Points)
  Finalplot <- cowplot::ggdraw() +
    cowplot::draw_image(Final)
  if (regression_text != "") {
    Finalplot <- Finalplot + 
      cowplot::draw_label(regression_text, x = 0.95, y = 0.05, hjust = 1, size = 10)
  }
  Background <- magick::image_read(file.path(temp_dir, 'BackgroundA.png'))
  Network <- magick::image_read(file.path(temp_dir, 'NetworkA.png'))
  Points <- magick::image_read(file.path(temp_dir, 'PointsA.png'))
  Final <- magick::image_composite(Background,Network)
  Final <- magick::image_composite(Final,Points)
  Finalalt <- cowplot::ggdraw() +
    cowplot::draw_image(Final)
  if (regression_text != "") {
    Finalalt <- Finalalt + 
      cowplot::draw_label(regression_text, x = 0.95, y = 0.05, hjust = 1, size = 10)
  }
  
  unlink(file.path(temp_dir, 'BackgroundO.png'))
  unlink(file.path(temp_dir, 'NetworkO.png'))
  unlink(file.path(temp_dir, 'PointsO.png'))
  unlink(file.path(temp_dir, 'BackgroundA.png'))
  unlink(file.path(temp_dir, 'NetworkA.png'))
  unlink(file.path(temp_dir, 'PointsA.png'))
  
  return(list(Original = Finalplot, Alternative = Finalalt))
}



#' Combine probabilities and SUCRA from the list created by rankdata().
#'
#' @param data List of ranking data created by rankdata().
#' @return Data frame of probabilities and SUCRA.
rank_probs_table = function(data) {
  Probs <- data$Probabilities %>% dplyr::right_join(data$SUCRA[,1:2], by="Treatment")
  Probs[order(-Probs$SUCRA),]
  return(Probs)
}



# 3f Deviance report

#' UME scatter plot
#' 
#' @param model Various model output created by baye().
#' @return UME scatter plot created by umeplot.df().
scat_plot = function(model) {
  x <- gemtc::mtc.deviance(model$mtcResults)
  residual_deviance <- data.frame(x$dev.ab)
  umeplot.df(residual_deviance = residual_deviance,
             mtcNetwork = model$mtcNetwork,
             model = model$model,
             outcome = model$outcome)
}



#' Stemplot
#' 
#' @param model Various model output created by baye().
#' @return Stemplot created by stemplot.df().
stemplot <- function(model, package = "gemtc") {
  if (package == "gemtc") {
    x <- mtc.deviance(model$mtcResults)
    c <- data.frame(x$dev.ab)
    c$names <- rownames(c)
  } else if (package == "bnma") {
    #In gemtc the only element of mtc.deviance(model$mtcResults) that is used in the plot is $dev.ab, so this is the only thing that needs to be passed to x.
    x <- list(dev.ab = model$deviance$dev_arm)
    c <- data.frame(x$dev.ab)
    #The arm-level deviances in bnma are not named, so the study names cannot come from rownames(c) as they do in gemtc.
    c$names <- unname(model$network$Study.order)
  } else {
    stop("package must be 'gemtc' or 'bnma'")
  }
  return(stemplot.df(c, x))
}



#' Leverage plot
#' 
#' @param model Various model output created by baye().
#' @return Leverage plot created by levplot.df().
levplot <- function(model, package = "gemtc") {
  if (package == "gemtc") {
    x <- mtc.deviance(model$mtcResults)
  } else if (package == "bnma") {
    #These are the only elements of mtc.deviance(model$mtcResults) that are used in the plot, so these are the only things that needs to be passed to x.
    x <- list(dev.ab = model$deviance$dev_arm,
              fit.ab = model$deviance$devtilda_arm,
              dev.re = NULL,
              fit.re = NULL,
              nd.ab = model$network$na,
              nd.re = NULL
              )
  } else {
    stop("package must be 'gemtc' or 'bnma'")
  }
  return(levplot.df(x))
}


#' Creates a Gelman plot for a gemtc or bnma model.
#' 
#' @param gelman_plot Output from coda::gelman.plot(model$samples[, parm]), where parm is a parameter from the model.
#' @param parameter The parameter from the previous argument, used as the title.
#' @return Reproduces the Gelman plot mentioned in @param gelman_plot as a plot that can be put in a grid.
GelmanPlot <- function(gelman_plot, parameter){
  y_vals_median <- gelman_plot$shrink[, , "median"]
  y_vals_975 <- gelman_plot$shrink[, , "97.5%"]
  x_vals <- gelman_plot$last.iter
  
  plot(x_vals, y_vals_975, type = "l", col = "red", lty = 2, ylab = "shrink factor",
       xlab = "last iteration in chain", cex.lab = 1.5, cex.main = 1.5, main = parameter)
  lines(x_vals, y_vals_median, type = "l")
  lines(c(-max(x_vals)/5, max(x_vals)), c(1, 1))
  legend("topright", legend = c("median", "97.5%"), lty = c(1, 2), col = c("black", "red"))
}


#' Creates Gelman plots for a gemtc or bnma model.
#' 
#' @param gelman_plots List of outputs from coda::gelman.plot(model$samples[, parm]), where parm is a parameter from bnma_model.
#' @param parameters Vector of parameters mentioned in the previous argument.
#' @return Plots the Gelman plots mentioned in @param gelman_plots.
GelmanPlots <- function(gelman_plots, parameters){
  for (i in 1:length(parameters)) {
    GelmanPlot(gelman_plot = gelman_plots[[i]], parameter = parameters[i])
  }
}
