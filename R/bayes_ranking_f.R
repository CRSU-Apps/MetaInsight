#' Generate ranking data required to produce SUCRA plots from Bayesian models
#'
#' @param model list. Output produced by `baseline_model()`, `bayes_model()` or `covariate_model()`.
#' @param cov_value numeric. Covariate value if a meta-regression. Default `NA`
#' @inheritParams common_params
#'
#' @return List of output created by `rankdata()`.
#' @export
bayes_ranking <- function(connected_data, treatment_df, model, ranking_option, cov_value = NA, logger = NULL) {

  check_param_classes(c("connected_data", "treatment_df", "ranking_option"),
                      c("data.frame", "data.frame", "character"), logger)

  if (!ranking_option %in% c("good", "bad")){
    logger |> writeLog(type = "error", "ranking_option must be either good or bad")
    return()
  }

  if (!inherits(model, "bayes_model") && !inherits(model, "baseline_model")){
    logger |> writeLog(type = "error", "model must be an object created by baseline_model(), bayes_model() or covariate_model()")
    return()
  }

  if (!inherits(model, "covariate_model") && !is.na(cov_value)){
    logger |> writeLog(type = "error", "cov_value can only be provided for covariate models")
    return()
  }

  if (inherits(model, "covariate_model") && (is.na(cov_value) || !is.numeric(cov_value))){
    logger |> writeLog(type = "error", "please specify a numeric cov_value")
    return()
  }

  longsort <- dataform.df(connected_data, treatment_df, model$outcome)

  rankdata(
    NMAdata = model$mtcResults,
    rankdirection = ranking_option,
    longdata = longsort,
    cov_value = cov_value,
    package = ifelse(inherits(model, "baseline_model"), "bnma", "gemtc")
  )
}

#' @rdname bayes_ranking
#' @param ... Parameters passed to `bayes_ranking()`
#' @export
baseline_ranking <- function(...){
  bayes_ranking(...)
}

#' @rdname bayes_ranking
#' @param ... Parameters passed to `bayes_ranking()`
#' @export
covariate_ranking <- function(...){
  bayes_ranking(...)
}

#' Get SUCRA data.
#'
#' @param NMAdata Output from 'baye' function or from bnma::network.run.
#' @param rankdirection "good" or "bad" (referring to smaller outcome values).
#' @param longdata Output from 'dataform.df' function. This should be the same dataset that was passed as the 'data' argument to baye(), which resulted in @param NMAdata.
#'        (TM: Suggested improvement: baye() should output its 'data' argument, then @param longdata becomes superfluous, and there is no possibility of a mismatch between @param NMAdata and @param longdata.)
#' @param cov_value covariate value if a meta-regression
#' @param package "gemtc" or "bnma", defaults to "gemtc".
#' @return List:
#' - 'SUCRA' = Data frame of SUCRA data.
#'     - 'Treatment'
#'     - 'SUCRA' = Sucra percentages.
#'     - 'N' = Total number of patients in 'Treatment' arms (summed over all studies).
#'     - 'SizeO' = Size of points (relative to 'N') in original SUCRA plot.
#'     - 'SizeA' = Size of points (relative to 'N') in alternative SUCRA plot.
#' - 'Colour' = Data frame of colours.
#'     - 'SUCRA' = Possible SUCRA values.
#'     - 'colour' = Colour values corresponding to 'SUCRA'.
#' - 'Cumulative' = Data frame of cumulative ranking probabilities, in long format.
#'     - 'Treatment'
#'     - 'Rank'
#'     - 'Cumulative_Probability'
#'     - 'SUCRA'
#' - 'Probabilities' = Data frame of ranking probabilities.
#'     - 'Treatment'
#'     - 'Rank 1' = Probability 'Treatment' is ranked first.
#'     - ...
#'     - 'Rank n_t' = Probability 'Treatment' is ranked last (n_t = number of treatments).
#' - 'BUGSnetData' = Output from BUGSnet::data.prep with arguments from @param longdata.
rankdata <- function(NMAdata, rankdirection, longdata, cov_value = NA, package = "gemtc") {
  # data frame of colours
  colour_dat = data.frame(SUCRA = seq(0, 100, by = 0.1))
  colour_dat = dplyr::mutate(colour_dat, colour = seq(0, 100, length.out = 1001))

  direction <- ifelse(rankdirection == "good", -1, 1)
  # probability rankings
  if (package == "gemtc"){
    prob <- as.data.frame(
      unclass( # required to convert
        gemtc::rank.probability(
          NMAdata,
          preferredDirection = direction,
          covariate = cov_value
        )
      )
    ) # rows treatments, columns ranks
  } else if (package == "bnma"){
    if (rankdirection == "good"){
      prob <- as.data.frame(t(BnmaSwitchRanking(NMAdata$rank.tx)))
    } else{
      prob <- as.data.frame(t(NMAdata$rank.tx))
    }
    #Remove "treatment " from the start of the treatment names
    rownames(prob) <- substr(rownames(prob), start = 11, stop = nchar(rownames(prob)))
  } else{
    stop("package must be 'gemtc' or 'bnma'")
  }
  names(prob)[1:ncol(prob)] <- paste("Rank ", 1:(ncol(prob)), sep="")
  sucra <- gemtc::sucra(prob)  # 1 row of SUCRA values for each treatment column
  treatments <- row.names(prob)

  # SUCRA
  SUCRA <- data.frame(
    Treatment = treatments,
    SUCRA = as.numeric(sucra) * 100
  )

  # Cumulative Probabilities
  cumprob <- prob              # obtain copy of probabilities
  for (i in 2:ncol(prob)) {    # for each rank (column)
    for (j in 1:ncol(prob)) {  # for each treatment (row)
      cumprob[j, i] <- cumprob[j, i-1] + cumprob[j, i]
    }
  }
  Cumulative_Data <- data.frame(
    Treatment = rep(treatments, each = ncol(prob)),
    Rank = rep(1:ncol(prob), times = ncol(prob)),
    Cumulative_Probability = as.numeric(t(cumprob))
  )
  Cumulative_Data <- Cumulative_Data |> dplyr::left_join(SUCRA, by = "Treatment")
  # Number of people in each node #
  Patients <- data.frame(
    Treatment = longdata$T,
    Sample = longdata$N
  )
  Patients <- aggregate(
    Patients$Sample,
    by = list(Category = Patients$Treatment),
    FUN = sum
  )
  Patients <- dplyr::rename(Patients, c(Treatment = "Category", N = "x"))  # previously using plyr::rename where old/new names are other way round
  SUCRA <- SUCRA |> dplyr::right_join(Patients, by = "Treatment")

  # Node size #
  size.maxO <- 15
  size.maxA <- 10
  size.min <- 1
  n <- ncol(prob)
  for (i in 1:n) {
    SUCRA$SizeO[i] <- size.maxO * SUCRA$N[i] / max(SUCRA$N)
    SUCRA$SizeA[i] <- size.maxA * SUCRA$N[i] / max(SUCRA$N)
    if (SUCRA$SizeO[i] < size.min) {
      SUCRA$SizeO[i] <- size.min
    }
    if (SUCRA$SizeA[i] < size.min) {
      SUCRA$SizeA[i] <- size.min
    }
  }

  prob <- data.table::setDT(prob, keep.rownames = "Treatment") # treatment as a column rather than rownames (useful for exporting)
  prob$Treatment <- prob$Treatment

  # Number of trials as line thickness taken from BUDGnetData object #
  BUGSnetData <- BUGSnet::data.prep(arm.data = longdata, varname.t = "T", varname.s = "Study")
  return(
    list(
      SUCRA = SUCRA,
      Colour = colour_dat,
      Cumulative = Cumulative_Data,
      Probabilities = prob,
      BUGSnetData = BUGSnetData
    )
  )
}

#' Litmus Rank-O-Gram
#'
#' @param ranking_data list created by bayes_ranking().
#' @param colourblind TRUE for colourblind friendly colours (default = FALSE).
#' @param regression_text Text to show for regression (default = "").
#' @return Litmus rank-o-gram.
#' @import patchwork
#' @import ggplot2
#' @export
LitmusRankOGram <- function(ranking_data, colourblind=FALSE, regression_text="") {    #CumData needs Treatment, Rank, Cumulative_Probability and SUCRA; SUCRAData needs Treatment & SUCRA; COlourData needs SUCRA & colour; colourblind friendly option; regression annotation text
  # Basic Rankogram #
  Rankogram <- ggplot(ranking_data$Cumulative, aes(x = Rank, y = Cumulative_Probability, group = Treatment)) +
    geom_line(aes(colour = SUCRA)) + theme_classic() + theme(legend.position = "none", aspect.ratio = 1) +
    labs(x = "Rank", y = "Cumulative Probability") + scale_x_continuous(expand = c(0, 0), breaks = seq(1, nrow(ranking_data$SUCRA)))
  if (colourblind == FALSE) {
    A <- Rankogram + scale_colour_gradient2(low = "red",
                                                     mid = "yellow",
                                                     high = "green", midpoint = 50, limits = c(0,100))
  } else {
    A <- Rankogram + scale_colour_gradientn(colours = c("#7b3294","#c2a5cf","#a6dba0", "#008837"),
                                                     values = c(0, 0.33, 0.66, 1), limits = c(0,100))
  }
  # Litmus SUCRA Scale #
  Litmus_SUCRA <- ggplot(ranking_data$SUCRA, aes(x = rep(0.45, times = nrow(ranking_data$SUCRA)), y = SUCRA)) +
    geom_segment(data = ranking_data$Colour,
                          aes(x = -Inf, xend = 0.5,
                          y = SUCRA, yend = SUCRA, colour = colour),
                          show.legend = FALSE) +
    geom_point() + labs(y = "SUCRA (%)") +
    ggrepel::geom_text_repel(aes(label = stringr::str_wrap(gsub("_", " ", Treatment), width = 10)), box.padding = 0, direction="y", hjust = 0, nudge_x = 0.05, size = 3) + scale_x_continuous(limits = c(0.4,0.8)) +
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
#' @param ranking_data list created by bayes_ranking().
#' @param colourblind TRUE for colourblind friendly colours (default = FALSE).
#' @param regression_text Text to show for regression (default = "").
#' @return Radial SUCRA plot.
#' @import ggplot2
#' @export
RadialSUCRA <- function(ranking_data, colourblind=FALSE, regression_text="") {      # SUCRAData needs Treatment & Rank; ColourData needs SUCRA & colour; colourblind friendly option; regression annotation text
  # Tempory Directory
  temp_dir <- tempdir()
  n <- nrow(ranking_data$SUCRA) # number of treatments
  # Add values to angle and adjust radial treatment labels
  SUCRAData <- ranking_data$SUCRA[order(-ranking_data$SUCRA$SUCRA), ]
  SUCRAData$Angle <- rev(90 + seq(180 / n, 360 - 180 / n, len = n)) - c(rep(360, ceiling(n / 2)), rep(180,floor(n / 2)))
  SUCRAData$Adjust <- c(rep(0, ceiling(n / 2)), rep(1, floor(n / 2)))
  # Background #
  Background <- ggplot(SUCRAData, aes(x = reorder(Treatment, -SUCRA), y = SUCRA, group = 1)) +
    geom_segment(data = ranking_data$Colour, aes(x = -Inf, xend = Inf, y = SUCRA, yend = SUCRA, colour = colour), show.legend = FALSE, alpha=0.05) +
    theme_classic() +
    theme(panel.grid.major.y = element_line(colour = c(rep("black", 6), "white")), axis.title = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), axis.line = element_blank(),
          aspect.ratio = 1, axis.text.x = element_blank()) +
    coord_polar() +
    geom_text(aes(label=stats::reorder(stringr::str_wrap(gsub("_", " ", Treatment), width = 10), -SUCRA), y = 110, angle = Angle, hjust = Adjust),
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
  SUCRA <- SUCRAData |> dplyr::arrange(-SUCRA)
  edges <- network.structure(ranking_data$BUGSnetData, my_order = SUCRA$Treatment)  # from file 'network_structure.R'
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

  # Creates the network part of the radical SUCRA plot, excluding the nodes.
  #
  # @param Type "Original" or "Alternative.
  # @return ggplot object.
  CreateNetwork <- function(Type) {
    if (Type == 'Original') {
      g <- ggplot(dat.edges, aes(x = stats::reorder(treatment, -SUCRA), y = SUCRA, group = pairwiseID)) +
        geom_line(linewidth = dat.edges$lwdO, show.legend = FALSE) +
        scale_y_continuous(limits = c(-40, 115))
    } else {
      g <- ggplot(dat.edges, aes(x = stats::reorder(treatment, -SUCRA), y = -20, group = pairwiseID)) +
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


  # Creates the nodes for the network part of the radial SUCRA plot.
  #
  # @param Type "Original" or "Alternative".
  # @param colourblind TRUE for colourblind-friendly colours (default = FALSE).
  # @return ggplot object.
  CreatePoints <- function(Type, colourblind = FALSE) {
    if (Type == 'Original') {
      g <- ggplot(SUCRAData, aes(x = stats::reorder(Treatment, -SUCRA), y = SUCRA, group = 1)) +
        geom_point(aes(fill = SUCRA, size = SizeO), size = SUCRAData$SizeO, shape = 21, show.legend = FALSE) +
        scale_y_continuous(limits = c(-40, 115))
    } else {
      g <- ggplot(SUCRAData, aes(x = stats::reorder(Treatment, -SUCRA), y = -20, group = 1)) +
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

#' Ranking probability table
#'
#' @param ranking_data list created by bayes_ranking().
#' @return dataframe
#' @export
ranking_table = function(ranking_data) {
  df <- ranking_data$Probabilities |> dplyr::right_join(ranking_data$SUCRA[,1:2], by="Treatment")
  df <- df[order(-df$SUCRA),]
  return(df)
}
