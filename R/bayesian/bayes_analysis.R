#' Bayesian analysis.
#'
#' @param data Data frame in long format, with 'se' instead of 'SD' and 'N' when the outcome is continuous.
#' @param treat_list Data frame consisting of the treatment IDs ('Number') and the treatment names ('Label').
#' @param model "random" or "fixed"
#' @param outcome "MD", "OR" or "RR".
#' @param CONBI "Continuous" or "Binary".
#' @param ref Reference treatment.
#' @return List:
#'  - 'mtcResults' = Output from gemtc::mtc.run
#'  - 'lstx' = Vector of treatment names
#'  - 'ntx' = Number of treatments
#'  - 'treat_list2' = Same as @param treat_list (TM: Not sure why this is here)
#'  - 'mtcRelEffects' = Output from gemtc::relative.effect
#'  - 'rel_eff_tbl = Output from gemtc::relative.effect.table
#'  - 'sumresults' = summary(mtcRelEffects)
#'  - 'a' = "fixed effect" or "random effect"
#'  - 'mtcNetwork' = Output from gemtc::mtc.network
#'  - 'dic' = Data frame containing the statistics 'Dbar', 'pD', 'DIC', and 'data points'
#'  - 'model' = Same as @param model
#'  - 'outcome' = Same as @param outcome
baye <- function(data, treat_list, model, outcome, CONBI, ref) {
  if (!outcome %in% c('OR', 'RR', 'MD')) {
    stop(glue::glue("Outcome type '{outcome}' is not supported. Please use one of: 'MD', 'OR', 'RR'"))
  }
  
  progress <- shiny::Progress$new()   # Adding progress bars
  on.exit(progress$close())
  progress$set(message = "Updating.This may take up to 10 minutes", value = 0)
  treat_list2 <- data.frame(treat_list)
  
  if (CONBI == "Continuous") { 
    armData <- data.frame(study = data$Study,       # Create arm level data set for gemtc
                          treatment = data$T,
                          mean = data$Mean,
                          std.err = data$se)
  } else if (CONBI == "Binary") {
    armData <- data.frame(study = data$Study,
                          treatment = data$T,
                          responders = data$R,
                          sampleSize = data$N)
  }
  
  progress$inc(0.2, detail = "Preparing to run simulation models")
  mtcNetwork <- mtc.network(data.ab = armData, description = "Network")   # Gemtc network object
  
  if (outcome == "MD") {
    like <- "normal"
    link <- "identity"
  } else if (outcome == "OR" || outcome == "RR") {
    like <- "binom"
    link <- ifelse (outcome == "OR","logit", "log")
  }
  
  set.seed(145) # needs to be set before mtc.model
  
  mtcModel <- mtc.model(
    network = mtcNetwork,
    type = "consistency",
    linearModel = model,
    likelihood = like,
    link = link,
    dic = TRUE
  )
  
  # Settings for JAGS seeds and generator types for reproducible results (code taken from mtc.model manual)
  seeds <- sample.int(4, n = .Machine$integer.max) # 4 chains
  mtcModel$inits <- mapply(c, mtcModel$inits, list(
    list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = seeds[1]),
    list(.RNG.name = "base::Marsaglia-Multicarry", .RNG.seed = seeds[2]),
    list(.RNG.name = "base::Super-Duper", .RNG.seed = seeds[3]),
    list(.RNG.name = "base::Mersenne-Twister", .RNG.seed = seeds[4])),
    SIMPLIFY = FALSE)
  
  progress$inc(0.4, detail = "Running simulation models")
  mtcResults <- mtc.run(mtcModel)   # Run gemtc model object for analysis
  progress$inc(0.4, detail = "Rendering results")
  mtcRelEffects <- relative.effect(mtcResults, t1 = ref)  #Set reference treatment
  rel_eff_tbl <- relative.effect.table(mtcResults) 
  sumresults <- summary(mtcRelEffects)
  a <- paste(model, "effect", sep = " ")   #Create text for random/fixed effect
  lstx <- treat_list$Label # Treatment names
  ntx <- nrow(treat_list) # Number of treatments
  sumoverall <- summary(mtcResults)
  dic <- as.data.frame(sumoverall$DIC) # The statistics 'Dbar', 'pD', 'DIC', and 'data points'
  
  return(
    list(
      mtcResults = mtcResults,
      lstx = lstx,
      ntx = ntx,
      treat_list2 = treat_list2,
      mtcRelEffects = mtcRelEffects,
      rel_eff_tbl=rel_eff_tbl,
      sumresults = sumresults,
      a = a,
      mtcNetwork = mtcNetwork,
      dic = dic,
      model = model,
      outcome = outcome
    )
  )
}



#' Create text with the point estimate and 95% CrI of between-trial SD of treatment effects.
#'
#' @param results Output from the 'baye' function. These are the list elements that are relevant:
#'  - 'mtcResults' = Output from gemtc::mtc.run
#'  - 'sumresults' = summary(mtcRelEffects)
#'  - 'a' = "fixed effect" or "random effect"
#' @param outcome One of "SMD", "RD", "MD", "OR". Anything else is interpreted as RR. (TM: Probably don't need this, as it's included as @param results$outcome)
#' @return Text with the point estimate and 95% CrI of between-trial SD of treatment effects (all 0 if fixed effects)
CreateTauSentence <- function(results,outcome) {
  if (!outcome %in% c('OR', 'RR', 'MD')) {
    stop(glue::glue("Outcome type '{outcome}' is not supported. Please use one of: 'MD', 'OR', 'RR'"))
  }
  
  sumresults <- results$sumresults
  if (results$a == "random effect") {   #SD and its 2.5% and 97.5%
    sd_mean <- round(sumresults$summaries$statistics["sd.d", "Mean"], digits = 2)
    sd_lowCI <- round(sumresults$summaries$quantiles["sd.d", "2.5%"], digits = 2)
    sd_highCI <- round(sumresults$summaries$quantiles["sd.d", "97.5%"], digits=2)
  }   else {
    sd_mean = 0
    sd_lowCI = 0
    sd_highCI = 0
  }
  if (results$a=="random effect") {
    if (outcome=="OR") {
      paste("Between-study standard deviation (log-odds scale):", sd_mean, ". 95% credible interval:",sd_lowCI,", ", sd_highCI, ".")
    } else if (outcome=="RR") {
      paste ("Between-study standard deviation (log probability scale):", sd_mean, ". 95% credible interval:",sd_lowCI,", ", sd_highCI, ".")
    } else {
      paste ("Between-study standard deviation:", sd_mean, ". 95% credible interval:",sd_lowCI,", ", sd_highCI, ".")
    }
  } else{
    if (outcome=="OR") {
      paste("Between-study standard deviation (log-odds scale) set at 0")
    } else if (outcome=="RR") {
      paste("Between-study standard deviation (log probability scale) set at 0")
    } else {
      paste("Between-study standard deviation set at 0")
    }
  }
}


#' Put the output from a bnma model into the format of a gemtc model, in order to apply CreateTauSentence().
#'
#' @param br_model Output from bnma::network.run.
#' @return List:
#'  - 'sumresults' = List:
#'    - 'summaries' = equivalent of summary(gemtc model)$summaries.
#'    - 'a' = "fixed effect" or "random effect".
FormatForCreateTauSentence <- function(br_model){
  br_summary <- summary(br_model)
  #Rename "sd" (bnma name) to "sd.d" (gemtc name)
  rownames(br_summary$summary.samples$statistics)[rownames(br_summary$summary.samples$statistics) == "sd"] <- "sd.d"
  rownames(br_summary$summary.samples$quantiles)[rownames(br_summary$summary.samples$quantiles) == "sd"] <- "sd.d"
  return(
    list(
      sumresults = list(
        summaries = br_summary$summary.samples
      ),
      a = paste0(br_model$network$type, " effect")
    )
  )
}



### 3c. Ranking results 


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
      print(
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
  Cumulative_Data <- Cumulative_Data %>% left_join(SUCRA, by = "Treatment")
  
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
  SUCRA <- SUCRA %>% dplyr::right_join(Patients, by = "Treatment")
  
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
  BUGSnetData <- data.prep(arm.data = longdata, varname.t = "T", varname.s = "Study")
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

# text to go underneath plots #
relative_rank_text <- function(results) {
  paste("Forest plot of relative effects from Bayesian ", results$a, " consistency model", sep="")
}



#' 3d. nodesplit models.
#' 
#' @param data Input data set.
#' @param treat_list Data frame consisting of the treatment IDs ('Number') and the treatment names ('Label').
#' @param model "random" or "fixed".
#' @param outcome "MD", "SMD", "OR", "RR" or "RD".
#' @param CONBI "Continuous" or "Binary".
#' @return Nodesplit model in mtc.nodesplit object.
bayenode <- function(data, treat_list, model, outcome, CONBI) {
  if (!outcome %in% c('OR', 'RR', 'MD')) {
    stop(glue::glue("Outcome type '{outcome}' is not supported. Please use one of: 'MD', 'OR', 'RR'"))
  }
  
  if (outcome=="SMD" ) {
    print("Please note: standardised mean difference currently cannot be analysed in Bayesian analysis", type = "warning")
  } 
  else if (outcome == "RD") {
    print("Please note: Bayesian analysis of risk differences is not currently implemented in MetaInsight", type = "warning")
  } else if (outcome %in% c("MD", "OR", "RR")) {
    progress <- shiny::Progress$new()   # Adding progress bars
    on.exit(progress$close())
    progress$set(message = "Updating. This may take up to 20 minute", value = 0)
    lstx <- treat_list$Label
    ntx <- nrow(treat_list)
    progress$inc(0.2, detail = "Preparing to run simulation models")
    if (CONBI == "Continuous") { 
      armData <- data.frame(study = data$Study,       # Create arm level data set for gemtc
                            treatment = data$T,
                            mean = data$Mean,
                            std.err = data$se)
    } else {
      armData <- data.frame(study = data$Study,
                            treatment = data$T,
                            responders = data$R,
                            sampleSize = data$N)
    }
    mtcNetwork <- mtc.network(data.ab = armData, description = "Network",
                               treatments = data.frame(id = treat_list$Label, description = treat_list$Label)) # treatment argument needed so that it takes the reference treatment correctly
    progress$inc(0.4, detail = "Running simulation models")
    if (outcome == "MD") {
      like <- "normal"
      link <- "identity"
    } else  {
      like <- "binom"
      link <- ifelse (outcome == "OR", "logit", "log")
    }
    nodeSplitResults <- mtc.nodesplit(network = mtcNetwork,
                                      linearModel = model,
                                      likelihood = like,
                                      link = link)  # nodesplitting
    progress$inc(0.4, detail = "Rendering results")
    return(nodeSplitResults)
  } else {
    stop("outcome must be one of 'MD', 'SMD', 'OR', 'RR', or 'RD'") 
  }
}



### 3f. UME deviance scatter plot

#' Create unrelated mean effects (UME) plot.
#'
#' @param residual_deviance Data frame of residual deviance by arm, from a gemtc mtc.result object.
#' @param mtcNetwork GEMTC network object.
#' @param model Model effects type. "random" or "fixed".
#' @param outcome Outcome measure being analysed: one of "OR". "RR", "MD".
#'
#' @return Plot and deviance report object in a list of "p" and "y" respectively.
umeplot.df <- function(residual_deviance, mtcNetwork, model, outcome) {
  if (!outcome %in% c('OR', 'RR', 'MD')) {
    stop(glue::glue("Outcome type '{outcome}' is not supported. Please use one of: 'MD', 'OR', 'RR'"))
  }

  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Updating.", value = 0)
  residual_deviance$names <- rownames(residual_deviance)
  
  if (outcome == "MD") {
    like <- "normal"
    link <- "identity"
  } else if (outcome == "OR" || outcome == "RR") {
    like <- "binom"
    link <- ifelse (outcome == "OR", "logit", "log")
  } else {
    stop("outcome must be 'MD', 'OR', or 'RR'") 
  }
  
  set.seed(145) # needs to be set before mtc.model
  
  ume <- mtc.model(network = mtcNetwork,
                   type = "ume",
                   linearModel = model, 
                   likelihood = like,
                   link = link,
                   dic = TRUE)
  progress$inc(0.5, detail = "Preparing results")
  
  seeds <- sample.int(4, n = .Machine$integer.max) # 4 chains
  ume$inits <- mapply(c, ume$inits, list(
    list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = seeds[1]),
    list(.RNG.name = "base::Marsaglia-Multicarry", .RNG.seed = seeds[2]),
    list(.RNG.name = "base::Super-Duper", .RNG.seed = seeds[3]),
    list(.RNG.name = "base::Mersenne-Twister", .RNG.seed = seeds[4])),
    SIMPLIFY = FALSE)
  
  ume_results <- mtc.run(ume)
  progress$inc(0.3, detail = "Rendering results")
  y <- mtc.deviance({ume_results})
  inc <- data.frame(y$dev.ab)
  inc$names <- rownames(inc)
  all <- merge(residual_deviance, inc, by = "names")
  
  names(all)[names(all) == "X1.x"] <- "NMAmodel_arm1"
  names(all)[names(all) == "X1.y"] <- "UMEmodel_arm1"
  names(all)[names(all) == "X2.x"] <- "NMAmodel_arm2"
  names(all)[names(all) == "X2.y"] <- "UMEmodel_arm2"
  
  k <- all[, names(all) != "names"]  #### to define the maximum range of the equality line: find the maximum number of the dev data in the dataset.
  j <- max(k, na.rm = TRUE)
  m <- c(0, 1, j)
  n <- c(0, 1, j)
  
  dline <- data.frame(m, n)
  
  #The maximum number of arms
  max_arms <- ncol(residual_deviance) - 1

  p = plot_ly() %>%   # plot
    add_trace(data = dline, x = ~m, y = ~n, type = 'scatter', mode = 'lines',
              line = list(color = '#45171D'))
  p = p %>%
    add_trace(data = all, x = ~NMAmodel_arm1, y = ~UMEmodel_arm1, type = 'scatter', mode = 'markers',
              marker = list(size = 4, color = '#CAEFD1',
                          line = list(color = 'rgb(0,128,0)',
                                      width = 2)),
              hoverinfo='text',
              text=~paste('</br> Author', all$names,
                          '</br> Arm 1',
                          '</br> Deviance from NMA model:',round(NMAmodel_arm1, digits = 2),
                          '</br> Deviance from UME model:',round(UMEmodel_arm1, digits = 2)
              ))
  p = p %>% 
    add_trace(
      x = ~NMAmodel_arm2, y = ~UMEmodel_arm2, type = 'scatter', mode = 'markers', 
      marker = list(size = 4, color = '#CAEFD1',
                  line = list(color = 'rgb(0,128,0)',
                              width = 2)),
      hoverinfo = 'text',
      text = ~paste('</br> Author', all$names,
                  '</br> Arm 2',
                  '</br> Deviance from NMA model:', round(NMAmodel_arm2, digits = 2),
                  '</br> Deviance from UME model:', round(UMEmodel_arm2, digits = 2)
                  
      ))%>%
    layout(showlegend = FALSE, xaxis = list(title = "Deviance from NMA model"), 
           yaxis = list(title = "Deviance from UME inconsistency model"))
  
  if (max_arms >= 3) {
    for (arm_number in 3:max_arms) {
      p <- .AddTraceToUmePlot(ume_plot = p, all = all, arm_number = arm_number)
    }
  }

  progress$inc(0.2, detail = "Exporting results")
  
  return(list(p = p, y = y))
}



#' Apply plotly::add_trace to the UME plot when there are more than two arms.
#'
#' @param ume_plot The current UME plot.
#' @param all The full data frame of residual deviances.
#' @param arm_number Current arm number.
#' @return 'ume_plot' with additional points added, corresponding to 'arm_number'.
.AddTraceToUmePlot <- function(ume_plot, all, arm_number) {
  x_column <- paste0("X", arm_number, ".x")
  y_column <- paste0("X", arm_number, ".y")
  
  return(
    ume_plot %>% 
      add_trace(data = all,
                x = ~all[, x_column], y = ~all[, y_column], type = 'scatter', mode = 'markers', 
                marker = list(size = 4, color = '#CAEFD1',
                              line = list(color = 'rgb(0,128,0)',
                                          width = 2)),
                hoverinfo = 'text',
                text = ~paste('</br>', all$names,
                              '</br> Arm', arm_number, 
                              '</br> Deviance from NMA model:', round(all[, x_column], digits = 2),
                              '</br> Deviance from UME model:', round(all[, y_column], digits = 2))
                )
  )
}


#' Per-arm residual deviance plot.
#' 
#' @param c Deviance obtained from @param x.
#' @param x gemtc::mtc.deviance() object.
#' @return Creates a stemplot of residual deviances by arm.
stemplot.df <- function(c, x) {
  tpl <- x[['dev.ab']]
  study <- matrix(rep(1:nrow(tpl), times = ncol(tpl)), nrow = nrow(tpl), ncol = ncol(tpl))
  study <- t(study)[t(!is.na(tpl))]
  devbar <- t(x[['dev.ab']])[t(!is.na(tpl))]
  title <- "Per-arm residual deviance"
  xlab <- "Arm"
  k <- rowSums(!is.na(tpl))
  studynames <- rep(c$names, k)
  v <- 1:length(devbar)
  sep <- study%%2
  d <- data.frame(v, devbar, sep, study, studynames)
  xl <- list(
    title = xlab,
    range= c(0, length(devbar) + 5),
    tick0 = 0,
    dtick = 5,
    zeroline = TRUE,
    showline = TRUE
  )
  yl <- list(
    title = "Residual deviance",
    range = c(0, ceiling(devbar)),
    autorange = TRUE,
    tick0 = 0,
    dtick = 0.5,
    zeroline = TRUE,
    showline = TRUE
  )
  p <- plot_ly(data = d, x = ~v, y = ~devbar)
  for (i in 1:length(devbar)) {
    p = p %>%
      add_segments(x = i,
                   xend = i,
                   y = 0,
                   yend = devbar[i],
                   marker = list(color = 'white', 
                                 line = list(color = 'white')
                                 ),
                   line = list(color = 'black',
                               width = 1)
                   )
  }
  p = p%>% 
    add_trace(data = d, x = ~v, y = ~devbar, type = 'scatter', mode = 'markers', 
              marker = list(size = 4,
                            color = '#CAEFD1',
                            line = list(color = 'rgb(0,128,0)',
                                        width = 2)
                            ),
              symbol = ~sep, symbols = c('circle', 'o'), 
              hoverinfo = 'text',
              text = ~paste('</br> Study', d$studynames,
                            '</br> Deviance from NMA model:', round(d$devbar, digits = 2)
              )) %>%
    layout(
      xaxis = xl, yaxis = yl, showlegend = FALSE)
  return(p)
}



#' Creates a plot of leverage versus residual deviance, by study.
#' 
#' @param x gemtc::mtc.deviance() object.
#' @return Leverage vs residual deviance plot.
levplot.df <- function(x) {
  fit.ab <- apply(x[['fit.ab']], 1, sum, na.rm = TRUE)
  dev.ab <- apply(x[['dev.ab']], 1, sum, na.rm = TRUE)
  lev.ab <- dev.ab - fit.ab
  fit.re <- x[['fit.re']]
  dev.re <- x[['dev.re']]
  lev.re <- dev.re - fit.re
  nd <- c(x[['nd.ab']], x[['nd.re']])
  w <- sqrt(c(dev.ab, dev.re) / nd)
  lev <- c(lev.ab, lev.re) / nd
  d <- data.frame(w, lev)
  d$names <- rownames(d)
  
  a <- seq(from = 0, to = 3, by = 0.05)
  b1 <- 1 - a^2
  b2 <- 2 - a^2
  b3 <- 3 - a^2
  b4 <- 4 - a^2
  parabola <- data.frame(a, b1, b2, b3, b4)
  
  xlab = "Square root of average residual deviance across the arms for each study"
  'sqrt(average(residual deviance for arm 1, residual deviance for arm 2...))'
  ylab = "Average leverage across the arms for each study"
  
  xl <- list(
    title = xlab,
    range = c(0, max(c(w, 2.5))),
    tick0 = 0,
    dtick = 0.5,
    zeroline = TRUE,
    showgrid = TRUE
  )
  yl <- list(
    title=ylab,
    range=c(0, max(c(lev, 4))),
    tick0 = 0,
    dtick = 1,
    zeroline = TRUE,
    showgrid = TRUE
  )
  
  p <- plot_ly(parabola, x = ~a) %>%
    add_trace(y = b1, mode = 'lines', line = list(color = 'black', width = 1), hoverinfo = 'skip') %>%
    add_trace(y = b2, mode = 'lines', line = list(color = 'black', width = 1), hoverinfo = 'skip') %>%
    add_trace(y = b3, mode = 'lines', line = list(color = 'black', width = 1), hoverinfo = 'skip') %>%
    add_trace(y = b4, mode = 'lines', line = list(color = 'black', width = 1), hoverinfo = 'skip') %>%
    add_trace(data = d, x = ~w, y = ~lev, 
              marker = list(size = 4,
                            color = '#CAEFD1',
                            line = list(color = 'rgb(0,128,0)',
                                        width = 2)
                            ),
              hoverinfo = 'text',
              text = ~paste('</br> Study:', d$names,
                            '</br> Deviance',round(d$w, digits = 2),
                            '</br> Leverage',round(d$lev, digits = 2)
              )) %>%
    layout(
      xaxis = xl, yaxis = yl, showlegend = FALSE, title = "Leverage versus residual deviance")
  return(p)
}
