#####
# Plot functions used in both app and report - NVB
#####

#' 1a Summary table plot
#'
#' @param bugsnetdt Long format data.
#' @param metaoutcome "Continuous" or "Binary".
#' @return Network created by BUGSnet::net.tab().
#' @export
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
#' @export
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
#' @export
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
#' @export
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
#' @export
make_netconnect_old <- function(freq) {
  d1 <- freq$d1
  nc <- netmeta::netconnection(treat1 = d1$treat1, treat2 = d1$treat2, studLab = d1$studlab, data = NULL)
  summary <- nc[names(nc) %in% c("k", "m", "n", "n.subnets", "d")]
  return(unlist(summary))
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





# 3f Deviance report

#' UME scatter plot
#'
#' @param model Various model output created by baye().
#' @return UME scatter plot created by umeplot.df().
scat_plot_old = function(model) {
  x <- gemtc::mtc.deviance(model$mtcResults)
  c <- data.frame(x$dev.ab)
  umeplot.df(c = c, mtcNetwork = model$mtcNetwork, model = model$model, outcome = model$outcome)
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


#' Creates a Gelman plot for a BNMA baseline-risk model.
#'
#' @param gelman_plot Output from coda::gelman.plot(bnma_model$samples[, parm]), where parm is a parameter from 'bnma_model'.
#' @param parameter The parameter from the previous argument, used as the title.
#' @return Reproduces the Gelman plot mentioned in @param gelman_plot as a plot that can be put in a grid.
BnmaGelmanPlot <- function(gelman_plot, parameter){
  y_vals_median <- gelman_plot$shrink[, , "median"]
  y_vals_975 <- gelman_plot$shrink[, , "97.5%"]
  x_vals <- gelman_plot$last.iter

  plot(x_vals, y_vals_975, type = "l", col = "red", lty = 2, ylab = "shrink factor",
       xlab = "last iteration in chain", cex.lab = 1.5, cex.main = 1.5, main = parameter)
  lines(x_vals, y_vals_median, type = "l")
  lines(c(-max(x_vals)/5, max(x_vals)), c(1, 1))
  legend("topright", legend = c("median", "97.5%"), lty = c(1, 2), col = c("black", "red"))
}


#' Creates Gelman plots for a BNMA baseline-risk model.
#'
#' @param gelman_plots List of outputs from coda::gelman.plot(bnma_model$samples[, parm]), where parm is a parameter from bnma_model.
#' @param parameters Vector of parameters mentioned in the previous argument.
#' @return Plots the Gelman plots mentioned in @param gelman_plots.
BnmaGelmanPlots <- function(gelman_plots, parameters){
  for (i in 1:length(parameters)) {
    BnmaGelmanPlot(gelman_plot = gelman_plots[[i]], parameter = parameters[i])
  }
}
