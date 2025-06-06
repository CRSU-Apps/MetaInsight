#' Converts long data to wide if necessary, then applies freq_wrap().
#' 
#' @param data Input dataset.
#' @param metaoutcome "Continuous" or "Binary".
#' @param treatment_list Data frame containing the treatment ID ('Number') and the treatment name ('Label').
#' @param outcome_measure "MD", "SMD", "OR", "RR", or "RD".
#' @param modelranfix "fixed" or "random".
#' @param reference Name of the reference treatment.
#' @return See output from freq_wrap().
frequentist <- function(data, metaoutcome, treatment_list, outcome_measure, modelranfix, reference) {
  if (FindDataShape(data) == "long") {
    data_wide <- LongToWide(long_data = data, outcome_type = metaoutcome)
  } else {
    data_wide <- data
  }
  
  # Use the self-defined function, freq_wrap
  return(
    freq_wrap(
      data = data_wide,
      treat_list = treatment_list,
      model = modelranfix,
      outcome = outcome_measure,
      CONBI = metaoutcome,
      ref = reference
    )
  )
}



#########################
##### function for transforming data to contrast form
#########################

#' Puts data in contrast form using meta::pairwise().
#' 
#' @param data Input dataset.
#' @param outcome "MD", "SMD", "OR", "RR", or "RD".
#' @param CONBI "Continuous" or "Binary".
#' @return Input data in contrast form.
contrastform.df <- function(data, outcome, CONBI) {

  #Create a list of columns of the variables to be passed to meta::pairwise()
  treat_list <- CreateListOfWideColumns(wide_data = data, column_prefix = "T")
  n_list <-  CreateListOfWideColumns(wide_data = data, column_prefix = "N")
  
  if (CONBI == 'Continuous') {
    
    mean_list <-  CreateListOfWideColumns(wide_data = data, column_prefix = "Mean")
    sd_list <-  CreateListOfWideColumns(wide_data = data, column_prefix = "SD")
    
    d1 <- meta::pairwise(treat = treat_list,
                         n = n_list,
                         mean = mean_list,
                         sd = sd_list,
                         data = data,                
                         sm = outcome,
                         studlab = data$Study)
  } else if (CONBI == 'Binary') {
    
    event_list <-  CreateListOfWideColumns(wide_data = data, column_prefix = "R")
    
    d1 <- meta::pairwise(treat = treat_list,
                         event = event_list,
                         n = n_list,
                         data = data,
                         sm = outcome,
                         studlab = data$Study)
  } else {
    stop("CONBI must be 'Continuous' or 'Binary'")
  }
  return(d1)
}



#########################
##### function for attaching treatment labels
#########################

#' Adds treatment labels to contrast data.
#' 
#' @param d1 Data in contrast form, typically created by contrastform.df().
#' @param ntx = Number of treatments.
#' @param treat_list  Data frame containing the treatment ID ('Number') and the treatment name ('Label').
#' @return Input data with the columns treat1 and treat2 now labelled.
labelmatching.df <- function(d1, ntx, treat_list) {
  d1$treat1 <- factor(d1$treat1,
                      levels = 1:ntx,
                      labels = as.character(treat_list$Label))
  d1$treat2 <- factor(d1$treat2,
                      levels = 1:ntx,
                      labels = as.character(treat_list$Label))
  return(d1)
}



##########################
##### function for conducting frequentist analysis for continuous outcome
##########################

#' Frequentist NMA.
#' 
#' @param model "fixed" or "random".
#' @param outcome "MD", "SMD", "OR", "RR", or "RD".
#' @param dataf Data in contrast form with treatment labels, typically output from labelmatching.df().
#' @param ref Reference treatment.
#' @return NMA results from netmeta::netmeta().
freq.df <- function(model, outcome, dataf, ref) {
  net1 <- netmeta::netmeta(TE = TE,
                           seTE = seTE,
                           treat1 = treat1,
                           treat2 = treat2,
                           studlab = studlab,
                           data = dataf,
                           subset = NULL,
                           sm = outcome,
                           level = 0.95,
                           level.ma = 0.95,
                           random = (model == "random"),
                           common = (model == "fixed"),
                           reference.group = ref,
                           all.treatments = NULL,
                           seq = NULL,
                           tau.preset = NULL,
                           tol.multiarm = 0.05,
                           tol.multiarm.se = 0.2,
                           warn = TRUE)
  return(net1) 
}



###********************###
### WRAPPING function: function for Wrapping the frequentist data format and analysis
###********************###


#' Calls preliminary functions, finishing with frequentist analysis in freq.df(), then outputs the model results and related objects.
#' 
#' @param data Input dataset in wide format.
#' @param treat_list Data frame containing the treatment ID ('Number') and the treatment name ('Label').
#' @param model "fixed" or "random".
#' @param outcome "MD", "SMD", "OR", "RR", or "RD".
#' @param CONBI "Continuous" or "Binary".
#' @param ref Reference treatment.
#' @return List
#'  - 'net1': NMA results from netmeta::netmeta().
#'  - 'lstx': Vector of treatment labels.
#'  - 'ntx': Number of treatments.
#'  - 'd0': Data in contrast form.
#'  - 'd1': Same as 'd0' but with treatment labels.
freq_wrap <- function(data, treat_list, model, outcome, CONBI, ref) {
  progress <- shiny::Progress$new()   # Adding progress bars
  on.exit(progress$close())
  progress$set(message = "Updating", value = 0)
  d0 <- contrastform.df(data, outcome, CONBI)    # transform data to contrast form
  lstx <- treat_list$Label      #obtain treatment labels
  ntx <- length(lstx)     #count treatment numbers
  d1 <- labelmatching.df(d1 = d0, ntx = ntx, treat_list = treat_list) #matching treatment labels to treatment code
  progress$inc(0.6, detail = "Updating")
  net1 <- freq.df(model = model, outcome = outcome, dataf = d1, ref = ref) # NMA of all studies
  progress$inc(0.4, detail = "Rendering results")
  return(list(net1 = net1, lstx = lstx, ntx = ntx, d0 = d0, d1 = d1))
}



#########################
##### function for producing group forest plot
#########################

#' Produces the forest plot of raw outcomes, with one section per treatment contrast. For a given treatment contrast, the section contains one confidence interval for each study that directly compares the two treatments in question.
#'
#' @param d1 Data in contrast form, typically output from contrastform.df() or labelmatching.df().
#' @param ntx Number of treatments.
#' @param lstx Vector of treatment labels.
#' @param outcome "MD", "SMD", "OR", "RR", or "RD".
#' @param HeaderSize Multiplier for size of text in treatment contrast headers.
#' @param TitleSize Multiplier for size of title.
#' @return The forest plot, created using metafor::forest().
groupforest.df <- function(d1, ntx, lstx, outcome, HeaderSize, TitleSize) {
  
  #Drop rows corresponding to NA treatment effects
  d1 <- d1[!is.na(d1$TE), ]
  
  text_label <- character()
  n_stud <- integer()
  for (i in 1:ntx) {
    for (j in 1:ntx) {
      if (nrow(d1[(d1$treat1 == i & d1$treat2 == j),]) > 0) {
        text_label <- c(paste(lstx[i], "vs", lstx[j]), text_label)
        n_stud <- c(n_stud, nrow(d1[(d1$treat1 == i & d1$treat2 == j),]))
      }
    }
  }
  gaps <- integer(length(n_stud))
  n_stud <- rev(n_stud)
  for (k in 1:length(n_stud)) {
    if (k == 1) {
      gaps[k] <- n_stud[k] + 1
    }
    else {
      gaps[k] <- gaps[k-1] + n_stud[k] + 2
    }
  }
  lines <- rev(c(1:(nrow(d1) + 2 * length(text_label) - 1)))
  lines <- lines[!lines %in% gaps]
  lines <- lines[!lines %in% (gaps + 1)]
  
  if (max(lines) < 28) {size = 7
  } else if (max(lines) >= 28 & max(lines) <= 40) {size = max(lines) / 4
  } else if (max(lines) > 40 & max(lines) <= 70) {size = max(lines) / 5
  } else if (max(lines) > 70 & max(lines) <= 100) {size = max(lines) / 6
  } else if (max(lines) > 100 & max(lines) <= 130) {size = max(lines) / 7
  } else {size = max(lines) / 8
  } # sizing for output
  
  d1 <- d1[order(d1$treat1, d1$treat2, d1$StudyID), ] #ensuring the ordering is correct
  
  if (outcome == "OR" | outcome =="RR" ){
    fplot <- metafor::forest(d1$TE, sei = d1$seTE, slab = paste(d1$Study), subset = order(d1$treat1, d1$treat2),
                             ylim = c(0, nrow(d1) + 2 * length(text_label) + 2), rows = lines, atransf = exp,
                             at = log(c(0.01, 1, 10, 100)), xlab = paste("Observed ", outcome), efac = 0.5
                             )
  } else {
    fplot <- metafor::forest(d1$TE, sei = d1$seTE, slab = paste(d1$Study), subset = order(d1$treat1, d1$treat2),
                             ylim = c(0, nrow(d1) + 2 * length(text_label) + 2), rows = lines,
                             xlab = paste("Observed ",outcome), efac = 0.5)
  }
  text(fplot$xlim[1], gaps, pos = 4, font = 4, text_label, cex = HeaderSize)
  title("Individual study results (with selected studies excluded) grouped by treatment comparison", cex.main = TitleSize)
  list(fplot = fplot, size = size)
}



##########################
##### function for drawing forest plot
##########################

#' Creates the forest plot from netmeta::netmeta() output.
#'
#' @param netresult NMA results from netmeta::netmeta(), as produced by freq_wrap().
#' @param model "fixed" or "random".
#' @param ntx Number of treatments.
#' @param lstx Vector of treatment labels.
#' @param ref Reference treatment.
#' @param min Minimum x-axis limit.
#' @param max Maximum x-axis limit.
#' @return Forest plot created using metafor::forest().
forest.df <- function(netresult, model, lstx, ref, min, max) {
  return(metafor::forest(netresult, reference.group = ref, pooled = model, xlim = c(min, max)))
}



#######################
##### function for text underneath forest plot
#######################

#' Creates the text to be displayed underneath the forest plots, with between-study SD, number of studies and number of treatments.
#'
#' @param tau Between study standard deviation.
#' @param k Number of studies.
#' @param n Number of treatments.
#' @param model "fixed" or "random".
#' @param outcome "MD", "SMD", "OR", "RR", or "RD".
#' @return Text as described above.
tau.df <- function(tau, k, n, model, outcome) {
  if (model == "random") {
    if (outcome == "OR") {
      output_text <- paste("Between-study standard deviation (log-odds scale):", tau,
                           ", Number of studies:", k,
                           ", Number of treatments:", n)
    } else if (outcome == "RR") {
      output_text <- paste("Between-study standard deviation (log probability scale):", tau,
                           ", Number of studies:", k,
                           ", Number of treatments:", n)
    } else if(outcome %in% c("MD", "SMD", "RD")) {
      output_text <- paste("Between-study standard deviation:", tau,
                           ", Number of studies:", k,
                           ", Number of treatments:", n)
    } else {
      stop("outcome must be one of 'MD', 'SMD', 'OR', 'RR', 'RD'")
    }
  } else if (model == "fixed") {
    if (outcome == "OR") {
      output_text <- paste("Between-study standard deviation (log-odds scale) set at 0. Number of studies:", k,
                           ", Number of treatments:", n)
      }
    else if (outcome == "RR") {
      output_text <- paste("Between-study standard deviation (log probability scale) set at 0. Number of studies:", k,
                           ", Number of treatments:", n)}
    else if(outcome %in% c("MD", "SMD", "RD")) {
      output_text <- paste("Between-study standard deviation set at 0. Number of studies:", k,
                           ", Number of treatments:", n)
    } else {
      stop("outcome must be one of 'MD', 'SMD', 'OR', 'RR', 'RD'")
    }
  } else {
    stop("model must be 'fixed' or 'random'")
  }
  return(output_text)
}



#######################
##### function for exporting netsplit (netmeta) results
#######################

#' Organises output from netmeta::netsplit() into a data frame.
#'
#' @param incona Output from netmeta::netsplit().
#' @param model "fixed" or "random".
#' @return Data frame of output from netmeta::netsplit()
#'  - 'Comparison': Treatment comparison.
#'  - 'No.Studies': Number of studies.
#'  - 'NMA': NMA treatment effect estimate.
#'  - 'Direct': Direct treatment effect estimate.
#'  - 'Indirect': Indirect treatment effect estimate.
#'  - 'Difference': Difference between treatment effects.
#'  - 'Diff_95CI_lower': 2.5% limit of difference in treatment effects.
#'  - 'Diff_95CI_upper': 97.5% limit of difference in treatment effects.
#'  - 'pValue': p-value for test of "difference in treatment effects == 0".
netsplitresult.df <- function(incona, model) {
  Comparison <- incona$comparison
  No.Studies <- as.integer(incona$k)
  
  if (model == "random") {
    Direct <- incona$direct.random$TE
    Indirect <- incona$indirect.random$TE
    Difference <- incona$compare.random$TE
    Diff_95CI_lower <- incona$compare.random$lower
    Diff_95CI_upper <- incona$compare.random$upper
    NMA <- incona$random$TE
    pValue <- incona$compare.random$p
  } else if (model == "fixed") {
    Direct <- incona$direct.fixed$TE
    Indirect <- incona$indirect.fixed$TE
    Difference <- incona$compare.fixed$TE
    Diff_95CI_lower <- incona$compare.fixed$lower
    Diff_95CI_upper <- incona$compare.fixed$upper
    NMA <- incona$fixed$TE
    pValue <- incona$compare.fixed$p
  } else {
    stop("model must be 'fixed' or 'random'")
  }
  return(data.frame(Comparison, No.Studies, NMA, Direct, Indirect, Difference, Diff_95CI_lower,
                    Diff_95CI_upper, pValue
                    )
         )
}



#######################
##### function for progress bar
#######################

#' Progress bar.
#'
#' @return shiny::withProgress() with some options set.
progress.df <- function() {
  withProgress(message = 'loading', value = 0, {
    n <- 10
    for (i in 1:n) {
      # Increment the progress bar, and update the detail text.
      incProgress(1/n, detail = paste(""))
    }
  })
}


