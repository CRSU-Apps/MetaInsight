#' Converts long data to wide if necessary, then applies freq_wrap().
#' 
#' @param data Input dataset.
#' @param metaoutcome "Continuous" or "Binary".
#' @param treatment_list Data frame containing the treatment ID ('Number') and the treatment name ('Label').
#' @param outcome_measure "MD", "SMD", "OR", "RR", or "RD".
#' @param modelranfix "fixed" or "random".
#' @param excluded = Vector of studies to be excluded for sensitivity analysis.
#' @return See output from freq_wrap().
frequentist <- function(data, metaoutcome, treatment_list, outcome_measure, modelranfix, excluded=c()){
  data_wide <-  entry.df(data = data, CONBI = metaoutcome) # Transform data to wide form
  
  # Subset of data when studies excluded
  if (length(excluded) > 0) {
    data_wide <- dplyr::filter(data_wide, !Study %in% excluded)
  }
  
  altered_reference <- ref_alter(data = data, metaoutcome = metaoutcome, excluded = excluded,
                                 treatment_list = treatment_list)$ref_sub
  
  # Use the self-defined function, freq_wrap
  return(freq_wrap(data = data_wide, treat_list = treatment_list, model = modelranfix,
                   outcome = outcome_measure, CONBI = metaoutcome, ref = altered_reference
                   )
         )
}



#' Converts long data to wide, leaves wide data unchanged.
#' 
#' @param data Input dataset.
#' @param CONBI "Continuous" or "Binary".
#' @return Input data in long format.
entry.df <- function(data, CONBI) {
  newData1 <- as.data.frame(data)
  if (ncol(newData1)==6 | ncol(newData1)==5){
    newData2<-newData1[order(newData1$StudyID, -newData1$T), ]
    newData2$number<- ave(as.numeric(newData2$StudyID),newData2$StudyID,FUN=seq_along)  # create counting variable for number of arms within each study.
    data_wide <- reshape(newData2, timevar = "number",idvar = c("Study", "StudyID"), direction = "wide") # reshape
    numbertreat=max(newData2$number)
  }
  else {
    data_wide<- newData1
    a<- ifelse(CONBI=='Continuous', 4, 3)
    numbertreat=(ncol(newData1)-2)/a
  }
  if (numbertreat < 6) {  # generate additional columns if less than 6 arms.
    for (k in (numbertreat+1):6) {
      if (CONBI=='Continuous') {
        data_wide[c(paste0("T.",k),paste0("N.",k),paste0("Mean.",k),paste0("SD.",k))]<-NA
      } else {
        data_wide[c(paste0("T.",k),paste0("R.",k),paste0("N.",k))]<-NA
      }
    }
  }
  return(data_wide)
}



#########################
##### function for transforming data to contrast form
#########################

#' Puts data in contrast form using netmeta::pairwise().
#' 
#' @param data Input dataset.
#' @param outcome "MD", "SMD", "OR", "RR", or "RD".
#' @param CONBI "Continuous" or "Binary".
#' @return Input data in contrast form.
contrastform.df <- function(data, outcome, CONBI) {
  if (CONBI == 'Continuous') {
    d1 <- netmeta::pairwise(treat = list(T.1, T.2, T.3, T.4, T.5, T.6),
                            n = list(N.1, N.2, N.3, N.4, N.5, N.6),
                            mean = list(Mean.1, Mean.2, Mean.3, Mean.4, Mean.5, Mean.6),
                            sd = list(SD.1, SD.2, SD.3, SD.4, SD.5, SD.6),
                            data = data,                
                            sm = outcome)
  } else if (CONBI == 'Binary') {
    d1 <- netmeta::pairwise(treat = list(T.1, T.2, T.3, T.4, T.5, T.6),
                            event = list(R.1, R.2, R.3, R.4, R.5, R.6),
                            n = list(N.1, N.2, N.3, N.4, N.5, N.6),
                            data = data,
                            sm = outcome)
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
#' @param lstx Vector of treatment labels. (TM: this parameter is not used. Removing it has knock on effects due to parameters not being named when this function is called. To be removed in a later task).
#' @param ref Reference treatment.
#' @return NMA results from netmeta::netmeta().
freq.df <- function(model, outcome, dataf, lstx, ref) {
  net1 <- netmeta(TE = TE, seTE = seTE, treat1 = treat1, treat2 = treat2, studlab = studlab, data = dataf, subset=NULL,
                  sm = outcome, level = 0.95, level.comb=0.95, comb.random = (model == "random"),
                  comb.fixed = (model == "fixed"), reference.group = ref, all.treatments = NULL, seq = NULL,
                  tau.preset = NULL, tol.multiarm = 0.05, tol.multiarm.se = 0.2, warn = TRUE)
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
  net1 <- freq.df(model = model, outcome = outcome, dataf = d1, lstx = lstx, ref = ref) # NMA of all studies
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
                             ylim = c(1, nrow(d1) + 2 * length(text_label) + 2), rows = lines, atransf = exp,
                             at = log(c(0.01, 1, 10, 100)), xlab = paste("Observed ", outcome), efac = 0.5
                             )
  } else {
    fplot <- metafor::forest(d1$TE, sei = d1$seTE, slab = paste(d1$Study), subset = order(d1$treat1, d1$treat2),
                             ylim = c(1, nrow(d1) + 2 * length(text_label) + 2), rows = lines,
                             xlab = paste("Observed ",outcome), efac = 0.5)
  }
  text(fplot$xlim[1], gaps, pos = 4, font = 4, text_label, cex = HeaderSize)
  title("Individual study results (with studies excluded) grouped by treatment comparison", cex.main = TitleSize)
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


