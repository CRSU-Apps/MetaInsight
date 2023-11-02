frequentist <- function(data, metaoutcome, treatment_list, outcome_measure, modelranfix, excluded=c()){
  data_wide <-  entry.df(data, metaoutcome) # Transform data to wide form
  
  # Subset of data when studies excluded
  if (length(excluded) > 0) {
    data_wide <- dplyr::filter(data_wide, !Study %in% excluded)
  }
  
  # Use the self-defined function, freq_wrap
  return(freq_wrap(data_wide, treatment_list, modelranfix, outcome_measure, metaoutcome, 
                   ref_alter(data, metaoutcome, excluded, treatment_list)$ref_sub))
}



##########################
##### function for formating uploaded data
##########################

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

contrastform.df <- function(data, outcome, CONBI) {
  if (CONBI=='Continuous') {
    d1<- pairwise(treat=list(T.1,T.2,T.3,T.4,T.5,T.6),n=list(N.1,N.2,N.3,N.4,N.5,N.6),mean=list(Mean.1,Mean.2,Mean.3,Mean.4,Mean.5,Mean.6),sd=list(SD.1,SD.2,SD.3,SD.4,SD.5,SD.6),data=data,sm=outcome)
  } else {
    d1<- pairwise(treat=list(T.1,T.2,T.3,T.4,T.5,T.6),event=list(R.1,R.2,R.3,R.4,R.5,R.6),n=list(N.1,N.2,N.3,N.4,N.5,N.6),data=data,sm=outcome)
  }
  return(d1)
}



#########################
##### function for attaching treatment labels
#########################

labelmatching.df <- function(d1,ntx,treat_list) {
  d1$treat1 <- factor(d1$treat1,
                      levels = c(1:ntx),
                      labels = as.character(treat_list$Label))
  d1$treat2 <- factor(d1$treat2,
                      levels = c(1:ntx),
                      labels = as.character(treat_list$Label))
  return(d1)
}



##########################
##### function for conducting frequentist analysis for continuous outcome
##########################

freq.df <- function(model,outcome,dataf,lstx, ref) {
  net1 <- netmeta(TE, seTE, treat1, treat2, studlab, data = dataf, subset=NULL,
                  sm = outcome, level=0.95, level.comb=0.95,
                  comb.random=(model=="random"), comb.fixed = (model=="fixed"),reference.group =ref,
                  all.treatments=NULL, seq=NULL, tau.preset=NULL,
                  tol.multiarm = 0.05, tol.multiarm.se = 0.2, warn=TRUE)
  return(net1) 
}



###********************###
### WRAPPING function: function for Wrapping the frequentist data format and analysis
###********************###

freq_wrap <- function(data, treat_list,model,outcome, CONBI, ref) {
  progress <- shiny::Progress$new()   # Adding progress bars
  on.exit(progress$close())
  progress$set(message="Updating", value=0)
  d0<- contrastform.df(data,outcome, CONBI)    # transform data to contrast form
  lstx <- treat_list$Label      #obtain treatment labels
  ntx <- length(lstx)     #count treatment numbers
  d1<-labelmatching.df(d0, ntx, treat_list) #matching treatment labels to treatment code
  progress$inc(0.6, detail="Updating")
  net1<-freq.df(model,outcome,d1,lstx,ref) # NMA of all studies
  progress$inc(0.4, detail="Rendering results")
  return (list(net1=net1,lstx=lstx, ntx=ntx,d0=d0,d1=d1))
}



#########################
##### function for producing group forest plot
#########################

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
  lines <- rev(c(1:(nrow(d1) + 2*length(text_label)-1)))
  lines <- lines[!lines %in% gaps]
  lines <- lines[!lines %in% (gaps+1)]
  
  if (max(lines) < 28) {size=7
  } else if (max(lines)>=28 & max(lines)<=40) {size=max(lines)/4
  } else if (max(lines)>40 & max(lines)<=70) {size=max(lines)/5
  } else if (max(lines)>70 & max(lines)<=100) {size=max(lines)/6
  } else if (max(lines)>100 & max(lines)<=130) {size=max(lines)/7
  } else {size=max(lines)/8
  } # sizing for output
  
  d1 <- d1[order(d1$treat1,d1$treat2,d1$StudyID),] #ensuring the ordering is correct
  
  if (outcome == "OR" | outcome =="RR" ){
    fplot <- metafor::forest(d1$TE, sei=d1$seTE, slab = paste(d1$Study), subset = order(d1$treat1, d1$treat2), ylim = c(1, nrow(d1) + 2*length(text_label) + 2),rows = lines, 
                             atransf = exp, at = log(c(0.01, 1, 10, 100)), xlab = paste("Observed ",outcome), efac=0.5 
    )
  } 
  else {
    fplot <- metafor::forest(d1$TE, sei=d1$seTE, slab = paste(d1$Study), subset = order(d1$treat1, d1$treat2), ylim = c(1, nrow(d1) + 2*length(text_label) + 2), rows = lines,
                             xlab = paste("Observed ",outcome), efac=0.5)
  }
  text(fplot$xlim[1], gaps, pos=4, font = 4, text_label, cex=HeaderSize)
  title("Individual study results (for all studies) grouped by treatment comparison", cex.main=TitleSize)
  list(fplot=fplot,size=size)
}

# } else if (outcome == "RD") {
#   fplot <- NULL



##########################
##### function for drawing forest plot
##########################

forest.df <- function(netresult,model,lstx,ref,min,max) {
    fp<- metafor::forest(netresult,reference.group=ref, pooled=model, xlim=c(min, max))
return(fp)    
}



#######################
##### function for text underneath forest plot
#######################

tau.df <- function(tau,k,n,model,outcome) {
  if (model=="random"){
    if (outcome=="OR"){
      y<-paste("Between-study standard deviation (log-odds scale):", tau,
               ", Number of studies:", k,
               ", Number of treatments:", n)}
    else if (outcome=="RR"){
      y<-paste("Between-study standard deviation (log probability scale):", tau,
               ", Number of studies:", k,
               ", Number of treatments:", n)}
    else {
      y<-paste("Between-study standard deviation:", tau,
               ", Number of studies:", k,
               ", Number of treatments:", n)}}
  else {
    if (outcome=="OR"){
      y<-paste("Between-study standard deviation (log-odds scale) set at 0. Number of studies:", k,
               ", Number of treatments:", n)}
    else if (outcome=="RR"){
      y<-paste("Between-study standard deviation (log probability scale) set at 0. Number of studies:", k,
               ", Number of treatments:", n)}
    else {
      y<-paste("Between-study standard deviation set at 0. Number of studies:", k,
               ", Number of treatments:", n)}
  }
  return(y)
}



#######################
##### function for exporting netsplit (netmeta) results
#######################

netsplitresult.df <- function(incona, model) {
  Comparison<- incona$comparison
  No.Studies<- as.integer(incona$k)
  
  if (model=="random"){
    Direct<- incona$direct.random$TE
    Indirect<- incona$indirect.random$TE
    Difference<- incona$compare.random$TE
    Diff_95CI_lower<- incona$compare.random$lower
    Diff_95CI_upper<- incona$compare.random$upper
    NMA<- incona$random$TE
    pValue<- incona$compare.random$p}
  else{
    Direct<- incona$direct.fixed$TE
    Indirect<- incona$indirect.fixed$TE
    Difference<- incona$compare.fixed$TE
    Diff_95CI_lower<- incona$compare.fixed$lower
    Diff_95CI_upper<- incona$compare.fixed$upper
    NMA<- incona$fixed$TE
    pValue<- incona$compare.fixed$p}
  df<- data.frame(Comparison, No.Studies, NMA, Direct, Indirect, Difference, Diff_95CI_lower, Diff_95CI_upper, pValue)
  return(df)
}



#######################
##### function for progress bar
#######################

progress.df <- function() {
  withProgress(message = 'loading', value = 0, {
    n <- 10
    for (i in 1:n) {
      # Increment the progress bar, and update the detail text.
      incProgress(1/n, detail = paste(""))
    }
  })
}


