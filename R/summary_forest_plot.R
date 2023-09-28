
#' Create a summary forest plot matrix.
#'
#' @param data_to_plot The meta-analysis data to plot
#' @param treatment_df Data frame containing the names and IDs of all treatments
#' @param plot_title Title of the plot
#' @param outcome_type Type of outcome being plotted
#' @param desirability "good" if the treatment effect is desirable, else "bad"
#' @param model "random" or "fixed" effects model being plotted
#' @return Summary forest matrix plot
CreateSummaryForestPlot <- function(data_to_plot, treatment_df, plot_title, outcome_type, desirability, model) {
  lstx <- treatment_df$Label
  ntx <- length(lstx)
  
  net1 <- data_to_plot$net1
  
  ma <- list()
  mtc <- list()
  
  # Update data in ma and mtc for plots
  ma$lor <- matrix(0, nrow = sum(1:(ntx - 1)), ncol = 7)
  ma$or <- matrix(0, nrow = sum(1:(ntx - 1)), ncol = 7)
  ma$predint <- matrix(0, nrow = sum(1:(ntx - 1)), ncol = 7)
  mtc$lor <- matrix(0, nrow = sum(1:(ntx - 1)), ncol = 7)
  mtc$or <- matrix(0, nrow = sum(1:(ntx - 1)), ncol = 7)
  mtc$rank <- matrix(0, nrow = ntx, ncol = 4)
  mtc$predint <- matrix(0, nrow = sum(1:(ntx - 1)), ncol = 4)
  mtc$rkgram <- matrix(0, nrow = ntx * ntx, ncol = 2)
  
  small_value_desirability <- ifelse(desirability == "good", "desirable", "undesirable")
  rkgrm <- netmeta::rankogram(net1, small.values = small_value_desirability)
  
  if (model == "random") {
    ranking <- rkgrm$ranking.random
    ranking_matrix <- rkgrm$ranking.matrix.random
  } else {
    ranking <- rkgrm$ranking.common
    ranking_matrix <- rkgrm$ranking.matrix.common
  }
  ordered_treatment_names <- names(ranking[order(-ranking)])
  # The rankogram command seems to sort the treatments alphabetically first, so this line converts back to the original treatment IDs
  mtc$rank[, 3] <- match(treatment_df$Label, ordered_treatment_names)
  
  for (index in 1:ntx) {
    mtc$rkgram[((index - 1) * ntx) + (1:ntx), 1] <- ranking_matrix[ordered_treatment_names[index], ]
  }
  
  mtc$type <- outcome_type
  
  count <- 1
  for (i in 1:(ntx - 1)) {
    for (j in (i + 1):ntx) {
      
      ma$lor[count, 5] <- net1$lower.direct.random[i, j]
      ma$lor[count, 6] <- net1$TE.direct.random[i, j]
      ma$lor[count, 7] <- net1$upper.direct.random[i, j]
      
      mtc$lor[count, 2] <- net1$lower.random[i, j]
      mtc$lor[count, 3] <- net1$TE.random[i, j]
      mtc$lor[count, 4] <- net1$upper.random[i, j]
      
      mtc$predint[count, 2] <- net1$lower.predict[i, j]
      mtc$predint[count, 4] <- net1$upper.predict[i, j]
      
      if (outcome_type == "RR" | outcome_type == "OR") {
        ma$or[count, 5:7] <- exp(ma$lor[count, 5:7])
        mtc$or[count, 2:4] <- exp(mtc$lor[count, 2:4])
      } else {
        ma$or[count, 5:7] <- ma$lor[count, 5:7]
        mtc$or[count, 2:4] <- mtc$lor[count, 2:4]
      }
      
      count <- count + 1
    }
  }
  
  prjtitle <- "Summary Forest Plot"
  mtcMatrixCont(
    prjtitle,
    ntx,
    lstx,
    mtc,
    ma,
    bpredd = TRUE,
    bkey = TRUE,
    p.only = ntx,
    ucex = 1.5
  )
}

#' MTC & MA estimates for Forest Matrix plot - used in for loop in multiplot function.
#'
#' @param mtc Meta-analysis data for direct and indirect evidence
#' @param pw Meta-analysis data for direct (pairwise) evidence only
#' @param xpos Position of text in X axis
#' @param ucex Text size
singleest <- function(mtc, pw, xpos = 0, ucex) {
  
  #define pos to be the same
  ypos <- 0
  
  #NMA
  text(xpos, (ypos + 2), sprintf("%.2f", mtc[2]), adj = 0.5, cex = ucex, col = "black")
  text(xpos, (ypos + 1), sprintf("(%.2f to %.2f)", mtc[1], mtc[3]), adj = 0.5, cex = ucex, col = "black")
  #PW
  text(xpos, (ypos - 1), sprintf("%.2f", pw[2]), adj = 0.5, cex = ucex, col = "grey55")
  if (!is.na(pw[2])) {
    text(xpos, (ypos - 2), sprintf("(%.2f to %.2f)",  pw[1], pw[3]), adj = 0.5, cex = ucex, col = "grey55")
  }
}

#' Function to draw two-tiered error bars for summary relative estimates.
#'
#' @param offs Axis offset
#' @param lower_confidence_limit Lower limit of confidence interval
#' @param point_estimate Point estimate
#' @param upper_confidence_limit Upper limit of confidence interval
#' @param ypos Position of error bars in Y axis
#' @param ucol Text colour. Defaults to black
#' @param ulwd Line width. Defaults to 1
#' @param pcI  TRUE if both confidence interval and predictive interval to be plotted, otherwise only confidence interval plotted. Defaults to FALSE
#' @param predbd Predictive data
PrICrI <- function(offs, lower_confidence_limit, point_estimate, upper_confidence_limit, ypos, ucol = "black", ulwd = 1, pcI = FALSE, predbd = c(NA, NA)) {
  # Show predictive interval
  if (pcI & predbd[1] != 0 & predbd[2] != 0) {
    # Predictive interval line
    lines(c(predbd[1] + offs, predbd[2] + offs), c(ypos, ypos), lty = "dotted", lwd = ulwd, col = ucol)
    # Predictive lower limit line
    lines(c(predbd[1] + offs, predbd[1] + offs), c(ypos - 0.2, ypos + 0.2), lty = 1, lwd = ulwd, col = ucol)
    # Predictive upper limit line
    lines(c(predbd[2] + offs, predbd[2] + offs), c(ypos - 0.2, ypos + 0.2), lty = 1, lwd = ulwd, col = ucol)
  }
  
  # Confidence interval line
  lines(
    c(lower_confidence_limit + offs, upper_confidence_limit + offs),
    c(ypos, ypos),
    col = ucol,
    lty = 1,
    lwd = ulwd,
    adj = 0.5
  )
  # Confidence interval 2.5% line
  lines(
    c(lower_confidence_limit + offs, lower_confidence_limit + offs),
    c(ypos - 0.4, ypos + 0.4),
    lty = 1,
    lwd = ulwd,
    col = ucol
  )
  # Confidence interval 97.5% line
  lines(
    c(upper_confidence_limit + offs, upper_confidence_limit + offs),
    c(ypos - 0.4, ypos + 0.4),
    lty = 1,
    lwd = ulwd,
    col = ucol
  )
  
  points(point_estimate + offs, ypos, pch = 15, cex = 0.8 * ulwd, col = ucol, adj = 0.5)
}

#' Single plot for MTC & MA SFP in NMA SPF Matrix.
#'
#' @param mtc Meta-analysis data for direct and indirect evidence
#' @param pw Meta-analysis data for direct (pairwise) evidence only
#' @param bpredd TRUE if predictive interval to be plotted as error bars
#' @param baxis TRUE if axes to be drawn for forest plots
#' @param scaletype The outcome type being plotted. "RR" for risk ratio, and "OR" for odds ratio will be plotted on a log scale, anything else will be plotted on a linear scale
singleSFP <- function(mtc, pw, bpredd = TRUE, baxis = TRUE, scaletype) {
  
  ##define axis offset
  offs = 0
  
  #Add reference line. This plots at 0 for continuous scales, and 1 for log scales
  lines(c(offs, offs), c(-3, 3), lty = 1, col = "grey80")
  
  #define pos to be the same, so that the two SFP line are plotted at y = (-0.5 & 0.5)
  pos <- 1
  if (!bpredd) {
    #NMA
    PrICrI(offs, mtc[1], mtc[2], mtc[3], pos, ulwd = 1.5)
    #PW
    PrICrI(offs, pw[1], pw[2], pw[3], pos - 2, ucol = "grey55", ulwd = 1.5)
  } else {
    #NMA
    PrICrI(offs, mtc[1], mtc[2], mtc[3], pos, ulwd = 1.5, pcI = TRUE, predbd = c(mtc[4], mtc[5]))		#summary w PrI
    #PW
    PrICrI(offs, pw[1], pw[2], pw[3], pos - 2, ucol = "grey55", ulwd = 1.5, pcI = TRUE, predbd = c(pw[4], pw[5]))		#summary  w PrI
  }
  
  #Add axis for last row
  if (baxis) {
    if (scaletype == "OR" | scaletype == "RR") { # Odds ratio or risk ratio log scale
      vticks <- c(1 / 1024, 1 / 256, 1 / 64, 1 / 16, 1 / 4, 1, 4, 16, 64, 256, 1024)
      lnticks <- log(vticks)
      lblticks <- c("1/1024", "1/256", "1/64", "1/16", "1/4", "1", "4", "16", "64", "256", "1024")
      axis(1, at = lnticks, labels = lblticks, cex.axis = 0.6, padj = -1.0, tck = -0.05)
    } else { #continuous data scale
      axis(1, at = NULL, labels = TRUE, cex.axis = 0.6, padj = -1.0, tck = -0.05)
    }
  }
}

#' Function to draw graphs along diagonal - for NMA SPF Matrix.
#'
#' @param ntx Number of treatments
#' @param rkgram Vector containing rankogram data
#' @param cumu True if to draw cumulative rankogram. Defaults to False
rankogram <- function(ntx, rkgram, cumu = FALSE) {
  ori.ntx <- length(rkgram) / ntx
  xseq <- seq(0, 1, length.out = (2 * ntx + 1))
  rankmat <- array(rkgram, c(ori.ntx, ntx))
  
  if (cumu) {
    rank.cumprob <- apply(rankmat, 2, cumsum)   #2:indicates column to all apply to
  }
  
  for (i in 1:ntx) {
    if (cumu) {
      par(fig = c(xseq[2 * i - 1], xseq[2 * i + 1], (1 - xseq[2 * i + 1]), (1 - xseq[2 * i])), new = TRUE, mar = c(1.2, 1.5, 0, 0.6))
    } else {
      par(fig = c(xseq[2 * i - 1], xseq[2 * i + 1], (1 - xseq[2 * i + 1]), (1.01 - xseq[2 * i])), new = TRUE, mar = c(1.2, 1.5, 0, 0.6))
    }
    plot(1:ori.ntx, seq(-1, 1, len = ori.ntx), type = "n",  ylab = "", xlab = "", ylim = c(0, 1), axes = FALSE)
    
    if (!cumu) {
      lines(1:ori.ntx, rankmat[, i], lwd = 1.8)
    } else {
      lines(c(1, c(1:c(ori.ntx - 1)) + 0.5, ori.ntx), rank.cumprob[c(1, 1:c(ori.ntx - 1), ori.ntx), i])
    }
    
    xticks <- c(1:ori.ntx)
    xlblticks <- c(1, rep("", (ori.ntx - 2)), ori.ntx)
    axis(1, at = xticks, labels = xlblticks, lwd = 0.8, cex.axis = 0.7, tck = -0.05, padj = -2, col = 'grey70', col.axis = 'grey70') #bottom axis
    yticks <- seq(0.0, 1.0, by = 0.5)
    axis(2, at = yticks, labels = yticks, lwd = 0.8, cex.axis = 0.7, las = 2, tck = -0.06, hadj = 0.2, line = -0.2, col = 'grey70', col.axis = 'grey70')  #left axis
  }
}

#' Function to create a vector indexing shading in mtcMultiplot.
#'
#' @param ntx Number of treatments
#'
#' @return the calculated shading vector
shading.vec <- function(ntx) {
  ordvec <- seq(1, ntx * ntx)
  shgvec <- rep(0, ntx * ntx)
  
  #Odd Number interventions
  shgvec[(ntx %% 2 != 0) & ordvec %% 2 != 0] <- 1
  
  #Even Number interventions
  shgvec[(ntx %% 2 == 0) & ((ceiling(ordvec / ntx)) %% 2 == 0) & (ordvec %% 2 == 0)] <- 1
  shgvec[(ntx %% 2 == 0) & ((ceiling(ordvec / ntx)) %% 2 != 0) & (ordvec %% 2 != 0)] <- 1
  
  return(shgvec)
}


#' Draw the elements of the summary forest matrix plot.
#'
#' @param stytitle Title of plot
#' @param ntx Number of treatments
#' @param lstx Vector of treatment names
#' @param mtc Meta-analysis data for direct and indirect evidence
#' @param ma Meta-analysis data for direct evidence only
#' @param bpredd TRUE if predictive interval to be plotted as error bars
#' @param plt.adj Plot position adjustment
#' @param ucex Text size
multiplot <- function(stytitle, ntx, lstx, mtc, ma, bpredd = TRUE, plt.adj, ucex) {
  
  #Start a matrix plot - define number of elements "squares" in Matrix
  tplot <- ntx * ntx
  
  if (plt.adj == 0) {
    par(mfcol = c(ntx, ntx), oma = c(3.5, 0, 2, 0))
  } else if (plt.adj == 1) {
    par(mfcol = c(ntx, ntx), oma = c(5, 0, 2, 0))
  } else if (plt.adj == 2) {
    par(mfcol = c(ntx, ntx), oma = c(6, 0, 2, 0))
  }
  
  
  #define cex - text size
  ucex <- 1.1 * ucex
  
  #create the vector indexing shading for use later in function
  shgvector <- shading.vec(ntx)
  
  #determine the plot (x-axis) range based on the MTC & MA results and midpoint (xpos) for printing text (SFP estimates, etc.)
  # symref = TRUE indicates that reference line must be at mid-point of plot, while symref = FALSE allows reference line to be data-driven but definitely on the plot.
  symref <- FALSE     #symref <- TRUE
  
  if (!symref) {
    if (!bpredd) {
      #Check the maximum required print range using lor range
      side.xl <- min(0, floor(min(ma$lor[, 5], mtc$lor[, 2], na.rm = TRUE)), na.rm = TRUE)
      side.xu <- max(0, ceiling(max(ma$lor[, 7], mtc$lor[, 4], na.rm = TRUE)), na.rm = TRUE)
    } else {
      #Check the maximum required print range using predictive interval range
      side.xl <- min(0, floor(min(ma$lor[, 5], mtc$lor[, 2], ma$predint[, 5], mtc$predint[, 2], na.rm = TRUE)), na.rm = TRUE)
      side.xu <- max(0, ceiling(max(ma$lor[, 7], mtc$lor[, 4], ma$predint[, 7], mtc$predint[, 4], na.rm = TRUE)), na.rm = TRUE)
    }
    xpos <- (side.xl + side.xu) / 2
  } else {   #symref
    xpos <- 0
    if (!bpredd) {
      #Check the maximum required print range using lor range
      absside <- max(abs(ma$lor[, 5:7]), abs(mtc$lor[, 2:4]), na.rm = TRUE)
      side.xl <- -1 * ceiling(absside)
      side.xu <- ceiling(absside)
    } else {
      #Check the maximum required print range using predictive interval range
      absside <- max(abs(ma$predint[, 5:7]), abs(mtc$predint[, 2:4]), na.rm = TRUE)
      side.xl <- -1 * ceiling(absside)
      side.xu <- ceiling(absside)
    }
  }
  
  i.pt <- 0
  i.tx <- 0
  for (i in 1:tplot) {
    
    par(mar = c(0.3, 0.25, 0.2, 0.25))
    plot(1:10, seq(-3, 3, len = 10), type = "n", axes = FALSE, ylab = "", xlab = "", xlim = c(side.xl, side.xu))
    
    #matrix cells alternate background shading
    if (shgvector[i] == 1) {
      rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "grey95")
    } else {
      rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "white")
    }
    
    i.pt <- i.pt + 1
    if (i %% (ntx + 1) == 1) {
      ## Diagonal
      if (i %% ntx != 0) {
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "white")
        text(xpos, 2.0, lstx[i %% ntx], cex = ucex, adj = c(0.5, 0.5), font = 2)
        text(xpos + 0.6, -0.1, sprintf("Rank = %.0f", mtc$rank[i %% ntx, 3]), cex = ucex * 0.9, adj = 0.5, font = 1)
      } else { #last box
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "white")
        text(xpos, 2.0, lstx[ntx], cex = ucex, adj = c(0.5, 0), font = 2)   #NOTE difference here for lstx!!
        text(xpos + 0.6, -0.1, sprintf("Rank = %.0f", mtc$rank[ntx, 3]), cex = ucex * 0.9, adj = 0.5, font = 1)
      }
      i.pt <- i.pt - 1
      
    } else if (i %% ntx == 0 | i %% ntx > i %/% ntx) {
      ##Lower triangle
      axis <- (i %% ntx == 0)
      
      if (!bpredd) {
        mtc.est <- c(mtc$lor[i.pt, 2], mtc$lor[i.pt, 3], mtc$lor[i.pt, 4])
        pw.est <- c(ma$lor[i.pt, 5], ma$lor[i.pt, 6], ma$lor[i.pt, 7])
      } else {
        mtc.est <- c(mtc$lor[i.pt, 2], mtc$lor[i.pt, 3], mtc$lor[i.pt, 4], mtc$predint[i.pt, 2], mtc$predint[i.pt, 4])
        pw.est <- c(ma$lor[i.pt, 5], ma$lor[i.pt, 6], ma$lor[i.pt, 7], ma$predint[i.pt, 5], ma$predint[i.pt, 7])
      }
      singleSFP(mtc.est, pw.est, bpredd, axis, mtc$type)
    } else if (i %% ntx <= i %/% ntx) {
      ##Upper triangle
      i.pt <- i.pt - 1
      i.tx <- ((i %% ntx) - 1) * ntx + (i %/% ntx + 1) - (sum(seq((i %% ntx))))
      mtc.or <- c(mtc$or[i.tx, 2], mtc$or[i.tx, 3], mtc$or[i.tx, 4])
      pw.or <- c(ma$or[i.tx, 5], ma$or[i.tx, 6], ma$or[i.tx, 7])
      
      singleest(mtc.or, pw.or, xpos, ucex + 0.1)
    }
    
    ##Draw a box around the multiple plots
    if (shgvector[i] == 1) {
      box(lty = 1, col = 'grey85')
    } else {
      box(lty = 1, col = 'grey85')
    }
  }
  
  #Identify original number of tx in analysis - for use in rankjar function
  ori.ntx <- length(mtc$rkgram[, 1]) / ntx
  
  #Draw graphs along the diagonal
  rankogram(ntx, mtc$rkgram[, 1])
  
  #Insert the graph title and x-axis title
  title(main = stytitle, outer = TRUE, cex.main = 1.5)
  
  label_elements <- c(" with 95% CI")
  if (bpredd) {
    label_elements <- c(label_elements, " & 95% PI")
  }
  
  if (mtc$type == "OR") {
    label_elements <- c("Odds Ratio", label_elements, " (Log scale)")
  } else if (mtc$type == "RR") {
    label_elements <- c("Risk Ratio", label_elements, " (Log scale)")
  } else if (mtc$type == "RD") {
    label_elements <- c("Risk Difference", label_elements)
  } else if (mtc$type == "MD") {
    label_elements <- c("Mean Difference", label_elements)
  } else if (mtc$type == "SMD") {
    label_elements <- c("Standardised Mean Difference", label_elements)
  }
  
  if (plt.adj == 0) {
    mtext(stringr::str_flatten(label_elements), side = 1, outer = TRUE, line = 2, cex = 0.75)
  } else {
    mtext(stringr::str_flatten(label_elements), side = 1, outer = TRUE, line = 1.5, cex = 0.75)
  }
}

#' Function to create the matrix sorting order - to be used for the MTC & MA numerical results (Upper triangle results),
#'
#' @param ntx Number of treatments
#' @param po Ranking order of treatments
#'
#' @return Sorting order matrix
sortres.matrix <- function(ntx, po) {
  
  #Correctly create corresponding treatment code no.
  txcode <- c(1:ntx)
  #Sorted tx code list
  st.txcode <- txcode[po]  #gives same results as po but doing this to be cautious
  
  #create new 'ranked' tx combinations
  # require install.packages("combinat")
  cm <- t(combn(st.txcode, 2))  #combination matrix of ntx choose 2
  
  #New 'ranked' tx combinations matrix made up of c("ordering", "t1", "t2", "inversion number")
  mtnew <- cbind(1:(choose(ntx, 2)), cm, (ifelse(cm[, 1] < cm[, 2], 1, -1)) )
  
  mtnew[(cm[, 1] > cm[, 2]), c(1, 2, 3, 4)] <- mtnew[(cm[, 1] > cm[, 2]), c(1, 3, 2, 4)]
  
  #ordering sorted by t1(ref) followed by t2(comparator)
  mo <- order(mtnew[, 2], mtnew[, 3], decreasing = FALSE)
  #All columns to be sorted by mo order
  mtorg <- mtnew[mo, ]   #Matrix having some format ordering as the standard WinsBUGS output
  
  return(mtorg)
}

#' Function to create the rankgram matrix sorting order.
#'
#' @param ntx Number of treatments
#' @param po Ranking order of treatments
#'
#' @return Rankgram matrix sorting order
sortrkg.ord <- function(ntx, po) {
  
  #Correctly create corresponding treatment code no.
  txcode <- c(1:ntx)
  #Sorted tx code list
  st.txcode <- txcode[po]  #gives same results as po but doing this to be cautious
  
  #create new 'ranked' rankogram combinations
  rkgnew <- array(c(1:(ntx * ntx), rep(st.txcode, each = ntx, len = ntx * ntx), rep(1:ntx, ntx, len = ntx * ntx)), c(ntx * ntx, 3))
  
  rkgmio <- order(rkgnew[, 2], rkgnew[, 3], decreasing = FALSE) #intermediate ordering
  rkgmtorg <- cbind(c(1:(ntx * ntx)), rkgnew[rkgmio, ])  #matrix from WinBUGS; col 1 for checking purpose only
  rkgmo <- order(rkgmtorg[, 2], decreasing = FALSE)
  
  return(rkgmo)
}

#' Function to update the pairwise meta-analysis results after changes to the tx rankings.
#'
#' @param ma Meta-analysis data for direct evidence only
#' @param mtc Meta-analysis data for direct and indirect evidence
#' @param mtorg Sorting order matrix
#'
#' @return Updated meta-analysis data
ma.sortres <- function(ma, mtc, mtorg) {
  #Re-calculate estimates after inverting the reference group, using mtorg[, 4 = inv]
  tmp.lor <- mtorg[, 4] * (ma$lor[, 4:7])
  tmp.or <- mtorg[, 4] * (ma$or[, 4:7])
  tmp.predint <- mtorg[, 4] * (ma$predint[, 4:7])
  
  #swap 2.5% & 97.5% estimates for those that we inverted the reference group
  tmp.lor[(mtorg[, 4] == -1), c(1, 2, 3, 4)] <- tmp.lor[(mtorg[, 4] == -1), c(1, 4, 3, 2)]
  tmp.or[(mtorg[, 4] == -1), c(1, 2, 3, 4)] <- tmp.or[(mtorg[, 4] == -1), c(1, 4, 3, 2)]
  tmp.predint[(mtorg[, 4] == -1), c(1, 2, 3, 4)] <- tmp.predint[(mtorg[, 4] == -1), c(1, 4, 3, 2)]
  
  tmp.lor <- cbind(ma$lor[, 1:3], tmp.lor)
  tmp.or <- cbind(ma$or[, 1:3], tmp.or)
  tmp.predint <- cbind(ma$predint[, 1:3], tmp.predint)
  
  #find order of final matrix for plotting
  mtord <- order(mtorg[, 1])  #gives same results as mo
  
  new.lor <- tmp.lor[mtord, ]
  new.or <- tmp.or[mtord, ]
  new.predint <- tmp.predint[mtord, ]
  
  newma <- list(lor = new.lor, or = new.or, predint = new.predint, type = mtc$type)
  return(newma)
}

#' Function to update the MTC results after changes to the tx rankings.
#'
#' @param mtc Meta-analysis data for direct and indirect evidence
#' @param mtorg Sorting order matrix
#' @param rkgmo Rankgram matrix sorting order
#' @param po Matrix containing ranking order of treatments
#'
#' @return Updated meta-analysis data
mtc.sortres <- function(mtc, mtorg, rkgmo, po) {
  #~VECTORS~
  new.rank <- mtc$rank[po, ]
  if (exists("sucra", where = mtc)) {
    new.sucra <- mtc$sucra[po]
  } else {
    new.sucra <- c(0)
  }
  
  #~MATRIX~
  new.rkgram <- mtc$rkgram
  
  #Re-calculate estimates after inverting the reference group, using mtorg[, 4 = inv]
  tmp.lor <- mtorg[, 4] * (mtc$lor[, 1:4])
  tmp.or <- mtorg[, 4] * (mtc$or[, 1:4])
  tmp.predint <- mtorg[, 4] * (mtc$predint[, 1:4])
  
  #swap 2.5% & 97.5% estimates for those that we inverted the reference group
  tmp.lor[(mtorg[, 4] == -1), c(1, 2, 3, 4)] <- tmp.lor[(mtorg[, 4] == -1), c(1, 4, 3, 2)]
  tmp.or[(mtorg[, 4] == -1), c(1, 2, 3, 4)] <- tmp.or[(mtorg[, 4] == -1), c(1, 4, 3, 2)]
  tmp.predint[(mtorg[, 4] == -1), c(1, 2, 3, 4)] <- tmp.predint[(mtorg[, 4] == -1), c(1, 4, 3, 2)]
  
  #find order of final matrix for plotting
  mtord <- order(mtorg[, 1])  #Note: does not give same results as mo!
  
  new.lor <- tmp.lor[mtord, ]
  new.or <- tmp.or[mtord, ]
  new.predint <- tmp.predint[mtord, ]
  
  newmtc <- list(lor = new.lor, or = new.or, predint = new.predint, rkgram = new.rkgram, rank = new.rank, sucra = new.sucra, tau = mtc$tau, type = mtc$type)
  return(newmtc)
}

#' Function to create the matrix reduction vector.
#'
#' @param ntx Number of treatments
#' @param po Matrix containing ranking order of treatments
#' @param p.only Number of treatments to plot
#' @param mtorg Sorting order matrix
#'
#' @return Matrix reduction vector
redu.matrix <- function(ntx, po, p.only, mtorg) {
  
  #Correctly create corresponding treatment code no.
  txcode <- c(1:ntx)
  #Sorted tx code list
  st.txcode <- txcode[po[1:p.only]]  #gives same results as po but doing this to be cautious
  
  r.mtorg <- mtorg[order(mtorg[, 1]), ]
  rmt <- (match(r.mtorg[, 2], st.txcode)) + (match(r.mtorg[, 3], st.txcode))
  
  return(rmt)
}

#' Function to reduce the matrix size based on user defined plotting range reduction.
#'
#' @param ma Meta-analysis data for direct evidence only
#' @param rmt Matrix reduction vector
#'
#' @return Reduced meta-analysis data
ma.redu <- function(ma, rmt) {
  new.lor <- ma$lor[!is.na(rmt), ]
  new.or <-  ma$or[!is.na(rmt), ]
  new.predint <-  ma$predint[!is.na(rmt), ]
  
  newma <- list(lor = new.lor, or = new.or, predint = new.predint, type = ma$type)
  return(newma)
}

#' Function to reduce the matrix size based on user defined plotting range reduction.
#'
#' @param mtc Meta-analysis data for direct and indirect evidence
#' @param rmt Matrix reduction vector
#' @param p.only Number of treatments to plot
#' @param po Matrix containing ranking order of treatments
#'
#' @return Reduced meta-analysis data
mtc.redu <- function(mtc, rmt, p.only, po) {
  #~VECTORS~ inputed mtc === st.mtc # already sorted, just truncate directly
  new.rank <- mtc$rank[1:p.only, ]
  if (exists("sucra", where = mtc)) {
    new.sucra <- mtc$sucra[1:p.only]
  }
  
  #~MATRIX~
  new.lor <- mtc$lor[!is.na(rmt), ]
  new.or <-  mtc$or[!is.na(rmt), ]
  new.predint <-  mtc$predint[!is.na(rmt), ]
  new.rkgram <- mtc$rkgram[1:(ntx * p.only), ]
  
  newmtc <- list(lor = new.lor, or = new.or, predint = new.predint, rkgram = new.rkgram, rank = new.rank, sucra = new.sucra, tau = mtc$tau, type = mtc$type)
  return(newmtc)
}

#' Create summary forest matrix plot.
#'
#' @param stytitle Title of plot
#' @param ntx Number of treatments
#' @param lstx Vector of treatment names
#' @param mtc Meta-analysis data for direct and indirect evidence
#' @param ma Meta-analysis data for direct evidence only
#' @param bpredd TRUE if predictive interval to be plotted as error bars
#' @param bkey TRUE if key should be included in plot
#' @param p.only Number of treatments to plot
#' @param ucex Font size multiplier. Defaults to 1
mtcMatrixCont <- function(stytitle, ntx, lstx, mtc, ma, bpredd = TRUE, bkey = TRUE, p.only = ntx, ucex = 1) {
  if (p.only < 3) {
    stop("Print selection must not be less than 3")
  } else if (p.only > ntx) {
    stop("Print selection cannot be more than the total number of interventions")
  }
  
  if (!bkey) {
    plt.adj <- 0
  } else if (p.only < ntx) {
    plt.adj <- 2
  } else {
    plt.adj <- 1
  }
  
  sp.order <- "Interventions are displayed sorted by SUCRA value."
  po <- order(mtc$rank[, 3], decreasing = FALSE)
  mtso <- sortres.matrix(ntx, po)
  rkgmo <- sortrkg.ord(ntx, po)
  st.ma <- ma.sortres(ma, mtc, mtso)
  st.mtc <- mtc.sortres(mtc, mtso, rkgmo, po)
  st.lstx <- lstx[po]
  
  if (p.only == ntx) {
    sp.only <- ""
    key.ypos <- 1.3
    
    multiplot(stytitle, ntx, st.lstx, st.mtc, st.ma, bpredd, plt.adj, ucex)
  } else {
    sp.only <- sprintf("A total of %i interventions were compared in this NMA but only %i interventions were displayed in this plot.", ntx, p.only)
    key.ypos <- 2.3
    
    #reduce the results matrices & vectors
    mtred <- redu.matrix(ntx, po, p.only, mtso)
    r.ma <- ma.redu(st.ma, mtred)
    r.mtc <- mtc.redu(st.mtc, mtred, p.only, po)
    r.lstx <- st.lstx[1:p.only]
    multiplot(stytitle, p.only, r.lstx, r.mtc, r.ma, bpredd, plt.adj, ucex)
  }
  
  if (bkey) {
    if (!bpredd) {
      slgd <- "NMA results in black; Pairwise MA results in grey."
    } else {
      slgd <- "NMA results in black; Pairwise MA results in grey. 95% CI presented as error bars."
    }
    
    srpinfo <- " Ranks shown along the diagonal are the SUCRA values."
    
    par(mfcol = c(ntx, 1), oma = c(0, 0, 2, 0), new = TRUE)
    par(mfg = c(ntx, 1), mar = c(0, 0, 0, 0))
    plot(0:19, seq(0, 10, len = 20), type = "n", axes = FALSE, ylab = "", xlab = "")
    text(-0.5, key.ypos, sprintf("Key:\n      %s\n      %s%s\n      %s", slgd, sp.order, srpinfo, sp.only), adj = 0, cex = 1)
  }
}