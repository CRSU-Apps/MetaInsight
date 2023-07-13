#------------------------------------------
#Project: NMA - Plot functions
#Author: Sze Huey TAN
#Supervisors: Alex, Nicola & Sylwia
#Last Modified: 20 November 2013
#------------------------------------------

#--------------------------------------------------------------
# Modified by Rhiannon Owen for Network Meta-Analysis Shiny App
# July 2017
#--------------------------------------------------------------



#----------------------------------------------------------------------------
# Functions for Summary Forest Plot Matrix & Table
#----------------------------------------------------------------------------

# Setup plot
create_summary_forest_plot <- function(data_to_plot, plot_title, axis_type) {
  # Network meta-analysis
  net1 <- netmeta(TE = data_to_plot$d0$TE, seTE = data_to_plot$d0$seTE,
                  treat1 = data_to_plot$d0$treat1, treat2 = data_to_plot$d0$treat2,
                  studlab = data_to_plot$d0$Study,
                  data=NULL, subset=NULL,
                  sm = "OR", level=0.95, level.comb=0.95,
                  comb.fixed=TRUE, comb.random=TRUE, reference.group="",
                  all.treatments=NULL, seq=NULL, tau.preset=NULL,
                  tol.multiarm = 0.0005, details.chkmultiarm = FALSE,
                  title="", warn=TRUE)
  
  rank <- netrank(net1, small.values="good")
  
  ntx <- data_to_plot$ntx
  lstx <- data_to_plot$lstx
  
  ma <- list()
  mtc <- list()
  
  # Update data in ma and mtc for plots
  ma$lor <- matrix(0, nrow = sum(1:(ntx-1)), ncol = 7)
  ma$or <- matrix(0, nrow = sum(1:(ntx-1)), ncol = 7)
  ma$predint <- matrix(0, nrow = sum(1:(ntx-1)), ncol = 7)
  mtc$lor <- matrix(0, nrow = sum(1:(ntx-1)), ncol = 7)
  mtc$or <- matrix(0, nrow = sum(1:(ntx-1)), ncol = 7)
  mtc$rank <- matrix(0, nrow = ntx, ncol = 4)
  mtc$pbest <- matrix(0, nrow = ntx, ncol = 2)
  mtc$predint <- matrix(0, nrow = sum(1:(ntx-1)), ncol = 4)
  mtc$rkgram <- matrix(0, nrow = sum(1:ntx*ntx), ncol = 2)
  
  mtc$type <- axis_type
  
  mtc$rank[,3] <- order(-rank$Pscore.random)
  
  count <- 1
  for (i in 1:(ntx-1)) {
    for (j in (i+1):ntx) {
      ma$or[count,5] <- exp(net1$lower.direct.random[i,j])
      ma$or[count,6] <- exp(net1$TE.direct.random[i,j])
      ma$or[count,7] <- exp(net1$upper.direct.random[i,j])
      
      ma$lor[count,5] <- net1$lower.direct.random[i,j]
      ma$lor[count,6] <- net1$TE.direct.random[i,j]
      ma$lor[count,7] <- net1$upper.direct.random[i,j]
      
      mtc$or[count,2] <- exp(net1$lower.random[i,j])
      mtc$or[count,3] <- exp(net1$TE.random[i,j])
      mtc$or[count,4] <- exp(net1$upper.random[i,j])
      
      mtc$lor[count,2] <- net1$lower.random[i,j]
      mtc$lor[count,3] <- net1$TE.random[i,j]
      mtc$lor[count,4] <- net1$upper.random[i,j]
      
      count <- count + 1
    }
  }
  
  mtcMatrixCont(plot_title, ntx, lstx, mtc, ma, bpredd=TRUE, bkey=TRUE, rpinfo=1, p.order=1, tx1=2, p.only=ntx)
}

#MTC & MA estimates for Forest Matrix plot - used in for loop in multiplot function
singleest <- function(mtc, pw, xpos=0, ucex) {
  
  #define pos to be the same
  ypos <- 0
  
  #NMA
  text(xpos,(ypos+2),sprintf("%.2f", mtc[2]),adj=0.5,cex=ucex+0.1,col="black")
  text(xpos,(ypos+1),sprintf("(%.2f to %.2f)", mtc[1], mtc[3]),adj=0.5,cex=ucex+0.1,col="black")
  #PW
  text(xpos,(ypos-1),sprintf("%.2f", pw[2]),adj=0.5,cex=ucex+0.1,col="grey55")
  if (!is.na(pw[2])) text(xpos,(ypos-2),sprintf("(%.2f to %.2f)",  pw[1], pw[3]),adj=0.5,cex=ucex+0.1,col="grey55")
}


#----------------------------------------------------------------------------
#MTC & MA Forest plot for use in NMA SPF Matix plot

#Function to draw two-tiered error bars for summary relative estimates
PrICrI<- function(offs, LL, OR, UL, ypos, ucol="black", ulwd=1, pcI=FALSE, predbd=c(NA,NA)) {
  if (pcI==TRUE) {   #show both CrI & PrI
    if (predbd[1]!=0 & predbd[2]!=0){
      lines(c(predbd[1]+offs,predbd[2]+offs),c(ypos,ypos),lty=1,lwd=ulwd,col=ucol)  		#pred dist interval
      lines(c(predbd[1]+offs,predbd[1]+offs),c(ypos-0.2,ypos+0.2),lty=1,lwd=ulwd,col=ucol)  	#pred dist 25% line
      lines(c(predbd[2]+offs,predbd[2]+offs),c(ypos-0.2,ypos+0.2),lty=1,lwd=ulwd,col=ucol)  	#pred dist 75% line
    }else { lines(c(LL+offs,UL+offs),c(ypos,ypos),col=ucol,lty=1,lwd=ulwd, adj=0.5) }
    lines(c(LL+offs,LL+offs),c(ypos-0.4,ypos+0.4),lty=1,lwd=ulwd,col=ucol)  	#credible interval 25% line
    lines(c(UL+offs,UL+offs),c(ypos-0.4,ypos+0.4),lty=1,lwd=ulwd,col=ucol)  	#credible interval 75% line
  }else {  ##pcI==FALSE ==> show only CrI
    lines(c(LL+offs,UL+offs),c(ypos,ypos),col=ucol,lty=1,lwd=ulwd, adj=0.5)
  }
  points(OR+offs,(ypos),pch=15,cex=0.8*ulwd,col=ucol, adj=0.5)
}

#----------------------------------------------------------------------------
#single plot for MTC & MA SFP in NMA SPF Matrix 
#Call functions: (i) PrICrI 
singleSFP <- function(mtc, pw, bpredd=TRUE, baxis=TRUE, scaletype, vec_xlim) {
  
  ##define axis offset
  offs=0
  
  #Add reference line; log(1) = 0
  lines(c(log(1)+offs,log(1)+offs),c(-3,3),lty=1, col="grey80")
  
  #define pos to be the same, so that the two SFP line are plotted at y=(-0.5 & 0.5)
  pos <- 1
  if (bpredd==FALSE) {
    #NMA
    PrICrI(offs, mtc[1],mtc[2],mtc[3], pos, ulwd=1.5)
    #PW
    PrICrI(offs, pw[1],pw[2],pw[3], pos-2, ucol="grey55", ulwd=1.5)
  } else {
    #NMA
    PrICrI(offs, mtc[1],mtc[2],mtc[3], pos, ulwd=1.5, pcI=TRUE, predbd=c(mtc[4], mtc[5]))		#summary w PrI
    #PW
    PrICrI(offs, pw[1],pw[2],pw[3], pos-2, ucol="grey55", ulwd=1.5, pcI=TRUE, predbd=c(pw[4], pw[5]))		#summary  w PrI
  }
  
  #Add axis for last row
  if (baxis) {
    if (scaletype == "log") { # Log data scale
      vticks <- c(1/1024, 1/256,1/64,1/16,1/4,1,4,16,64,256, 1024)
      lnticks <- log(vticks)
      lblticks <- c("1/1024", "1/256","1/64","1/16","1/4","1","4","16","64","256", "1024")
      axis(1, at=lnticks, labels=lblticks, cex.axis=0.6, padj=-1.0, tck=-0.05)
    }else { # Linear data scale
      axis(1, at = NULL, labels = TRUE, cex.axis=0.6,padj=-1.0, tck=-0.05)
    }
  }
}


#----------------------------------------------------------------------------
#Function to draw graphs along diagonal - for NMA SPF Matrix
#Comprise 4 graphs types:

rankogram <- function(ntx, rkgram, cumu=FALSE) {
  ori.ntx <- length(rkgram)/ntx
  xseq <- seq(0,1, length.out=(2*ntx+1))
  rankmat <- array(rkgram, c(ori.ntx,ntx))
  if (cumu==TRUE) rank.cumprob <- apply(rankmat, 2, cumsum)   #2:indicates column to all apply to
  
  for (i in 1:ntx){
    #Note: x1 position different from pbestpie function
    
    if (cumu==TRUE) par(fig=c(xseq[2*i-1],xseq[2*i+1],(1-xseq[2*i+1]),(1-xseq[2*i])), new=TRUE, mar=c(1.2,1.5,0,0.6))
    else par(fig=c(xseq[2*i-1],xseq[2*i+1],(1-xseq[2*i+1]),(1.01-xseq[2*i])), new=TRUE, mar=c(1.2,1.5,0,0.6))
    plot(1:ori.ntx,seq(-1,1,len=ori.ntx), type="n",  ylab="",xlab="", ylim=c(0,1), axes=F)
    
    if (cumu==FALSE) lines(1:ori.ntx, rankmat[,i], lwd=1.8)
    else lines(c(1, c(1:c(ori.ntx - 1)) + 0.5, ori.ntx), rank.cumprob[c(1, 1:c(ori.ntx - 1), ori.ntx), i])
    
    xticks <- c(1:ori.ntx)
    xlblticks <- c(1, rep("",(ori.ntx-2)) ,ori.ntx)
    axis(1, at=xticks, labels=xlblticks,lwd=0.8, cex.axis=0.7, tck=-0.05, padj=-2, col='grey70', col.axis='grey70') #bottom axis
    yticks <- seq(0.0,1.0, by=0.5)
    axis(2, at=yticks, labels=yticks,lwd=0.8, cex.axis=0.7,las=2,tck=-0.06, hadj=0.2, line=-0.2, col='grey70', col.axis='grey70')  #left axis
  }
}

rankjar <- function(ntx, ori.ntx, rank) {
  xseq <- seq(0,1, length.out=(2*ntx+1))
  for (i in 1:ntx){
    #Note: x1 & x2 positions different from pbestpie function
    par(fig=c(xseq[2*i-1],xseq[2*i],(1-xseq[2*i+1]),(1-xseq[2*i])), new=TRUE, mar=c(0.8,1.5,0,1.5))
    plot(0:ntx,0:ntx, type="n",  ylab="",xlab="", xlim=c(-1,1), ylim=c(ori.ntx,1), axes=F)
    rect(xleft=-1, ybottom=ori.ntx, xright=1, ytop=rank[i], col ="grey", border = NA) 
    lines(c(-1,1), c(rank[i],rank[i]), lwd=2)    
    rect(xleft=-1, ybottom=0, xright=1, ytop=ori.ntx, col =NA, border = NULL)
    axis(2, at=c(1,ori.ntx), labels=c(1,ori.ntx), cex.axis=0.8, las=2,tck=-0.08, hadj=-0.8, line = NULL)  #left axis
  }
}

pbestpie <- function(ntx, pbest) {
  xseq <- seq(0,1, length.out=(2*ntx+1))
  pbestmat <- cbind(pbest, (1-pbest))
  for (i in 1:ntx){
    par(fig=c(xseq[2*i],xseq[2*i+1],(1-xseq[2*i+1]),(1-xseq[2*i])), new=TRUE)
    pie(pbestmat[i,], labels = NA, edges = 200, radius = 0.9,
        clockwise = TRUE, init.angle = 90,
        density = NULL, col = c('grey','white'), border = NULL,
        lty = NULL, main = NULL)
  }
}

#----------------------------------------------------------------------------
#Function for computing SUCRA percentage estimates
SUCRA.perc <- function(ntx, rkgram) {
  rankmat <- array(rkgram, c(ntx,ntx))
  rank.cumprob <- apply(rankmat, 2, cumsum)   #2:indicates column to all apply to
  rank.sumcumprob <- apply(rank.cumprob[1:(ntx-1),], 2, sum)   #2:indicates column to all apply to
  SUCRA.perc <- (rank.sumcumprob/(ntx-1))*100
}

#----------------------------------------------------------------------------
#Function to create a vector indexing shading in mtcMultiplot
shading.vec <- function(ntx) {
  
  ordvec <- seq(1, ntx*ntx)
  shgvec <- rep(0, ntx*ntx)
  
  #Odd Number interventions
  shgvec[(ntx%%2!=0) & ordvec%%2!=0] <- 1 
  
  #Even Number interventions
  shgvec[(ntx%%2==0) & ((ceiling(ordvec/ntx))%%2==0) & (ordvec%%2==0)] <- 1 
  shgvec[(ntx%%2==0) & ((ceiling(ordvec/ntx))%%2!=0) & (ordvec%%2!=0)] <- 1 
  
  return(shgvec)
}


#----------------------------------------------------------------------------
#Call 5 functions above: (i)singleest (ii)singleSFP (iii) (iv)pbestpie (v) (vi)shading.vec
multiplot <- function(stytitle, ntx, lstx, mtc, ma, bpredd=TRUE, add.info=1, plt.adj, ucex) {
  
  #Start a matrix plot - define number of elements "squares" in Matrix
  tplot <- ntx*ntx
  #dev.new(width=10, height=10)  
  
  if(plt.adj==0) { par(mfcol = c(ntx,ntx), oma = c(3.5, 0, 2, 0)) 
  } else if(plt.adj==1){ par(mfcol = c(ntx,ntx), oma = c(5, 0, 2, 0))
  } else if(plt.adj==2){ par(mfcol = c(ntx,ntx), oma = c(6, 0, 2, 0))  }
  
  
  #define cex - text size
  ucex <- 1.1*ucex
  
  #create the vector indexing shading for use later in function
  shgvector <- shading.vec(ntx)
  
  #determine the plot (x-axis) range based on the MTC & MA results and midpoint (xpos) for printing text (SFP estimates, etc.)
  # symref=TRUE indicates that reference line must be at mid-point of plot, while symref=FALSE allows reference line to be data-driven but definitely on the plot.
  symref<-FALSE     #symref<-TRUE
  
  if (symref==FALSE) {
    if (bpredd==FALSE) {
      #Check the maximum required print range using lor range
      side.xl <- min(0,floor(min(ma$lor[,5],mtc$lor[,2], na.rm = TRUE)), na.rm = TRUE)
      side.xu <- max(0,ceiling(max(ma$lor[,7],mtc$lor[,4], na.rm = TRUE)), na.rm = TRUE)
    } else {
      #Check the maximum required print range using predictive interval range
      side.xl <- min(0,floor(min(ma$lor[,5],mtc$lor[,2],ma$predint[,5],mtc$predint[,2], na.rm = TRUE)), na.rm = TRUE)
      side.xu <- max(0,ceiling(max(ma$lor[,7],mtc$lor[,4], ma$predint[,7],mtc$predint[,4], na.rm = TRUE)), na.rm = TRUE)
    }
    xpos <- (side.xl + side.xu)/2
  } else {   #symref==TRUE
    xpos <- 0
    if (bpredd==FALSE) {
      #Check the maximum required print range using lor range
      absside <- max(abs(ma$lor[,5:7]),abs(mtc$lor[,2:4]), na.rm = TRUE)
      side.xl <- -1*ceiling(absside) 
      side.xu <- ceiling(absside) 
    }else{
      #Check the maximum required print range using predictive interval range
      absside <- max(abs(ma$predint[,5:7]), abs(mtc$predint[,2:4]), na.rm = TRUE)
      side.xl <- -1*ceiling(absside) 
      side.xu <- ceiling(absside) 
    }
  }
  
  i.pt <- 0
  i.tx <- 0
  for (i in 1:tplot){
    
    par(mar=c(0.3,0.25,0.2,0.25))
    plot(1:10,seq(-3,3,len=10), type="n",axes=F, ylab="",xlab="",xlim=c(side.xl,side.xu))
    
    #matrix cells alternate background shading
    if (shgvector[i]==1) rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col ="grey95") 
    else rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col ="white")
    
    i.pt <- i.pt+1
    if (i%%(ntx+1) == 1) { 
      ## Diagonal
      if (i%%ntx!=0){
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col ="white")  
        if (add.info==0) text(xpos,0,lstx[i%%ntx], cex=ucex, adj=c(0.5,0.5),font=2)
        else text(xpos,2.0,lstx[i%%ntx], cex=ucex, adj=c(0.5,0.5),font=2)
        if (add.info==1) text(xpos+0.6,-0.1,sprintf("Rank=%.0f",mtc$rank[i%%ntx, 3]), cex=ucex*0.9, adj=0.5,font=1)
        if (add.info==2) text(xpos+1.2,-1.4,sprintf("Mean Rank\n=%.2f",mtc$rank[i%%ntx, 1]), cex=ucex*0.9, adj=0.5,font=1)
        if (add.info==3) text(xpos, 0.45,sprintf("SUCRA=%.0f%%",mtc$sucra[i%%ntx]), cex=ucex*0.8, adj=0.5,font=1)
        if (add.info==4) text((xpos+side.xl)/2,-1.4,sprintf("   Pbest\n   =%.2f",mtc$pbest[i%%ntx, 1]), cex=ucex*0.9, adj=0.5,font=1)
      }
      else { #last box
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col ="white")  
        if (add.info==0) text(xpos,0,lstx[ntx], cex=ucex, adj=c(0.5,0),font=2)   #NOTE difference here for lstx!!
        else text(xpos,2.0,lstx[ntx], cex=ucex, adj=c(0.5,0),font=2)   #NOTE difference here for lstx!!
        if (add.info==1) text(xpos+0.6,-0.1,sprintf("Rank=%.0f",mtc$rank[ntx,3]), cex=ucex*0.9, adj=0.5,font=1)
        if (add.info==2) text(xpos+1.2,-1.4,sprintf("Mean Rank\n=%.2f",mtc$rank[ntx,1]), cex=ucex*0.9, adj=0.5,font=1)
        if (add.info==3) text(xpos,0.45,sprintf("SUCRA=%.0f%%",mtc$sucra[ntx]), cex=ucex*0.8, adj=0.5,font=1)
        if (add.info==4) text((xpos+side.xl)/2,-1.4,sprintf("   Pbest\n   =%.2f",mtc$pbest[ntx,1]), cex=ucex*0.9, adj=0.5,font=1)
      }
      i.pt<- i.pt-1
      
    } else if (i %% ntx == 0 | i %% ntx > i%/%ntx) { 
      ##Lower triangle
      if (i%%ntx == 0) axis <- TRUE  else axis <- FALSE
      
      if (bpredd==FALSE) {
        mtc.est <- c(mtc$lor[i.pt,2],mtc$lor[i.pt,3],mtc$lor[i.pt,4])
        pw.est <- c(ma$lor[i.pt,5],ma$lor[i.pt,6],ma$lor[i.pt,7])
      }else {
        mtc.est <- c(mtc$lor[i.pt,2],mtc$lor[i.pt,3],mtc$lor[i.pt,4],mtc$predint[i.pt,2],mtc$predint[i.pt,4])
        pw.est <- c(ma$lor[i.pt,5],ma$lor[i.pt,6],ma$lor[i.pt,7],ma$predint[i.pt,5],ma$predint[i.pt,7])
      }
      singleSFP(mtc.est, pw.est, bpredd, axis, mtc$type, vec_xlim=c(side.xl,side.xu))
      
      
    }else if (i %% ntx <= i%/%ntx) {			
      ##Upper triangle
      i.pt<- i.pt-1
      i.tx <- ((i%%ntx)-1)*ntx + (i%/%ntx+1) - (sum(seq((i%%ntx)))) 
      mtc.or <- c(mtc$or[i.tx,2],mtc$or[i.tx,3],mtc$or[i.tx,4])
      pw.or <- c(ma$or[i.tx,5],ma$or[i.tx,6],ma$or[i.tx,7])
      
      singleest(mtc.or, pw.or, xpos, ucex)
    }
    
    ##Draw a box around the mulitple plots
    if (shgvector[i]==1) box(lty = 1, col = 'grey85') else box(lty = 1, col = 'grey85')
    
  }
  #Identify original number of tx in analysis - for use in rankjar function
  ori.ntx <- length(mtc$rkgram[,1])/ntx
  
  #Draw graphs along the diagonal
  if (add.info==1) rankogram(ntx, mtc$rkgram[,1])
  if (add.info==2) rankjar(ntx,ori.ntx, mtc$rank[,1])
  if (add.info==3) rankogram(ntx, mtc$rkgram[,1], cumu=TRUE)
  if (add.info==4) pbestpie(ntx, mtc$pbest[,1])
  
  #Insert the graph title and x-axis title
  title(main=stytitle, outer = TRUE , cex.main=1.5)
  if (!bpredd) {
    if (mtc$type == 'OR') {
      mtext("Odds Ratio with 95% CI  (log scale)", side=1, outer=TRUE, line=2, cex=0.75)
    } else if (mtc$type == "RR") {
      mtext("Hazard Ratio with 95% CrI  (log scale)", side=1, outer=TRUE, line=2, cex=0.75)
    } else {
      mtext("Mean difference with 95% CI", side=1, outer=TRUE, line=2, cex=0.75)
    }
  } else {
    if (mtc$type == 'OR') {
      straxis <- "Odds Ratio with 95% CI (log scale)"
    } else if (mtc$type == 'RR') {
      straxis <- "Hazard Ratio with 95% CrI & 95% PI (log scale)"
    } else {
      straxis <- "Mean Difference with 95% CI & 95% PI"
    }
    
    if (plt.adj == 0) {
      mtext(straxis, side=1, outer=TRUE, line=2, cex=0.75)
    } else {
      mtext(straxis, side=1, outer=TRUE, line=1.5, cex=0.75)
    }
  }
}

#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
#Plot function for mtcMatrixCont

#Function to create the matrix sorting order - to be used for the MTC & MA numerical results (Upper triangle results)
sortres.matrix <-function(ntx, po) {
  
  #Correctly create corresponding treatment code no.
  txcode <- c(1:ntx)
  #Sorted tx code list
  st.txcode <- txcode[po]  #gives same results as po but doing this to be cautious
  
  #create new 'ranked' tx combinations
  # require install.packages("combinat")
  cm <- t(combn(st.txcode, 2))  #combination matrix of ntx choose 2
  
  #New 'ranked' tx combinations matrix made up of c("ordering", "t1", "t2", "inversion number")
  mtnew <- cbind(1:(choose(ntx, 2)), cm, (ifelse(cm[,1]<cm[,2], 1 ,-1)) )
  
  mtnew[(cm[,1]>cm[,2]), c(1,2,3,4)] <- mtnew[(cm[,1]>cm[,2]), c(1,3,2,4)]
  
  #ordering sorted by t1(ref) followed by t2(comparator)
  mo <- order(mtnew[,2],mtnew[,3] ,decreasing = FALSE)
  #All columns to be sorted by mo order
  mtorg <- mtnew[mo,]   #Matrix having some format ordering as the standarded WinsBUGS output
  
  return(mtorg)
}

#Function to create the rankgram matrix sorting order
sortrkg.ord <-function(ntx, po) {
  
  #Correctly create corresponding treatment code no.
  txcode <- c(1:ntx)
  #Sorted tx code list
  st.txcode <- txcode[po]  #gives same results as po but doing this to be cautious
  
  #create new 'ranked' rankogram combinations
  rkgnew <- array(c(1:(ntx*ntx), rep(st.txcode, each = ntx, len = ntx*ntx), rep(1:ntx, ntx, len = ntx*ntx)), c(ntx*ntx,3))
  
  rkgmio <- order(rkgnew[,2],rkgnew[,3] ,decreasing = FALSE) #intermediate ordering 
  rkgmtorg <-cbind(c(1:(ntx*ntx)), rkgnew[rkgmio,])  #matrix from WinBUGS; col 1 for checking purpose only
  rkgmo <- order(rkgmtorg[,2] ,decreasing = FALSE)
  
  return(rkgmo)
}

#Function to update the PW MA results after changes to the tx rankings.
ma.sortres <-function(ma, mtorg, mtc) {
  #ma data frame contains: $lor; $or; $predint
  
  #Re-calculate estimates after inverting the reference group, using mtorg[,4=inv]
  tmp.lor <- mtorg[,4]*(ma$lor[, 4:7])
  tmp.or <- mtorg[,4]*(ma$or[, 4:7])
  tmp.predint <- mtorg[,4]*(ma$predint[, 4:7])
  
  #swap 25% & 75% estimates for those that we inverted the reference group
  tmp.lor[(mtorg[,4]==-1), c(1,2,3,4)] <- tmp.lor[(mtorg[,4]==-1), c(1,4,3,2)]
  tmp.or[(mtorg[,4]==-1), c(1,2,3,4)] <- tmp.or[(mtorg[,4]==-1), c(1,4,3,2)]
  tmp.predint[(mtorg[,4]==-1), c(1,2,3,4)] <- tmp.predint[(mtorg[,4]==-1), c(1,4,3,2)]
  
  tmp.lor <- cbind(ma$lor[,1:3] , tmp.lor)
  tmp.or <- cbind(ma$or[,1:3] , tmp.or)
  tmp.predint <- cbind(ma$predint[,1:3] , tmp.predint)
  
  #find order of final matrix for plotting
  mtord <- order(mtorg[,1])  #gives same results as mo
  
  new.lor <- tmp.lor[mtord,]
  new.or <- tmp.or[mtord,]
  new.predint <- tmp.predint[mtord,]
  
  newma <- list(lor=new.lor, or=new.or, predint=new.predint, type=mtc$type)
  return(newma)
}

#Function to update the MTC results after changes to the tx rankings.
mtc.sortres <-function(mtc, mtorg, rkgmo, po) {
  #mtc data frame contains: $lor; $or; $predint; $rkgram (MATRIX)
  #$rkgram (MATRIX)
  #$rank; $pbest; $p2best; $p3best; $sucra (VECTOR, length(ntx))
  #$tau (VECTOR, 1 row x 4element)
  
  #~VECTORS~
  new.rank <- mtc$rank[po,]
  new.pbest <- mtc$pbest[po,]
  new.p2best <- mtc$p2best[po,]
  new.p3best <- mtc$p3best[po,]
  if (exists("sucra", where=mtc)) new.sucra <- mtc$sucra[po] else new.sucra <- c(0)
  
  #~MATRIX~
  new.rkgram <- mtc$rkgram[rkgmo,]
  
  #Re-calculate estimates after inverting the reference group, using mtorg[,4=inv]
  tmp.lor <- mtorg[,4]*(mtc$lor[, 1:4])
  tmp.or <- mtorg[,4]*(mtc$or[, 1:4])
  tmp.predint <- mtorg[,4]*(mtc$predint[, 1:4])
  
  #swap 25% & 75% estimates for those that we inverted the reference group
  tmp.lor[(mtorg[,4]==-1), c(1,2,3,4)] <- tmp.lor[(mtorg[,4]==-1), c(1,4,3,2)]
  tmp.or[(mtorg[,4]==-1), c(1,2,3,4)] <- tmp.or[(mtorg[,4]==-1), c(1,4,3,2)]
  tmp.predint[(mtorg[,4]==-1), c(1,2,3,4)] <- tmp.predint[(mtorg[,4]==-1), c(1,4,3,2)]
  
  #find order of final matrix for plotting
  mtord <- order(mtorg[,1])  #Note: does not give same results as mo!
  
  new.lor <- tmp.lor[mtord,]
  new.or <- tmp.or[mtord,]
  new.predint <- tmp.predint[mtord,]
  
  newmtc <- list(lor=new.lor, or=new.or, predint=new.predint, rkgram=new.rkgram, rank=new.rank, pbest=new.pbest, p2best=new.p2best, p3best=new.p3best, sucra=new.sucra, tau=mtc$tau, type=mtc$type)
  return(newmtc)
}

#Function to create the matrix reduction vector
redu.matrix <- function(ntx, po, p.only, mtorg) {
  
  #Correctly create corresponding treatment code no.
  txcode <- c(1:ntx)
  #Sorted tx code list
  st.txcode <- txcode[po[1:p.only]]  #gives same results as po but doing this to be cautious
  
  r.mtorg <- mtorg[order(mtorg[,1]),]
  rmt <- (match(r.mtorg[,2],st.txcode)) +(match(r.mtorg[,3],st.txcode))
  
  return(rmt)
}

#Function to reduce the matix size based on user defined plotting range reduction
ma.redu <-function(ma, rmt) {
  #ma data frame contains: $lor; $or; $predint
  
  new.lor <- ma$lor[!is.na(rmt),]
  new.or <-  ma$or[!is.na(rmt),]
  new.predint <-  ma$predint[!is.na(rmt),]
  
  newma <- list(lor=new.lor, or=new.or, predint=new.predint, type=ma$type)
  return(newma)
}

#Function to reduce the matix size based on user defined plotting range reduction
mtc.redu <-function(mtc, rmt, p.only, po) {
  #mtc data frame contains: $lor; $or; $predint; $rkgram (MATRIX)
  #$rank; $pbest; $p2best; $p3best; $sucra (VECTOR, length(ntx))
  #$tau (VECTOR, 1 row x 4element)
  
  #~VECTORS~ inputed mtc===st.mtc # already sorted, just truncate directly
  new.rank <- mtc$rank[1:p.only,]
  new.pbest <- mtc$pbest[1:p.only,]
  new.p2best <- mtc$p2best[1:p.only,]
  new.p3best <- mtc$p3best[1:p.only,]
  if (exists("sucra", where=mtc)) new.sucra <- mtc$sucra[1:p.only] 
  
  #~MATRIX~
  new.lor <- mtc$lor[!is.na(rmt),]
  new.or <-  mtc$or[!is.na(rmt),]
  new.predint <-  mtc$predint[!is.na(rmt),]
  new.rkgram <- mtc$rkgram[1:(ntx*p.only),]
  
  newmtc <- list(lor=new.lor, or=new.or, predint=new.predint, rkgram=new.rkgram, rank=new.rank, pbest=new.pbest, p2best=new.p2best, p3best=new.p3best, sucra=new.sucra, tau=mtc$tau, type=mtc$type)
  return(newmtc)
}

#----------------------------------------------------------------------------
sort.ord <- function(mtc, p.order, tx1) {
  
  #Define parameters for sorting use
  mdrank <-mtc$rank[,3]
  mnrank <-mtc$rank[,1]
  pbest <-mtc$pbest[,1]
  
  if(p.order==1) { #sort by: median rank order (increasing)
    sp.order <- "Interventions are displayed sorted by median rank."
    #Parameters for sorting use in this case: mdrank; mnrank; pbest 
    
    if (tx1==1) {
      # sort data but leave tx1 at the top
      #sorting parameter for sorting by median rank in increasing order. Highest rank first
      po <- c(1, (1+order(mdrank[-1], mnrank[-1], pbest[-1],decreasing = FALSE)))
    }else if (tx1==2) {
      # sort all results by median rank 
      #sorting parameter for sorting by median rank in increasing order. Highest rank first
      po <- order(mdrank, decreasing = FALSE)
    }
    
  } else if(p.order==2) { #sort by: mean rank order (increasing)
    
    sp.order <- "Interventions are displayed sorted by mean rank."
    #Parameters for sorting use in this case: mnrank; pbest 
    
    if (tx1==1) {
      # sort data but leave tx1 at the top
      #sorting parameter for sorting by mean rank in increasing order. Highest rank first
      po <- c(1, (1+order(mnrank[-1], pbest[-1],decreasing = FALSE)))
    }else if (tx1==2) {
      # sort all results by median rank 
      #sorting parameter for sorting by mean rank in increasing order. Highest rank first
      po <- order(mnrank, pbest, decreasing = FALSE)
    }
    
  } else if(p.order==3) { #sort by: SUCRA percentage (decreasing)
    
    sp.order <- "Interventions are displayed sorted by SUCRA percentages."
    #Parameters for sorting use in this case: sucra.percent; mnrank
    sucra.percent <-mtc$sucra
    
    if (tx1==1) {
      # sort data but leave tx1 at the top
      #sorting parameter for sorting by sucra in decreasing order. Highest first
      po <- c(1, (1+order(sucra.percent[-1], mnrank[-1] ,decreasing = TRUE)))
    }else if (tx1==2) {
      # sort all results by sucra
      #sorting parameter for sorting by sucra in decreasing order. Highest first
      po <- order(sucra.percent, mnrank ,decreasing = TRUE)
    }
    
    
  } else if(p.order==4) { #sort by: pbest order (decreasing)
    sp.order <- "Interventions are displayed sorted by probability best results."
    #Parameters for sorting use in this case: pbest; mnrank 
    
    if (tx1==1) {
      # sort data but leave tx1 at the top
      #sorting parameter for sorting by pbest in decreasing order. Highest first
      po <- c(1, (1+order(pbest[-1],mnrank[-1] ,decreasing = TRUE)))
    }else if (tx1==2) {
      # sort all results by pbest
      #sorting parameter for sorting by pbest in decreasing order. Highest first
      po <- order(pbest,mnrank ,decreasing = TRUE)
    }
    
    
  } else if(p.order==5) { #sort by: treatment relative effect vs trt 1 in list (should be usual care/placebo)
    sp.order <- sprintf("Interventions are displayed sorted by magnitude of relative effect vs %s.", lstx[1])
    
    #Define parameter for sorting use, this is not shared by any of the above sorting order choices
    txeff <-c(1, mtc$or[1:(ntx-1),3])
    
    if (tx1==1) {
      # sort data but leave tx1 at the top
      #sorting parameter 
      po <- c(1, (1+order(txeff[-1] ,decreasing = TRUE)))
    }else if (tx1==2) {
      # sort all results by  treatment relative effect vs trt 1 in list
      #sorting parameter. Largest relative effect first
      po <- order(txeff ,decreasing = TRUE)
    }
  }
  return(list(sp.order=sp.order, po=po))
}

#----------------------------------------------------------------------------
#Call 8 functions above: (i)multiplot (ii)sortrkg.ord (iii)sortres.matrix
#(iv)ma.sortres (v)mtc.sortres (vi)redu.matrix (vii)ma.redu (viii)mtc.redu (ix) SUCRA.perc
mtcMatrixCont <- function(stytitle, ntx, lstx, mtc, ma, bpredd=TRUE, bkey=TRUE, rpinfo=1, p.order=0, tx1=1, p.only=ntx, ucex=1) {
  
  #Additional MTC component selection
  #rpinfo=0 #default
  #1: rankogram with median rank; 2: Jar/Bar plot with mean rank
  #3: cum-rankogram with sucra percentage; 4:Pie chart with probability best
  if (rpinfo<0 | rpinfo>4) stop("rpinfo must be an integer between 0 and 4 inclusive")
  
  #matrix printing order
  #p.order=0 #default - follow treatment list order
  #1: median rank order (increasing); 2:mean rank order (increasing)
  #3: SUCRA percentage (decreasing)
  #4: pbest order (decreasing); 5:treatment relative effect vs trt 1 in list (should be usual care/placebo) (decreasing)
  if (p.order<0 | p.order>5) stop("p.order must be an integer between 0 and 5 inclusive")
  
  #tx1 (Usual care/placebo) to be presented at the top or at its 'true' ranking position
  #tx1=1 #default - at the top
  #tx1=2: sorted position
  if (tx1<1 | tx1>2) stop("tx1 must be either 1 or 2!")
  
  #print only the first x interventions
  #p.only = ntx # default
  #p.only = integer (3:ntx)
  if (p.only<3) stop("Print selection must not be less than 3!")
  if (p.only>ntx) stop("Print selection cannot be more than the total number of interventions!")
  
  #Parameter for adjusting the plot area for the matrix in the graph
  #plt.adj = 0 when graph key is to be removed
  # = 1 for only 2 lines of keys (i.e. when p.order=0)
  # = 2 for 3 lines of keys (i.e when p.order!=0 && p.only <ntx)
  if (bkey==FALSE) { plt.adj <- 0 
  }else { if (p.order==0) { plt.adj <- 1 } else { if(p.only<ntx) plt.adj <- 2 else plt.adj <- 1 }
  }
  
  #Check if SUCRA percentage is required for presentation and 
  #calculate SUCRA if using SUCRA cumulative probability plot for display
  if (rpinfo==3) mtc$sucra <- SUCRA.perc(ntx, mtc$rkgram[,1])
  
  if (p.order==0) {
    multiplot(stytitle, ntx, lstx, mtc, ma, bpredd, rpinfo, plt.adj, ucex)
    
    sp.order <- "Interventions are displayed in the order that they were entered in the analysis."
    sp.only <- ""
    key.ypos <- 1.3
    
    #tx1 -NA-
    #p.only NA
    
  } else {
    
    sort.vec <- sort.ord(mtc, p.order, tx1) 
    sp.order <- sort.vec$sp.order
    po <- sort.vec$po
    mtso <- sortres.matrix(ntx, po)
    rkgmo <- sortrkg.ord(ntx, po) 
    st.ma <- ma.sortres(ma, mtso, mtc)
    st.mtc <- mtc.sortres(mtc, mtso, rkgmo, po) 
    st.lstx <- lstx[po]
    
    if (p.only==ntx) {
      sp.only <- ""
      key.ypos <- 1.3
      
      multiplot(stytitle, ntx, st.lstx, st.mtc, st.ma, bpredd, rpinfo, plt.adj, ucex)
    } else {
      sp.only <- sprintf("A total of %i interventions were compared in this NMA but only %i interventions were displayed in this plot.", ntx, p.only)
      key.ypos <- 2.3
      
      #reduce the results matrices & vectors
      mtred <- redu.matrix(ntx, po, p.only, mtso)
      r.ma <- ma.redu(st.ma, mtred)
      r.mtc <- mtc.redu(st.mtc, mtred, p.only, po)
      r.lstx <- st.lstx[1:p.only]
      multiplot(stytitle, p.only, r.lstx, r.mtc, r.ma, bpredd, rpinfo, plt.adj, ucex)
    }
  }
  
  if (bkey==TRUE) {
    if (bpredd==FALSE) slgd <- "NMA results in black; Pairwise MA results in grey."
    else slgd <- "NMA results in black; Pairwise MA results in grey. 95% CI presented as error bars."
    
    srpinfo <- ""
    if(rpinfo==1) { srpinfo <- " Ranks shown along the diagonal are the median rank." 
    } else if(rpinfo==2) { srpinfo <- " Ranks shown along the diagonal are the mean rank." 
    } else if(rpinfo==3) { srpinfo <- " SUCRA refers to the surface under the cumulative ranking line." }
    
    par(mfcol = c(ntx,1), oma = c(0, 0, 2, 0), new=TRUE)
    par(mfg=c(ntx,1),mar = c(0, 0, 0, 0))
    plot(0:19,seq(0,10,len=20), type="n",axes=F, ylab="",xlab="")
    text(-0.5, key.ypos, sprintf("Key:\n      %s\n      %s%s\n      %s", slgd, sp.order, srpinfo, sp.only), adj=0, cex=1) 
  }
  
}


#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
#END OF FUNCTIONS CODES