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
# Functions to install necessary libraries
#----------------------------------------------------------------------------



#----------------------------------------------------------------------------
# SUCRA function codes from G.Salanti et. al. JCE article 
#----------------------------------------------------------------------------
"sucraplot.fun" = function(effectiveness, plotmfrow = c(3, 3))
{
	# effectiveness: the effectiveness matrix as DATABASE, each column is a treatment
	# plotmfrow is a vector of length two which defines the panels in the plot
	#Creates cumulative ranking curves extrapolated at the middle of each bar

	names <- names(effectiveness)
	nr.of.treat <- dim(effectiveness)[2]
	cumeffectiveness <- apply(effectiveness, 2, cumsum)
	par(mfrow = plotmfrow)
	for(i in 1:nr.of.treat) {
		plot(1:nr.of.treat, type = "none", ylim = c(0, 1), xlab = 
			paste("Rank of", as.character(names[i])), ylab = 
			"Cumulative Probability")
		#lines(stepfun(1:nr.of.treat, cumeffectiveness[, i]), lty = 2, col = 2, lwd = 2)
		lines(lwd = 2, c(1, c(1:c(nr.of.treat - 1)) + 0.5, nr.of.treat
			), cumeffectiveness[c(1, 1:c(nr.of.treat - 1), c(
			nr.of.treat - 1)), i])
	}
	
	for(i in 1:nr.of.treat) {
		plot(1:nr.of.treat, type = "none", ylim = c(0, 1), xlab = 
			paste("Rank of", as.character(names[i])), ylab = 
			"Probability")
		#lines(stepfun(1:nr.of.treat, cumeffectiveness[, i]), lty = 2, col = 2, lwd = 2)
		lines(lwd = 2, c(1, c(1:c(nr.of.treat - 1)) + 0.5, nr.of.treat
			), effectiveness[c(1, 1:c(nr.of.treat - 1), c(
			nr.of.treat - 1)), i])
	}
}


#----------------------------------------------------------------------------
# Function for SFP Median Rank Plot
#----------------------------------------------------------------------------
#Function to create traffic lights color wheel
traffic.color <- function(ntx){ 
  if(ntx%%2 != 0) red <- c(seq(0,1, length.out=median(1:ntx)), rep(1, (ntx-median(1:ntx))))  #odd number
  else red <- c(seq(0,1-(0.8/ntx), length.out=ntx/2), rep(1, (ntx-ntx/2)))  #even number

  green <- rev(red)
  clrlst <-NULL
  for (i in 1:ntx) {
    clrlst <- c(clrlst, rgb(red[i],green[i],0))
  }
  return(clrlst)
}

#Function that plots the bar rank chart
mtcRank <- function(stytitle, ntx, lstx, mtc, bcolr=FALSE) {
  
  #win.graph()
  #dev.new()
  par(mar=c(0.5,2,2,2))
  plot(0:20,seq(1,ntx,len=length(0:20)),type="n",axes=F ,ylab="",xlab="", xlim=c(0,21),ylim=c(-10*ntx,4))
  title(main=stytitle,cex=0.8)
  
  #Define the traffice colour list
  if(bcolr==TRUE) { clrlst <- traffic.color(ntx) 
  } else { 
    gy <- as.character(floor(seq(94, 54 ,length.out = ntx)) )
    clrlst <- paste ("grey",gy, sep = "" )  #to be changed to grey scale
  }
  
  strmdrk <- as.character(mtc$rank[,3])  #Need this step to prevent fault in the rankings
  mdrk <- as.integer(strmdrk)
  str.rank <- rep("", ntx)
  for (i in 1:ntx) str.rank[mdrk[i]] <- paste( str.rank[mdrk[i]], lstx[i], sep = "\n")
  v.rank <- substring(str.rank, 2)
  
  pbestplot <- round(mtc$pbest, digits=2)
  #str.best <- rep("", ntx)
  #for (i in 1:ntx) str.best[pbestplot[i]] <- paste( str.best[pbestplot[i]], lstx[i], sep = "\n")
  #v.best <- substring(str.best, 3)
  
  
  #Draw the rank chart
  #Heading for the rank chart
  rect(xleft=1, ybottom=0, xright=20, ytop=4, col = "NA")
  text(3, 2, "Rank", cex=1.15, adj=c(0.5,0.5))
  text(11, 2, "Intervention", cex=1.15, adj=c(0.5,0.5))
  text(18, 2, "P-score", cex=1.15, adj=c(0.5,0.5))
  for (i in 1:ntx) {
    rect(xleft=1, ybottom=-10*i, xright=20, ytop=-10*(i-1), col = clrlst[i])
    text(3,-10*i+5, i , cex=10/ntx, adj=c(0.5,0.5),font=1)
    text(11,-10*i+5, v.rank[i] , cex=8/ntx, adj=c(0.5,0.5),font=1)
    text(18,-10*i+5, pbestplot[i] , cex=8/ntx, adj=c(0.5,0.5),font=1)
  }
}



###############################################
#Function that plots the bar rank chart
mtcRank2 <- function(stytitle, ntx, rankl, prob,  bcolr=FALSE) {
  
  #win.graph()
  par(mar=c(0.5,2,2,2))
  plot(0:20,seq(1,ntx,len=length(0:20)),type="n",axes=F ,ylab="",xlab="", xlim=c(0,21),ylim=c(-10*ntx,4))
  title(main=stytitle,cex=0.8)
  
  #Define the traffice colour list
  if(bcolr==TRUE) { clrlst <- traffic.color(ntx) 
  } else { 
    gy <- as.character(floor(seq(94, 54 ,length.out = ntx)) )
    clrlst <- paste ("grey",gy, sep = "" )  #to be changed to grey scale
  }
  
  #strmdrk <- as.character(mtc$rank[,3])  #Need this step to prevent fault in the rankings  
  #mdrk <- as.integer(strmdrk)    #use the three lines below to replace these two lines (this one and above)
  
  #probn <- colnames(rankp)[apply(rankp,1,which.max)]
  #probnum <- substr(probn,2,2)  # remove all the 'V' in the column names
  #mdrk <- as.integer(probnum)
  # median rank graph
  probnumvec <- c()	
  for (i in 1:(ntx)) {
    probnum <- 0
    probsum <- 0
    for (j in 1:(ntx)) {
      if (probsum<0.5) {
        probsum<-probsum+prob[i,j]
        probnum <- probnum+1
      }
    }
    probnumvec[i] <- probnum
  }
  
  rankl <- rownames(prob)
  
  mdrk<-as.integer(probnumvec)
  
  str.rank <- rep("", ntx)
  for (i in 1:ntx) str.rank[mdrk[i]] <- paste( str.rank[mdrk[i]], rankl[i], sep = "\n")
  v.rank <- substring(str.rank, 2)
  
  #Draw the rank chart
  #Heading for the rank chart
  rect(xleft=1, ybottom=0, xright=20, ytop=4, col = "NA")
  text(3, 2, "Rank", cex=1.15, adj=c(0.5,0.5))
  text(11, 2, "Intervention", cex=1.15, adj=c(0.5,0.5))
  for (i in 1:ntx) {
    rect(xleft=1, ybottom=-10*i, xright=20, ytop=-10*(i-1), col = clrlst[i])
    text(3,-10*i+5, i , cex=ifelse(ntx<=20, 10/ntx, 0.5), adj=c(0.5,0.5),font=1)
    text(11,-10*i+5, v.rank[i] , cex=ifelse(ntx<=16, 8/ntx, 0.5), adj=c(0.5,0.5),font=1)
  }
}

#----------------------------------------------------------------------------
# Functions for Summary Forest Plot Matrix & Table
#----------------------------------------------------------------------------

#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
#Function used in tmpSFP to draw the slider bars for mean and median rankings
rankbars <- function(ntx, ori.ntx, mean=0, xpos, ptxpos, pos, rank, rcex) {
  
  if (mean==1) rankstr = "Mean Rank"
  if (mean==0) rankstr = "Median Rank" 
  if (mean==1) ranknumstr = "%.2f"
  if (mean==0) ranknumstr = "%.0f" 
  
  #vector for xpos in the left first column
  xbx1 <- seq(xpos[1],xpos[2], length=5)
  
  #ptxpos[1] represent the position of the first tx in the first box
  #pos[ori.ntx]-2 represent the bottom line in the first box
  #3 sections with 4 points:
  temp <- seq(ptxpos[1],(pos[ntx]-2), length=4)
  s.pt <- temp[3]
  
  #List the rankbar legend/explanation notes
  text(xbx1[3],(ptxpos[1]+s.pt)/2, rankstr, cex=rcex, adj=c(0.5,0.5),font=1)	# for primary comparator treatment
  #text(xbx1[2],s.pt,sprintf("%i ", ori.ntx), cex=0.9*rcex, adj=c(1,0.5),font=1, col="grey70")	
  text(xbx1[4],s.pt," 1", cex=0.9*rcex, adj=c(0,0.5),font=1, col="grey70")	
  text(xbx1[2],s.pt+0.45,"Bottom", cex=rcex, adj=c(0.5,1),font=1, col="grey70")	
  text(xbx1[4],s.pt+0.45,"Top", cex=rcex, adj=c(0.5,1),font=1, col="grey70")	
  
  #Draw the first tx rankbar
  lines(c(((xbx1[2]-xbx1[4])/(ori.ntx-1)*(rank[1]-1))+xbx1[4],((xbx1[2]-xbx1[4])/(ori.ntx-1)*(rank[1]-1))+xbx1[4]), 
        c(s.pt-0.3,s.pt+0.3), lwd=2)    
  rect(xleft=xbx1[2], ybottom=s.pt+0.3, xright=xbx1[4], ytop=s.pt-0.3, col =NA, border = NULL)
  text(((xbx1[2]-xbx1[4])/(ori.ntx-1)*(rank[1]-1))+xbx1[4], s.pt-0.45,sprintf(ranknumstr, rank[1]), cex=rcex, adj=c(0.5,0),font=1)	#State numerical estimates
  
  #Define column 2 boxes left and right x-coordinators
  width.rkbar <- xbx1[4]-xbx1[2]  #Use to standardise all the rankbars 
  xbx2r <- xpos[3] - 0.8   
  xbx2l <- xbx2r - width.rkbar
  #Set the y-pos for the mid point in each row 
  mid.pos <- pos + 0.5
  
  for (i in 2:ntx){
    #Draw the sequent rankbars#
    xpos.rank <- ((xbx2l-xbx2r)/(ori.ntx-1)*(rank[i]-1)) + xbx2r
    lines(c(xpos.rank,xpos.rank), c(mid.pos[i-1]-0.3,mid.pos[i-1]+0.3), lwd=2)    
    rect(xleft=xbx2l, ybottom=mid.pos[i-1]+0.3, xright=xbx2r, ytop=mid.pos[i-1]-0.3, col =NA, border = NULL)
    #text(xbx2l,mid.pos[i-1],sprintf("%i ", ori.ntx), cex=0.9*rcex, adj=c(1,0.5),font=1, col="grey70")	#left side bottom rank 'ntx'
    text(xbx2r+0.1,mid.pos[i-1],"1", cex=0.9*rcex, adj=c(0,0.5),font=1, col="grey70")			#right side top rank '1'
    text(xpos.rank,mid.pos[i-1]-0.45,sprintf(ranknumstr, rank[i]), cex=0.95*rcex, adj=c(0.5,0), font=1)	#State numerical estimates
  }
}

#----------------------------------------------------------------------------
#Function to draw the pie in clockwise position starting at 90 degree (12 o'clock) 
pieclkws <- function(xpos,ypos,x,uedges=200,uradius=1,ucol=NULL, ustartpos=pi/2, ushadow=FALSE, clockwise=TRUE, ...) {
  rev.x <- rev(x)
  if(length(ucol)>0) ucol <- rev(ucol) 
  bisect.angles<-floating.pie(xpos,ypos,rev.x, edges=uedges,radius=uradius, col=ucol, startpos=ustartpos, shadow=ushadow, ...)
  return (rev(bisect.angles))
}

#----------------------------------------------------------------------------
# Plot function for MTC SFP with SFP on the same vertical plane
#Call 1 functions above: (i)PrICrI (ii)pieclkws (iii)rankbars
tmpSFP <- function(stytitle, ntx, lstx, mtc, ma, bpredd=TRUE, plt.adj, add.info=1, ucex=1) {
  
  #Check the maximum required print range
  if (bpredd==FALSE) {
    FPmin <- min(ma$lor[,5], mtc$lor[,2], na.rm = TRUE)
    FPmax <- max(ma$lor[,7], mtc$lor[,4], na.rm = TRUE)
  } else {
    FPmin <- min(ma$lor[,5], mtc$lor[,2], ma$predint[,5], mtc$predint[,2], na.rm = TRUE)
    FPmax <- max(ma$lor[,7], mtc$lor[,4], ma$predint[,7], mtc$predint[,4], na.rm = TRUE)
  }
  
  ##Create xpos vector for drawing the vertical lines on the plot
  if (FPmin > -1) {
    if (FPmax>1) width.FP <- 1+ (ceiling(FPmax)+0.5) else width.FP <- 10 #This should never happen; setting as 10 to revert to default scale
  }else {
    if (FPmax>1) width.FP <- (ceiling(abs(FPmin)) +0.5) + (ceiling(FPmax)+0.5) else width.FP <- (ceiling(abs(FPmin)) +0.5) + 1
  }
  #Width of FP should represent 5 units on the graph out of a total of 20 units of ratios (4:6:1:4:5)
  x.p.unit <- width.FP/5
  x.sf <- x.p.unit/2 #x.scaling factor after revision to auto scale the x coordinators; factor before revision is 2 (as used in denominator here) == used in pie below
  xvl.vec <- -20 + c(0, (4*x.p.unit), (10*x.p.unit), (11*x.p.unit), (15*x.p.unit) , (20*x.p.unit))
  
  ##Define axis offset
  if (FPmin > -1) offs=(xvl.vec[5]+1) else offs = xvl.vec[5] + ceiling(abs(FPmin)) +0.5
  
  
  ##tcex == tx name font size
  tcex <- 0.8*ucex
  ##Define results font size
  rcex <- 0.9*tcex
  
  ##For tx=7, 6 comparison sets
  nset <- ntx-1
  ncp <- choose(ntx, 2)
  
  ##Start plot
  #win.graph()
  # if (ntx>=6)  dev.new(width=10, height=11) 
  #   else if (ntx==5 | ntx==4) dev.new(width=9, height=10*sum(c(1:5))/21) #sum(c(1:6)) =21
  #   else if (ntx<4) dev.new(width=9, height=10*sum(c(1:4))/21) #sum(c(1:6)) =21 
  # if (plt.adj==2) { 
  #   if (ntx>=6)  par(mar=c(1,0,1,0))
  #     else if (ntx==5 | ntx==4) par(mar=c(2,0,2,0))
  #     else if (ntx<4) par(mar=c(3,0,2,0))
  # } else if (plt.adj==1) par(mar=c(0.5,0,1,0)) else if (plt.adj==0) par(mar=c(0,0,1,0)) 
  # 
  plot(1:26,seq(-3,22,len=26), type="n", axes=F,ylab="",xlab="", xlim=c(xvl.vec[1],xvl.vec[6]),ylim=c((3*ncp+ntx), -1))
  
  ##Main Title and table headings
  title(main=stytitle, cex.main=ucex)
  text((sum(xvl.vec[1],xvl.vec[3])/2),-0.5,"Comparators", cex=tcex, adj=c(0.5, 0), font=1)
  text((sum(xvl.vec[3:4])/2),-0.5,"H-H\nTrials", cex=tcex, adj=c(0.5, 0), font=1)
  
  if (mtc$type==1){
    text((sum(xvl.vec[4:5])/2),-0.5,"Odds Ratio (95% CrI)", cex=tcex, adj=c(0.5, 0), font=1)
    text((sum(xvl.vec[5:6])/2),-0.5,"Summary Forest Plot\n(log Scale)", cex=tcex, adj=c(0.5, 0), font=1)
  }else if (mtc$type==2){ 
    text((sum(xvl.vec[4:5])/2),-0.5,"Mean Difference\n(95% CrI)", cex=tcex, adj=c(0.5, 0), font=1)
    text((sum(xvl.vec[5:6])/2),-0.5,"Summary Forest Plot", cex=tcex, adj=c(0.5, 0), font=1)
  }else if (mtc$type==3){
    text((sum(xvl.vec[4:5])/2),-0.5,"Hazard Ratio (95% CrI)", cex=tcex, adj=c(0.5, 0), font=1)
    text((sum(xvl.vec[5:6])/2),-0.5,"Summary Forest Plot\n(log Scale)", cex=tcex, adj=c(0.5, 0), font=1)
  }
  
  ##Create position vector for inserting the tx names
  ptxpos <- c((3*(ntx-1)/2))					#primary comparator tx y-position
  pos <- c(seq((0*3)+1,((ntx-1)*3+0),by=3))		#secondary tx y-position
  for (i in (ntx-1):2){
    ptxpos <- c(ptxpos, ((3*sum((ntx-1):i)+(ntx-i)) + 3*(i-1)/2))
    pos <- c(pos, seq((3*sum((ntx-1):i)+(ntx-i)+1),(3*sum((ntx-1):(i-1))+(ntx-i)),by=3))
  }
  
  #Insert the graph for the rank/SUCRA/pbest
  if (add.info==1) {     #List the median rank
    ori.ntx <- length(mtc$rkgram[,1])/ntx
    rankbars(ntx, ori.ntx, mean=0, xvl.vec, ptxpos, pos, mtc$rank[,3], rcex)
  }else if (add.info==2) {    #List the mean rank
    ori.ntx <- length(mtc$rkgram[,1])/ntx
    rankbars(ntx, ori.ntx, mean=1, xvl.vec, ptxpos, pos, mtc$rank[,1], rcex)
  }else if (add.info==3) {    #List the SUCRA percentages estimates
    text((sum(xvl.vec[1:2])/2),1.5*ptxpos[1],sprintf("SUCRA =%.0f%%", mtc$sucra[1]), cex=rcex, adj=c(0.5,0.5), font=1)	# for primary comparator treatment
    for (i in 2:ntx){
      if (i==2) text((xvl.vec[3]-2.6*x.sf),pos[i-1]+0.5,sprintf("SUCRA\n= %.0f%%", mtc$sucra[i]), cex=rcex, adj=0,font=1)
      else text((xvl.vec[3]-2.6*x.sf),pos[i-1]+0.5,sprintf("= %.0f%%", mtc$sucra[i]), cex=rcex, adj=0,font=1)
    }
  }else if (add.info==4) {    #List the pbest estimates
    pbestmat <- cbind(mtc$pbest[,1], (1-mtc$pbest[,1]))
    text((sum(xvl.vec[1:2])/2),1.5*ptxpos[1],sprintf("Pbest\n=%.2f", mtc$pbest[1,1]), cex=rcex, adj=0,font=1)	# for primary comparator treatment
    if (pbestmat[1,1]!=0) pieclkws((sum(xvl.vec[1:2])/2)-0.8*x.sf,(1.5*ptxpos[1]),pbestmat[1,],uedges=200,uradius=0.6*x.sf,ucol=c('grey','white'))
    else pieclkws((sum(xvl.vec[1:2])/2)-0.8*x.sf,(1.5*ptxpos[1]),c(0.0000001, 0.9999999),uedges=200,uradius=0.6*x.sf,ucol=c('grey','white'))
    for (i in 2:ntx){
      if (i==2) text((xvl.vec[3]-2.3*x.sf),pos[i-1]+0.5,sprintf("Pbest\n=%.2f", mtc$pbest[i,1]), cex=rcex, adj=0,font=1)
      else text((xvl.vec[3]-2.3*x.sf),pos[i-1]+0.5,sprintf("=%.2f", mtc$pbest[i,1]), cex=rcex, adj=0,font=1)
      if (pbestmat[i,1]!=0) pieclkws((xvl.vec[3]-3*x.sf),(pos[i-1]+0.5),pbestmat[i,],uedges=200,uradius=0.6*x.sf,ucol=c('grey','white'))
      else pieclkws((xvl.vec[3]-3*x.sf),(pos[i-1]+0.5),c(0.0000001, 0.9999999),uedges=200,uradius=0.6*x.sf,ucol=c('grey','white'))
    }
  }
  
  ini <- 0
  inj <- 0
  inpos <-1
  
  for (k in nset:1) {
    
    #List the primary comparator treatment
    text((sum(xvl.vec[1:2])/2),ptxpos[k],lstx[k], cex=tcex, adj=0.5,font=1)
    
    #List the sec. treatments from txlist(lstx)
    inj <- inj +1
    for (j in 1:k) {	
      ini <-ini + 1		#ini range[1,21]
      text(xvl.vec[2]+1,pos[ini]+0.5,lstx[inj+j], cex=tcex, adj=0,font=1)
    }
    
    #Draw the top & bottom lines from left to right
    lines(c(xvl.vec[1],xvl.vec[6]),c((pos[inpos]-1),(pos[inpos]-1)),lty=1,col=1)#across - top
    if (inpos != 1){
      lines(c(xvl.vec[1],xvl.vec[6]),c((pos[inpos]-2),(pos[inpos]-2)),lty=1,col=1)#across - bottom
      #draw vertical lines & reference line
      for (x in xvl.vec) {
        lines(c(x,x),c(prevtop,(pos[inpos]-2)),lty=1) #down
      }
      #reference line
      lines(c(log(1)+offs,log(1)+offs),c(prevtop,(pos[inpos]-2)),lty=1, col="grey75")
    }
    prevtop <-(pos[inpos]-1)
    inpos <- inpos + k
  }
  
  ####AXES#####
  yaxpos <- pos[ncp]+2			#y-pos of axis line
  lines(c(xvl.vec[1],xvl.vec[6]),c(yaxpos,yaxpos),lty=1,col=1)	#across - axis line
  #Vertical lines for last tx set
  for (x in xvl.vec) {
    lines(c(x,x),c(pos[ncp]-1,yaxpos),lty=1)#down
  }
  #reference line
  lines(c(log(1)+offs,log(1)+offs),c(pos[ncp]-1,yaxpos),lty=1, col="grey75")
  
  #define axis ticks
  if (mtc$type==1 | mtc$type==3) {   #Odds ratio log scale
    lblcex <- c(0.56*ucex,0.56*ucex,0.56*ucex,0.56*ucex,0.56*ucex,0.6*ucex,0.6*ucex,0.6*ucex,0.6*ucex,0.6*ucex,0.6*ucex)
    vticks <- c(1/1024,1/256,1/64,1/16,1/4,1,4,16,64,256,1024)
    lblticks <- c("1/1024","1/256","1/64","1/16","1/4","1","4","16","64","256","1024")
    lnrange <- log(vticks)
    vticks <- vticks[lnrange>=(FPmin-0.5) & lnrange<=(FPmax+0.5)]
    lblticks <- lblticks[lnrange>=(FPmin-0.5) & lnrange<=(FPmax+0.5)]
    lblcex <- lblcex[lnrange>=(FPmin-0.5) & lnrange<=(FPmax+0.5)]
    
    #Print the axis ticks and labels 
    for (i in 1:length(vticks)){ 
      lines(c(log(vticks[i])+offs,log(vticks[i])+offs),c(yaxpos,(yaxpos+0.35)),lty=1)
      text(log(vticks[i])+offs,(yaxpos+0.9),lblticks[i],cex=lblcex[i])
    }
    
    ##Axis text
    if (mtc$type==1) str.type<-"Odds Ratio" else str.type<-"Hazard Ratio"
    if (bpredd==FALSE) text(sum(xvl.vec[5:6])/2,(yaxpos+2.0),paste(str.type, "with 95% CrI\n(log scale)"),font=1, cex=rcex, adj=c(0.5,1))
    else text(sum(xvl.vec[5:6])/2,(yaxpos+2.0), paste(str.type, "with 95% CrI & 95% PI\n(log scale)"),font=1, cex=rcex, adj=c(0.5,1))
    
  }else if (mtc$type==2) { #continuous data scale
    
    vticks <- seq(floor(FPmin), ceiling(FPmax))
    if (length(vticks)>8) vticks <- c( seq(0,floor(FPmin), by=-2) , seq(0, ceiling(FPmax), by=2))
    if (length(vticks)>8) vticks <- c( seq(0,floor(FPmin), by=-5) , seq(0, ceiling(FPmax), by=5))
    if (length(vticks)>8) vticks <- c( seq(0,floor(FPmin), by=-10) , seq(0, ceiling(FPmax), by=10))
    if (length(vticks)>8) vticks <- c( seq(0,floor(FPmin), by=-20) , seq(0, ceiling(FPmax), by=20))
    
    #Print the axis ticks and labels 
    for (i in 1:length(vticks)){ 
      lines(c(vticks[i]+offs,vticks[i]+offs),c(yaxpos,(yaxpos+0.35)),lty=1)
      text(vticks[i]+offs,(yaxpos+0.9),vticks[i],cex=0.6*ucex)
    }
    ##Axis text
    if (bpredd==FALSE) text(sum(xvl.vec[5:6])/2,(yaxpos+1.5),"Mean Difference with 95% CrI",font=1, cex=rcex, adj=c(0.5,1))
    else text(sum(xvl.vec[5:6])/2,(yaxpos+1.5),"Mean Difference with 95% CrI & 95% PI",font=1, cex=rcex, adj=c(0.5,1))
  }
  
  #Heterogeneity
  text(xvl.vec[1],(yaxpos+1.5),sprintf("Heterogeneity: between-study variance = %.2f; 95%% CrI (%.3f to %.3f)", (1/mtc$tau[3]), (1/mtc$tau[4]), (1/mtc$tau[2]) ),font=1, adj=c(0,0.5), cex=rcex)
  
  ##Forest Plot and relative estimates
  for (i in 1:ncp){  
    lines(c(xvl.vec[2],xvl.vec[5]),c((pos[i]+2),(pos[i]+2)),lty=1)	#Lines in between
    text((sum(xvl.vec[3:4])/2),pos[i]+1,format(ma$lor[i,3]),adj=0.5,cex=rcex,col="grey55")	#HTH trial number
    
    if (bpredd==FALSE) {
      #NMA
      text((sum(xvl.vec[4:5])/2),pos[i],sprintf("%.2f ( %.2f to %.2f )", mtc$or[i,3], mtc$or[i,2], mtc$or[i,4]),adj=0.5,cex=rcex,col="black")
      PrICrI(offs, mtc$lor[i,2],mtc$lor[i,3],mtc$lor[i,4], pos[i], ulwd=1.3)
      #PW
      if(!is.na(ma$or[i,5])) text((sum(xvl.vec[4:5])/2),(pos[i]+1),sprintf("%.2f ( %.2f to %.2f )", ma$or[i,6], ma$or[i,5], ma$or[i,7]),adj=0.5,cex=rcex,col="grey55")
      else text((sum(xvl.vec[4:5])/2),(pos[i]+1),sprintf("%.2f", ma$or[i,5]),adj=0.5,cex=rcex,col="grey55")
      PrICrI(offs, ma$lor[i,5], ma$lor[i,6], ma$lor[i,7], pos[i]+1, ucol="grey55", ulwd=1.3)
    }else {
      #NMA
      text((sum(xvl.vec[4:5])/2),pos[i],sprintf("%.2f ( %.2f to %.2f )", mtc$or[i,3], mtc$or[i,2], mtc$or[i,4]),adj=0.5,cex=rcex,col="black")
      PrICrI(offs, mtc$lor[i,2],mtc$lor[i,3],mtc$lor[i,4], pos[i], ulwd=1.3, pcI=TRUE, predbd=c(mtc$predint[i,2], mtc$predint[i,4]))		#summary w PrI
      #PW
      if(!is.na(ma$or[i,5])) text((sum(xvl.vec[4:5])/2),(pos[i]+1),sprintf("%.2f ( %.2f to %.2f )", ma$or[i,6], ma$or[i,5], ma$or[i,7]),adj=0.5,cex=rcex,col="grey55")
      else text((sum(xvl.vec[4:5])/2),(pos[i]+1),sprintf("%.2f", ma$or[i,5]),adj=0.5,cex=rcex,col="grey55")
      PrICrI(offs, ma$lor[i,5], ma$lor[i,6], ma$lor[i,7], pos[i]+1, ucol="grey55", ulwd=1.3, pcI=TRUE, predbd=c(ma$predint[i,5],ma$predint[i,7]))		#summary  w PrI
    }
  }
} #end of function


#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
#END OF FUNCTIONS CODES