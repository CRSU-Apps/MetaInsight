##########################################################################
# BaselineRiskRegression function
# Tom Morris
# October 2023
##########################################################################


#====================================================================
# Preliminaries
#====================================================================
#------------------------------------------------
# Libraries
#------------------------------------------------
library(bnma)
library(dplyr)
library(reshape2)

#------------------------------------------------
# Working directory
#------------------------------------------------
#Set working directory to the folder containing this script
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


#====================================================================
# Load and create example data
#====================================================================
#This is roughly the format the data should have. They have used a list to store all variables, whereas I am using a data frame where possible as I think it's clearer.
bnma::certolizumab


#Binomial MetaInsight-long-formatted data
BRdataBin <- read.csv(".\\Datasets\\BinOutBR.csv")


#------------------------------------------
# Convert BRdataBin to wide format
#------------------------------------------

# Clareece, this is quite crude compared to what you did. I couldn't work out how to do it with pivot_wider, or how to do it in one step using dcast.


#' Adds a new column 'count' to 'dataFrame', which is a running total of the number
#'  of treatments within each study
#'
#' @param dataFrame A data frame with the column 'Study'
#' @return dataFrame with new column 'count'
CountTreatment <- function(dataFrame){
  dataFrame$count <- 1
  for(i in 2:length(dataFrame[[1]])){
    if(dataFrame$Study[i] == dataFrame$Study[i-1]){
      dataFrame$count[i] <- dataFrame$count[i-1] + 1
    }
  }
  return(dataFrame)
}


BRdataBin <- CountTreatment(BRdataBin)
#Convert BRdataBin to wide format
BRdataBinT <- reshape2::dcast(BRdataBin, formula=Study~count, value.var="T")
BRdataBinR <- reshape2::dcast(BRdataBin, formula=Study~count, value.var="R")
BRdataBinN <- reshape2::dcast(BRdataBin, formula=Study~count, value.var="N")
BRdataBinT <- rename(BRdataBinT, "T.1"="1", "T.2"="2", "T.3"="3", "T.4"="4")
BRdataBinR <- rename(BRdataBinR, "R.1"="1", "R.2"="2", "R.3"="3", "R.4"="4")
BRdataBinN <- rename(BRdataBinN, "N.1"="1", "N.2"="2", "N.3"="3", "N.4"="4")
BRdataBin_wide <- merge(merge(BRdataBinT, BRdataBinN, by="Study"), BRdataBinR, by="Study")

#------------------------------------------


#Continuous MetaInsight-long-formatted data
BRdataCont <- read.csv(".\\Datasets\\ContOutBR.csv")




#====================================================================
# The functions
#====================================================================

#' Convert wide format to long format (including covariate columns)
#' 
#' @param wide_data Data frame of wide format
#' @param ConBi Indicator whether outcome is binary or continuous
#' @return Data frame in long format
WideToLong <- function(wide_data, ConBi) {
  # Specify columns that contain wide data
  if (ConBi == "Continuous") {
    change_cols <- wide_data %>%
      select(starts_with(c("T","N","Mean","SD")))
  } else {
    change_cols <- wide_data %>%
      select(starts_with(c("T","N","R")))
  }
  # Transform to long
  long_data <- wide_data %>%
    tidyr::pivot_longer(cols = names(change_cols),
                        names_to = c(".value", "arm"),
                        names_pattern = "(.*).(.)",
                        values_drop_na = TRUE
    )
  return(long_data[,names(long_data)!="arm"])
}





#' Takes MetaInsight data and converts it into BNMA format
#'
#' @param BRdata A data frame of data in MetaInsight format
#' @param dataFormat "long" or "wide"
#' @param outcomeType "continuous" or "binomial"
#' @param ref An element of 'BRdata$T', the reference treatment
#' @return A list with elements 'ArmLevel' and 'Treat.order'
#'          'ArmLevel' is a data frame containing 'Study', 'Treat', 'N', 'Outcomes',
#'          and (for outcomeType="continuous") 'SD'
FormatForBnma <- function(BRdata, dataFormat, outcomeType, ref){
  if(dataFormat == "wide"){
    BRdata2 <- as.data.frame(WideToLong(BRdata, ConBi=outcomeType))
  } else if(dataFormat == "long"){
    BRdata2 <- BRdata
  }
  #Treatment order (put 'ref' first)
  Treat.order <- c(ref, unique(BRdata2$T)[-which(unique(BRdata2$T) == ref)])
  #Arm-level data
  if(outcomeType == "binomial"){
    ArmLevel <- dplyr::rename(BRdata2, "Treat"="T", "Outcomes"="R")
  } else if(outcomeType == "continuous"){
    ArmLevel <- dplyr::rename(BRdata2, "Treat"="T", "Outcomes"="Mean")
  }
  return(list(ArmLevel=ArmLevel, Treat.order=Treat.order))
}

#Examples
BRbinBnma <- FormatForBnma(BRdata=BRdataBin, dataFormat="long", outcomeType="binomial", ref="Placebo")
BRbinBnma_wide <- FormatForBnma(BRdata=BRdataBin_wide, dataFormat="wide", outcomeType="binomial", ref="Placebo")
BRcontBnma <- FormatForBnma(BRdata=BRdataCont, dataFormat="long", outcomeType="continuous", ref="Placebo")




#' Fits the baseline risk meta-regression model in BNMA
#'
#' @param BRdata A list of data in the format produced by the 'FormatForBnma' function
#' @param outcomeType "continuous" or "binomial"
#' @param effectsType "fixed" or "random"
#' @param covParameters "common", "exchangable", or "independent"
#' @return Output from bnma::network.run
BaselineRiskRegression <- function(BRdata, outcomeType, effectsType, covParameters){
  if(outcomeType == "binomial"){
    BRnetwork <- with(BRdata, bnma::network.data(Outcomes = ArmLevel$Outcomes,
                                                 Study = ArmLevel$Study,
                                                 Treat = ArmLevel$Treat,
                                                 N = ArmLevel$N,                                          
                                                 response = "binomial",
                                                 Treat.order = Treat.order,
                                                 type = effectsType,
                                                 baseline = covParameters,
                                                 baseline.risk = "independent"))
  }
  if(outcomeType == "continuous"){
    BRnetwork <- with(BRdata, bnma::network.data(Outcomes = ArmLevel$Outcomes,
                                                 Study = ArmLevel$Study,                                           
                                                 Treat = ArmLevel$Treat,
                                                 response = "normal",
                                                 SE = ArmLevel$SD,
                                                 Treat.order = Treat.order,
                                                 type = effectsType,
                                                 baseline = covParameters,
                                                 baseline.risk = "independent"))
  }
  return(network.run(BRnetwork, n.run=10000))
}



#--------------------------------------
# Examples
#--------------------------------------
BRbinRun <- BaselineRiskRegression(BRdata=BRbinBnma, outcomeType="binomial", effectsType="fixed", covParameters="common")
summary(BRbinRun)

BRcontRun <- BaselineRiskRegression(BRdata=BRcontBnma, outcomeType="continuous", effectsType="random", covParameters="exchangeable")
summary(BRcontRun)
