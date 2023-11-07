#' Takes MetaInsight data and converts it into BNMA format
#'
#' @param BRdata A data frame of data in MetaInsight format
#' @param dataFormat "Long" or "Wide"
#' @param outcomeType "Continuous" or "Binomial"
#' @param ref An element of 'BRdata$T', the reference treatment
#' @return A list with elements 'ArmLevel' and 'Treat.order'
#'          'ArmLevel' is a data frame containing 'Study', 'Treat', 'N', 'Outcomes',
#'          and (for outcomeType="continuous") 'SD'
FormatForBnma <- function(BRdata, dataFormat, outcomeType, ref){
  if(dataFormat == "Wide"){
    BRdata2 <- as.data.frame(WideToLong(BRdata, ConBi=outcomeType))
  } else if(dataFormat == "Long"){
    BRdata2 <- BRdata
  }
  #Treatment order (put 'ref' first)
  Treat.order <- c(ref, unique(BRdata2$T)[-which(unique(BRdata2$T) == ref)])
  #Arm-level data
  if(outcomeType == "Binomial"){
    ArmLevel <- dplyr::rename(BRdata2, "Treat"="T", "Outcomes"="R")
  } else if(outcomeType == "Continuous"){
    ArmLevel <- dplyr::rename(BRdata2, "Treat"="T", "Outcomes"="Mean")
  }
  return(list(ArmLevel=ArmLevel, Treat.order=Treat.order))
}




#' Fits the baseline risk meta-regression model in BNMA
#'
#' @param BRdata A list of data in the format produced by the 'FormatForBnma' function.
#' @param outcomeType "Continuous" or "Binomial".
#' @param effectsType "fixed" or "random".
#' @param covParameters "common", "exchangable", or "independent".
#' @param seeds Vector of seeds, which also determines the number of chains. Defaults to 123:125.
#' @return Output from bnma::network.run.
BaselineRiskRegression <- function(BRdata, outcomeType, effectsType, covParameters, seeds=123:125){
  #Put the seeds in the required format
  rng_inits <- list()
  for(i in 1:length(seeds)){
    rng_inits[[i]] <- list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = seeds[i])
  }
  
  if(outcomeType == "Binomial"){
    network <- with(BRdata, bnma::network.data(Outcomes = ArmLevel$Outcomes,
                                                 Study = ArmLevel$Study,
                                                 Treat = ArmLevel$Treat,
                                                 N = ArmLevel$N,                                          
                                                 response = "binomial",
                                                 Treat.order = Treat.order,
                                                 type = effectsType,
                                                 baseline = covParameters,
                                                 baseline.risk = "independent"))
  }
  if(outcomeType == "Continuous"){
    network <- with(BRdata, bnma::network.data(Outcomes = ArmLevel$Outcomes,
                                                 Study = ArmLevel$Study,                                           
                                                 Treat = ArmLevel$Treat,
                                                 response = "normal",
                                                 SE = ArmLevel$SD / sqrt(ArmLevel$N),
                                                 Treat.order = Treat.order,
                                                 type = effectsType,
                                                 baseline = covParameters,
                                                 baseline.risk = "independent"))
  }
  return(bnma::network.run(network,
                           n.run=10000,
                           RNG.inits=rng_inits,
                           n.chains=length(seeds)))
}
