#' Takes MetaInsight data and converts it into BNMA format
#'
#' @param br_data A data frame of data in MetaInsight format
#' @param treatment_ids Data frame containing treatment IDs and names in columns named 'Number' and 'Label' respectively
#' @param outcome_type "Continuous" or "Binary"
#' @param ref An element of treatment_ids$Label, the reference treatment.
#' @return List:
#'  - 'ArmLevel' = Data frame containing 'Study', 'Treat', 'N', 'Outcomes', and (for outcome_type="Continuous") 'SD'
#'  - 'Treat.order' = Vector of (unique) treatments, with the reference treatment first
FormatForBnma <- function(br_data, treatment_ids, outcome_type, ref){
  if (FindDataShape(br_data) == "wide"){
    br_data2 <- as.data.frame(WideToLong(br_data, outcome_type=outcome_type))
  } else if (FindDataShape(br_data) == "long"){
    br_data2 <- br_data
  } else {
    stop("data_format has to be 'wide' or 'long'")
  }

  #Use wrangled treatment names
  br_data3 <- br_data2
  br_data3$Treat <- treatment_ids$Label[match(br_data2$T, treatment_ids$Number)]
  
  #Treatment order (put 'ref' first)
  Treat.order <- VectorWithItemFirst(vector = unique(br_data3$Treat), first_item = ref)
  
  #Arm-level data
   if (outcome_type == "Binary"){
     ArmLevel <- dplyr::rename(br_data3, "Outcomes" = "R")
     ArmLevel <- dplyr::select(ArmLevel, c("Study", "Treat", "Outcomes", "N"))
   } else if (outcome_type == "Continuous"){
     ArmLevel <- dplyr::rename(br_data3, "Outcomes" = "Mean")
     ArmLevel <- dplyr::select(ArmLevel, c("Study", "Treat", "Outcomes", "SD", "N"))
   } else{
     stop("outcome_type has to be 'Continuous' or 'Binary'")
   }
  
   return(list(ArmLevel=ArmLevel, Treat.order=Treat.order))
}



#' Creates the baseline risk meta-regression network in BNMA
#'
#' @param br_data A list of data in the format produced by FormatForBnma().
#' @param outcome_type "Continuous" or "Binary".
#' @param effects_type "fixed" or "random".
#' @param cov_parameters "shared", "exchangable", or "unrelated".
#' @return Output from bnma::network.data.
BaselineRiskNetwork <- function(br_data, outcome_type, effects_type, cov_parameters){
  #Use bnma terms
  if (cov_parameters == "shared"){
    cov_parameters <- "common"
  } else if (cov_parameters == "unrelated"){
    cov_parameters <- "independent"
  } else if (cov_parameters != "exchangeable"){
    stop("cov_parameters must be 'shared', 'exchangeable', or 'unrelated'")
  }
  
  if(outcome_type == "Binary"){
    network <- with(br_data, bnma::network.data(Outcomes = ArmLevel$Outcomes,
                                                Study = ArmLevel$Study,
                                                Treat = ArmLevel$Treat,
                                                N = ArmLevel$N,
                                                response = "binomial",
                                                Treat.order = Treat.order,
                                                type = effects_type,
                                                baseline = cov_parameters,
                                                baseline.risk = "independent"))
  }
  if(outcome_type == "Continuous"){
    network <- with(br_data, bnma::network.data(Outcomes = ArmLevel$Outcomes,
                                                Study = ArmLevel$Study,
                                                Treat = ArmLevel$Treat,
                                                response = "normal",
                                                SE = ArmLevel$SD / sqrt(ArmLevel$N),
                                                Treat.order = Treat.order,
                                                type = effects_type,
                                                baseline = cov_parameters,
                                                baseline.risk = "independent"))
  }
  return(network)
}



#' Fits the baseline risk meta-regression model in BNMA
#'
#' @param br_data A data frame of data in MetaInsight format.
#' @param treatment_ids Data frame containing treatment IDs and names in columns named 'Number' and 'Label' respectively.
#' @param outcome_type "Continuous" or "Binary".
#' @param ref An element of treatment_ids$Label, the reference treatment.
#' @param effects_type "fixed" or "random".
#' @param cov_parameters "shared", "exchangable", or "unrelated".
#' @param seed Seed. Defaults to 123.
#' @return Output from bnma::network.run.
BaselineRiskRegression <- function(br_data, treatment_ids, outcome_type, ref,  effects_type, cov_parameters, seed=123){
  formatted_data <- FormatForBnma(br_data, treatment_ids, outcome_type, ref)
  network <- BaselineRiskNetwork(formatted_data, outcome_type, effects_type, cov_parameters)
  #Select random seeds for the four chains based on 'seed'
  set.seed(seed)
  seeds <- sample.int(4, n = .Machine$integer.max)

  #Put the seeds in the required format for passing to bnma::network.run
  rng_inits <- list()
  for(i in 1:length(seeds)){
    rng_inits[[i]] <- list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = seeds[i])
  }

  return(bnma::network.run(network,
                           n.run=10000,
                           RNG.inits=rng_inits,
                           n.chains=length(seeds)))
}



#' Creates a DIC table in gemtc format from a bnma model
#'
#' @param br_model Output from bnma::network.run, typically created from BaselineRiskRegression().
#' @return A DIC table in the same format as from gemtc.
BaselineRiskDicTable <- function(br_model){
  summary <- summary(br_model)
  dic_table <- c(summary$deviance, summary$total_n)
  names(dic_table)[4] <- "Data points"
  return(dic_table)
}
