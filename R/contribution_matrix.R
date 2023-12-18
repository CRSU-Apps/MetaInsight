
#' Create the variance-covariance matrix of treatment effects.
#' 
#' @param data Input data in long format plus the column 'Treatment', a text version of 'T'.
#' @param studies Vector of studies.
#' @param treatments Vector of treatments with the reference treatment first.
#' @param outcome_type "Continuous" or "Binary".
#' @return V matrix.
CreateVMatrix <- function(data, studies, treatments, outcome_type){
  #Number of studies
  n_studies <- length(studies)
  #Data with only control treatment rows kept
  data_control <- KeepControlTreatment(data = data, treatments = treatments)
  #Data with control treatment rows deleted
  data_no_control <- DeleteControlTreatment(data = data, treatments = treatments)
  #Create a wide version of data
  data_wide <- merge(data_control, data_no_control, by = "Study")
  #Used as labels for the matrix rows and columns
  study_treatment <- paste0("(", data_no_control$Study, ")", data_no_control$Control, ":", data_no_control$Treatment)
  
  if(outcome_type == "Binary"){
    #Calculate treatment effect variances
    effect_sizes <- metafor::escalc(measure = "OR",
                                    ai = data_wide$R.x,
                                    ci = data_wide$R.y,
                                    n1i = data_wide$N.x,
                                    n2i = data_wide$N.y)
    effect_variance <- data.frame(Study = data_no_control$Study,
                                  Treatment = data_no_control$Treatment,
                                  Variance = effect_sizes$vi)
    
    #Calculate variance in the control arm
    control_log_odds <- metafor::escalc(measure = "PLO",
                                        xi = data_control$R,
                                        ni = data_control$N)
    control_variance <- data.frame(Study = data_control$Study,
                                   Treatment = data_control$Treatment,
                                   Variance = control_log_odds$vi)
  } else if(outcome_type == "Continuous"){
    #Calculate treatment effect variances
    effect_sizes <- metafor::escalc(measure = "MD",
                                    m1i = data_wide$Mean.x,
                                    m2i = data_wide$Mean.y,
                                    sd1i = data_wide$SD.x,
                                    sd2i = data_wide$SD.y,
                                    n1i = data_wide$N.x,
                                    n2i = data_wide$N.y)
    effect_variance <- data.frame(Study = data_no_control$Study,
                                  Treatment = data_no_control$Treatment,
                                  Variance = effect_sizes$vi)
    
    #Calculate variance in the control arm
    control_mean <- metafor::escalc(measure = "MN",
                                    mi = data_control$Mean,
                                    sdi = data_control$SD,
                                    ni = data_control$N)
    control_variance <- data.frame(Study = data_control$Study,
                                   Treatment = data_control$Treatment,
                                   Variance = control_mean$vi)
  } else{
    stop("outcome_type must be 'Continuous' or 'Binary'")
  }
  
  #Create the V matrix with treatment effect variances on the leading diagonal
  V <- diag(effect_variance$Variance)
  rownames(V) <- study_treatment
  colnames(V) <- study_treatment
  #Number of arms per study
  n_arms <- table(data_wide$Study)
  #Insert covariances into multi-arm studies. The covariance is always the variance in the control arm.
  for(i in 1:n_studies){
    if(n_arms[studies[i]] > 1){
      #Create a small matrix for this study only, with the control variance everywhere
      mini_V <- matrix(control_variance$Variance[control_variance$Study == studies[i]],
                       nrow = n_arms[studies[i]],
                       ncol = n_arms[studies[i]])
      #Put the treatment effect variances on the leading diagonal
      diag(mini_V) <- effect_variance$Variance[effect_variance$Study == studies[i]]
      #Overwrite the submatrix in V corresponding to this study
      V[which(effect_variance$Study == studies[i]), which(effect_variance$Study == studies[i])] <- mini_V
    }
  }
  return(V)
}



#' Create the design matrix.
#' 
#' @param data Input data in long format plus the column 'Treatment', a text version of 'T'.
#' @param studies Vector of studies.
#' @param treatments Vector of treatments with the reference treatment first.
#' @param covar_centered Vector of centered covariate values, named by study.
#' @param cov_parameters "Shared", "Exchangeable", or "Unrelated".
#' @return X matrix.
CreateXMatrix <- function(data, studies, treatments, covar_centered, cov_parameters){
  #Number of studies
  n_studies <- length(studies)
  #Number of treatments
  n_treatments <- length(treatments)
  #Reference treatment
  reference <- treatments[1]
  #Data with control treatment rows deleted
  data_no_control <- DeleteControlTreatment(data = data, treatments = treatments)
  #Used as labels for the matrix rows
  study_treatment <- paste0("(", data_no_control$Study, ")", data_no_control$Control, ":", data_no_control$Treatment)
  
  #Create the design matrix with the right dimensions and zeros everywhere
  if(cov_parameters %in% c("Unrelated", "Exchangeable")){
    X <- matrix(0, nrow = length(study_treatment), ncol = 2 * (n_treatments - 1))
    colnames(X) <- c(paste0(treatments[1], ":", treatments[-1]), paste0(treatments[1], ":", treatments[-1], "_beta"))
  } else if(cov_parameters == "Shared"){
    X <- matrix(0, nrow = length(study_treatment), ncol = n_treatments)
    colnames(X) <- c(paste0(treatments[1], ":", treatments[-1]), "B")
  } else{
    stop("cov_parameters must be 'Shared', 'Exchangeable', or 'Unrelated'")
  }
  rownames(X) <- study_treatment
  
  #Local function to extract the studies from a vector of strings that are in the format "(Study)Control_treatment:Treatment"
  GetStudies <- function(x){
    substr(x, start = 2, stop = unlist(gregexpr(")", x)) - 1)
  }
  
  #Local function to extract the second treatments from a vector of strings that are in the format "(Study)Control_treatment:Treatment2" or "Reference_treatment:Treatment2"
  GetTreatments <- function(x){
    substr(x, start = unlist(gregexpr(":", x)) + 1, stop = nchar(x))
  }
  
  #Populate the design matrix for unrelated covariate parameters
  if(cov_parameters %in% c("Unrelated", "Exchangeable")){
    for(i in 1:length(data_no_control$Study)){
      #If the study contains the reference treatment...
      if(data_no_control$Control[i] == reference){
        #...put 1 in the treatment column...
        X[which(GetStudies(rownames(X)) == data_no_control$Study[i] & GetTreatments(rownames(X)) == data_no_control$Treatment[i]), which(GetTreatments(colnames(X)) == data_no_control$Treatment[i])] <- 1
        #...and put the covariate value in the treatment's covariate column
        X[which(GetStudies(rownames(X)) == data_no_control$Study[i] & GetTreatments(rownames(X)) == data_no_control$Treatment[i]), which(GetTreatments(colnames(X)) == paste0(data_no_control$Treatment[i], "_beta"))] <- covar_centered[data_no_control$Study[i]]
        
        #If the study does not contain the reference treatment...
      } else if(data_no_control$Control[i] != reference){
        #...put 1 in the treatment column...
        X[which(GetStudies(rownames(X)) == data_no_control$Study[i] & GetTreatments(rownames(X)) == data_no_control$Treatment[i]), which(GetTreatments(colnames(X)) == data_no_control$Treatment[i])] <- 1
        #...put -1 in the control treatment column...
        X[which(GetStudies(rownames(X)) == data_no_control$Study[i] & GetTreatments(rownames(X)) == data_no_control$Treatment[i]), which(GetTreatments(colnames(X)) == data_no_control$Control[i])] <- -1
        #...put the covariate value in the treatment's covariate column...
        X[which(GetStudies(rownames(X)) == data_no_control$Study[i] & GetTreatments(rownames(X)) == data_no_control$Treatment[i]), which(GetTreatments(colnames(X)) == paste0(data_no_control$Treatment[i], "_beta"))] <- covar_centered[data_no_control$Study[i]]
        #...and put minus the covariate value in the control treatment's covariate column
        X[which(GetStudies(rownames(X)) == data_no_control$Study[i] & GetTreatments(rownames(X)) == data_no_control$Treatment[i]), which(GetTreatments(colnames(X)) == paste0(data_no_control$Control[i], "_beta"))] <- -covar_centered[data_no_control$Study[i]]
      }
    }
  }
  
  #Populate the design matrix for shared covariate parameters
  if(cov_parameters == "Shared"){
    for(i in 1:length(data_no_control$Study)){
      #If the study contains the reference treatment...
      if(data_no_control$Control[i] == reference){
        #...put 1 in the treatment column...
        X[which(GetStudies(rownames(X)) == data_no_control$Study[i] & GetTreatments(rownames(X)) == data_no_control$Treatment[i]), which(GetTreatments(colnames(X)) == data_no_control$Treatment[i])] <- 1
        #...and put the covariate value in the covariate column
        X[which(GetStudies(rownames(X)) == data_no_control$Study[i] & GetTreatments(rownames(X)) == data_no_control$Treatment[i]), which(GetTreatments(colnames(X)) == "B")] <- covar_centered[data_no_control$Study[i]]
        
        #If the study does not contain the reference treatment...
      } else if(data_no_control$Control[i] != reference){
        #...put 1 in the treatment column...
        X[which(GetStudies(rownames(X)) == data_no_control$Study[i] & GetTreatments(rownames(X)) == data_no_control$Treatment[i]), which(GetTreatments(colnames(X)) == data_no_control$Treatment[i])] <- 1
        #...put -1 in the control treatment column...
        X[which(GetStudies(rownames(X)) == data_no_control$Study[i] & GetTreatments(rownames(X)) == data_no_control$Treatment[i]), which(GetTreatments(colnames(X)) == data_no_control$Control[i])] <- -1
        #...and put 0 in the covariate column
        X[which(GetStudies(rownames(X)) == data_no_control$Study[i] & GetTreatments(rownames(X)) == data_no_control$Treatment[i]), which(GetTreatments(colnames(X)) == "B")] <- 0
      }
    }
  }
  
  return(X)
}



#' Create the Z matrix.
#' 
#' @param treatments Vector of treatments with the reference treatment first.
#' @param cov_parameters "Shared", "Exchangeable", or "Unrelated".
#' @return Z matrix.
CreateZMatrix <- function(treatments, cov_parameters){
  #Number of treatments
  n_treatments <- length(treatments)
  #The reference treatment
  reference <- treatments[1]
  #Create the variable treat_treat, which is all pairs of treatments in the form treatment1:treatment2, where treatment1 comes earlier in 'treatments' than treatment2. There are choose(n_treatments, 2) of these, and they are used as row names in the Z matrix
  treat_treat <- matrix(0, nrow = n_treatments, ncol = n_treatments)
  for(i in 1:n_treatments){
    for(j in i:n_treatments){
      treat_treat[i, j] <- paste0(treatments[i], ":", treatments[j])
    }
  }
  diag(treat_treat) <- 0
  treat_treat <- as.vector(treat_treat)
  treat_treat <- treat_treat[treat_treat != "0"]
  
  #Create the top left of the Z matrix with the right dimensions and zeros everywhere
  Z_top_left <- matrix(0, nrow = n_treatments * (n_treatments - 1) / 2, ncol = n_treatments - 1)
  rownames(Z_top_left) <- treat_treat
  colnames(Z_top_left) <- treatments[-1]
  
  #Populate the Z_top_left matrix
  for(i in 1:length(rownames(Z_top_left))){
    #First treatment in the row
    first_treatment <- substr(rownames(Z_top_left)[i],
                              start = 1,
                              stop = unlist(gregexpr(":", rownames(Z_top_left)[i])) - 1)
    #Second treatment in the row
    second_treatment <- substr(rownames(Z_top_left)[i],
                               start = unlist(gregexpr(":", rownames(Z_top_left)[i])) + 1,
                               stop = nchar(rownames(Z_top_left)[i]))
    #If the first treatment in the row is the reference...
    if(first_treatment == reference){
      #...put 1 in the column correseponding to the second treatment
      Z_top_left[i, which(colnames(Z_top_left) == second_treatment)] <- 1
    }
    #If the first treatment in the row is not the reference...
    else if(first_treatment != reference){
      #...put -1 in the column correseponding to the first treatment
      Z_top_left[i, which(colnames(Z_top_left) == first_treatment)] <- -1
      #...and put 1 in the column correseponding to the second treatment
      Z_top_left[i, which(colnames(Z_top_left) == second_treatment)] <- 1
    }
  }
  
  #Create the other three corners of the Z matrix
  if(cov_parameters %in% c("Unrelated", "Exchangeable")){
    Z_top_right <- matrix(0, nrow = n_treatments * (n_treatments - 1) / 2, ncol = n_treatments - 1)
    Z_bottom_left <- Z_top_right
    Z_bottom_right <- Z_top_left
  } else if(cov_parameters == "Shared"){
    Z_top_right <- matrix(0, nrow = n_treatments * (n_treatments - 1) / 2, ncol = 1)
    Z_bottom_left <- matrix(0, nrow = 1, ncol = n_treatments - 1)
    Z_bottom_right <- 1
  } else{
    stop("cov_parameters must be 'Shared', 'Exchangeable', or 'Unrelated'")
  }
  
  #Create the Z matrix
  Z <- rbind(cbind(Z_top_left, Z_top_right), cbind(Z_bottom_left, Z_bottom_right))
  if(cov_parameters %in% c("Unrelated", "Exchangeable")){
    rownames(Z) <- c(treat_treat, paste0(treat_treat, "_beta"))
    colnames(Z) <- c(paste0(treatments[1], ":", treatments[-1]), paste0(treatments[1], ":", treatments[-1], "_beta"))
  } else if(cov_parameters == "Shared"){
    rownames(Z) <- c(treat_treat, "B")
    colnames(Z) <- c(paste0(treatments[1], ":", treatments[-1]), "B")
  }
  
  return(Z)
}



#' Create the matrix of random effects variances.
#' 
#' @param data Input data in long format.
#' @param studies Vector of studies.
#' @param treatments Vector of treatments with the reference treatment first.
#' @param std_dev_d Between-study standard deviation.
#' @return Lambda_tau matrix.
CreateLambdaTauMatrix <- function(data, studies, treatments, std_dev_d){
  #Number of studies
  n_studies <- length(studies)
  #Between study-variance
  var_d <- std_dev_d^2
  #Data with control treatment rows deleted
  data_no_control <- DeleteControlTreatment(data = data, treatments = treatments)
  #Used as labels for the matrix rows and columns
  study_treatment <- paste0("(", data_no_control$Study, ")", data_no_control$Control, ":", data_no_control$Treatment)
  #Create the Lambda_tau matrix with var_d on the leading diagonal
  Lambda_tau <- diag(var_d, nrow = length(data_no_control$Study))
  rownames(Lambda_tau) <- study_treatment
  colnames(Lambda_tau) <- study_treatment
  #Number of arms per study
  n_arms <- table(data_no_control$Study)
  #Insert covariances into multi-arm studies. The covariance is var_d/2.
  for(i in 1:n_studies){
    if(n_arms[studies[i]] > 1){
      #Create a small matrix for this study only, with var_d/2 everywhere
      mini_Lambda <- matrix(var_d/2,
                            nrow = n_arms[studies[i]],
                            ncol = n_arms[studies[i]])
      #Put var_d on the leading diagonal
      diag(mini_Lambda) <- var_d
      #Overwrite the submatrix in Lambda_tau corresponding to this study
      Lambda_tau[which(data_no_control$Study == studies[i]), which(data_no_control$Study == studies[i])] <- mini_Lambda
    }
  }
  return(Lambda_tau)
}



#' Create the matrix of variances for exchangeable covariate parameters.
#' 
#' @param treatments Vector of treatments with the reference treatment first.
#' @param std_dev_beta Standard deviation of covariate parameters.
#' @return Lambda_beta matrix.
CreateLambdaBetaMatrix <- function(treatments, std_dev_beta){
  #Between study-variance
  var_beta <- std_dev_beta^2
  #Number of treatments
  n_treatments <- length(treatments)
  #Create the Lambda_tau matrix
  Lambda_beta <- diag(var_beta, nrow = n_treatments - 1)
  rownames(Lambda_beta) <- paste0(treatments[1], ":", treatments[-1])
  colnames(Lambda_beta) <- paste0(treatments[1], ":", treatments[-1])
  return(Lambda_beta)
}



#' Create the contribution matrix
#' 
#' @param data Input data in long format.
#' @param treatment_ids Data frame containing treatment IDs and names in columns named 'Number' and 'Label' respectively.
#' @param outcome_type "Continuous" or "Binary".
#' @param effects_type "Fixed" or "Random".
#' @param std_dev_d Between-study standard deviation. Only required when @param effects_type == "Random". Defaults to NULL.
#' @param cov_parameters "Shared", "Exchangeable", or "Unrelated".
#' @param std_dev_beta Standard deviation of covariate parameters. Only required when @param cov_parameters == "Exchangeable". Defaults to NULL.
#' @param study_level TRUE for study-level contributions, FALSE for basic-comparison-level contributions.
#' @param percentages TRUE for percentage contributions, FALSE for absolute contributions.
#' @param all_parameters TRUE for one column per parameter, FALSE for one column per basic parameter. Defaults to TRUE.
#' @param full_output TRUE or FALSE, defaults to FALSE. See @return for details.
#' @return If @param full_output = FALSE:
#'           The contribution matrix.
#'         If @param full_output = TRUE:
#'           List
#'            - 'contribution' = contribution matrix.
#'            - 'V' = The treatment effect variance matrix.
#'            - 'X' = The design matrix.
#'            - 'Z' = The Z matrix.
#'            - 'Lambda_tau' = The Lambda_tau matrix (only included if @param effects_type == "Random").
#'            - 'Lambda_beta' = The Lambda_beta matrix (only included if @param cov_parameters == "Exchangeable").
CreateContributionMatrix <- function(data, treatment_ids, outcome_type, effects_type, std_dev_d = NULL, cov_parameters, std_dev_beta = NULL, study_level, percentages, all_parameters = TRUE, full_output = FALSE){
  #Create a text version of the treatment
  data$Treatment <- treatment_ids$Label[match(data$T, treatment_ids$Number)]
  #The unique studies
  studies <- unique(data$Study)
  #The unique treatments
  treatments <- treatment_ids$Label
  #Number of treatments
  n_treatments <- length(treatments)
  #Unduplicated covariate values (one per study)
  covariate <- unique(dplyr::select(data, starts_with(c("Study", "covar."))))$covar.
  #Centered covariate values
  covar_centered <- covariate - mean(covariate)
  names(covar_centered) <- studies
  
  V <- CreateVMatrix(data = data, studies = studies, treatments = treatments,  outcome_type = outcome_type)
  X <- CreateXMatrix(data = data,
                     studies = studies,
                     treatments = treatments,
                     covar_centered = covar_centered,
                     cov_parameters = cov_parameters)
  Z <- CreateZMatrix(treatments = treatments, cov_parameters = cov_parameters)
  
  if(effects_type == "Fixed"){
    
    if(cov_parameters %in% c("Unrelated", "Shared")){
      
      if(all_parameters){
        contribution <- Z %*% solve(t(X) %*% solve(V) %*% X) %*% t(X) %*% solve(V)
      } else if(!all_parameters){
        contribution <- solve(t(X) %*% solve(V) %*% X) %*% t(X) %*% solve(V)
      }
      
    } else if(cov_parameters == "Exchangeable"){
      
      if(is.null(std_dev_beta)){stop("Must specify 'std_dev_beta' when cov_parameters == 'Exchangeable'")}
      
      Lambda_beta <- CreateLambdaBetaMatrix(treatments = treatments, std_dev_beta = std_dev_beta)
      
      #Create V_star with the correct dimensions and 0 everywhere
      V_star <- matrix(0, nrow = nrow(V) + nrow(Lambda_beta), ncol = ncol(V) + ncol(Lambda_beta))
      rownames(V_star) <- c(rownames(V), rownames(Lambda_beta))
      colnames(V_star) <- c(colnames(V), colnames(Lambda_beta))
      #Populate the top left of V_star
      V_star[1:nrow(V), 1:ncol(V)] <- V
      #Populate the bottom right of V_star
      V_star[(nrow(V) + 1): nrow(V_star), (ncol(V) + 1): ncol(V_star)] <- Lambda_beta
      
      #The left half of X, corresponding to the treatment effects
      X_d <- X[, 1:(n_treatments - 1)]
      #The right half of X, corresponding to the covariate effects
      X_beta <- X[, n_treatments:(2 * (n_treatments - 1))]
      
      #Create X_star with the correct dimensions and 0 everywhere
      X_star <- matrix(0, nrow = nrow(X) + ncol(X_d), ncol = 2 * ncol(X_d) + 1)
      #Populate the top left of X_star
      X_star[1:nrow(X), 1:ncol(X_d)] <- X_d
      #Populate the top middle of X_star
      X_star[1:nrow(X), (ncol(X_d) + 1):(ncol(X_d) + ncol(X_beta))] <- X_beta
      #Populate the bottom middle of X_star
      X_star[(nrow(X) + 1):nrow(X_star), (ncol(X_d) + 1):(ncol(X_d) + ncol(X_beta))] <- diag(1, nrow = ncol(X_d))
      #Populate the bottom right of X_star
      X_star[(nrow(X) + 1):nrow(X_star), (ncol(X_d) + ncol(X_beta) + 1):ncol(X_star)] <- -matrix(1, nrow = ncol(X_d), ncol = 1)
      
      A <- solve(t(X_star) %*% solve(V_star) %*% X_star) %*% t(X_star) %*% solve(V_star)
      A_top_left <- A[1:ncol(X_d), 1:nrow(X_d)]
      A_middle_left <- A[(ncol(X_d) + 1):(ncol(X_d) + ncol(X_beta)), 1:nrow(X_beta)]
      
      if(all_parameters){
        contribution <- Z %*% rbind(A_top_left, A_middle_left)
      } else if(!all_parameters){
        contribution <- rbind(A_top_left, A_middle_left)
      }
    }
    
  } else if(effects_type == "Random"){
    
    if(is.null(std_dev_d)){stop("Must specify 'std_dev_d' when effects_type == 'Random'")}
    
    Lambda_tau <- CreateLambdaTauMatrix(data = data, studies = studies, treatments = treatments, std_dev_d = std_dev_d)
    
    if(cov_parameters %in% c("Unrelated", "Shared")){
      
      #Create V_star with the correct dimensions and 0 everywhere
      V_star <- matrix(0, nrow = nrow(V) + nrow(Lambda_tau), ncol = ncol(V) + ncol(Lambda_tau))
      rownames(V_star) <- c(rownames(V), rownames(Lambda_tau))
      colnames(V_star) <- c(colnames(V), colnames(Lambda_tau))
      #Populate the top left of V_star
      V_star[1:nrow(V), 1:ncol(V)] <- V
      #Populate the bottom right of V_star
      V_star[(nrow(V) + 1): nrow(V_star), (ncol(V) + 1): ncol(V_star)] <- Lambda_tau
      
      #Create X_star with the correct dimensions and 0 everywhere
      X_star <- matrix(0, nrow = 2 * nrow(X), ncol = nrow(X) + ncol(Z))
      #Populate the top left of X_star
      X_star[1:nrow(X), 1:nrow(X)] <- diag(1, nrow = nrow(X))
      #Populate the bottom left of X_star
      X_star[(nrow(X) + 1):nrow(X_star), 1:nrow(X)] <- diag(1, nrow = nrow(X))
      #Populate the bottom right of X_star
      X_star[(nrow(X) + 1):nrow(X_star), (nrow(X) + 1):ncol(X_star)] <- -X
      
      A <- solve(t(X_star) %*% solve(V_star) %*% X_star) %*% t(X_star) %*% solve(V_star)
      A_bottom_left <- A[(nrow(X) + 1):nrow(A), 1:nrow(X)]
      
      if(all_parameters){
        contribution <- Z %*% A_bottom_left
      } else if(!all_parameters){
        contribution <- A_bottom_left
      }
      
    } else if(cov_parameters == "Exchangeable"){
      
      if(is.null(std_dev_beta)){stop("Must specify 'std_dev_beta' when cov_parameters == 'Exchangeable'")}
      
      Lambda_beta <- CreateLambdaBetaMatrix(treatments = treatments, std_dev_beta = std_dev_beta)
      
      #Create V_star with the correct dimensions and 0 everywhere
      V_star <- matrix(0, nrow = nrow(V) + nrow(Lambda_tau) + nrow(Lambda_beta), ncol = ncol(V) + ncol(Lambda_tau) + ncol(Lambda_beta))
      rownames(V_star) <- c(rownames(V), rownames(Lambda_tau), rownames(Lambda_beta))
      colnames(V_star) <- c(colnames(V), colnames(Lambda_tau), colnames(Lambda_beta))
      #Populate the top left of V_star
      V_star[1:nrow(V), 1:ncol(V)] <- V
      #Populate the middle of V_star
      V_star[(nrow(V) + 1):(nrow(V) + nrow(Lambda_tau)), (ncol(V) + 1):(ncol(V) + ncol(Lambda_tau))] <- Lambda_tau
      #Populate the bottom right of V_star
      V_star[(nrow(V) + nrow(Lambda_tau) + 1):nrow(V_star), (ncol(V) + ncol(Lambda_tau) + 1):ncol(V_star)] <- Lambda_beta
      
      #The left half of X, corresponding to the treatment effects
      X_d <- X[, 1:(n_treatments - 1)]
      #The right half of X, corresponding to the covariate effects
      X_beta <- X[, n_treatments:(2 * (n_treatments - 1))]
      
      #Create X_star with the correct dimensions and 0 everywhere
      X_star <- matrix(0, nrow = 2 * nrow(X) + ncol(X_d), ncol = nrow(X) + 2 * ncol(X_d) + 1)
      #Populate the top left of X_star
      X_star[1:nrow(X), 1:nrow(X)] <- diag(1, nrow = nrow(X))
      #Populate the middle left of X_star
      X_star[(nrow(X) + 1):(2 * nrow(X)), 1:nrow(X)] <- diag(1, nrow = nrow(X))
      #Populate the middle second column of X_star
      X_star[(nrow(X) + 1):(2 * nrow(X)), (nrow(X) + 1):(nrow(X) + ncol(X_d))] <- -X_d
      #Populate the middle third column of X_star
      X_star[(nrow(X) + 1):(2 * nrow(X)), (nrow(X) + ncol(X_d) + 1):(nrow(X) + 2 * ncol(X_d))] <- -X_beta
      #Populate the bottom third column of X_star
      X_star[(2 * nrow(X) + 1):nrow(X_star), (nrow(X) + ncol(X_d) + 1):(nrow(X) + 2 * ncol(X_d))] <- diag(1, nrow = ncol(X_d))
      #Populate the bottom right column of X_star
      X_star[(2 * nrow(X) + 1):nrow(X_star), (nrow(X) + 2 * ncol(X_d) + 1):ncol(X_star)] <- -matrix(1, nrow = ncol(X_d), ncol = 1)
      
      A <- solve(t(X_star) %*% solve(V_star) %*% X_star) %*% t(X_star) %*% solve(V_star)
      A_row2_left <- A[(ncol(X) + 1):(ncol(X) + ncol(X_d)), 1:nrow(X)]
      A_row3_left <- A[(ncol(X) + ncol(X_d) + 1):(ncol(X) + 2 * ncol(X_d)), 1:nrow(X)]
      
      if(all_parameters){
        contribution <- Z %*% rbind(A_row2_left, A_row3_left)
      } else if(!all_parameters){
        contribution <- rbind(A_row2_left, A_row3_left)
      }
    }  
    
  } else{
    stop("effects_type must be 'Fixed' or 'Random'")
  }
  
  contribution_abs <- abs(contribution)
  contribution_row_sums <- rowSums(contribution_abs)
  contribution_percent <- 100 * contribution_abs / contribution_row_sums
  
  #Select percentage contributions or absolute contributions
  if(percentages){
    contribution_output <- t(contribution_percent)
  } else if(!percentages){
    contribution_output <- t(contribution_abs)
  }
  
  #Create a study level contribution matrix if required
  if(study_level){
    #Number of studies
    n_studies <- length(studies)
    #Number of arms per study
    n_arms <- table(data$Study)
    
    #Local function to extract the studies from a vector of strings that are in the format "(Study)Control_treatment:Treatment2"
    GetStudies <- function(x){
      substr(x, start = 2, stop = unlist(gregexpr(")", x)) - 1)
    }
    
    #Rename rows by study only
    rownames(contribution_output) <- GetStudies(rownames(contribution_output))
    #Create the contribution matrix for studies only with the correct dimensions
    contribution_study <- matrix(nrow = n_studies, ncol = ncol(contribution_output))
    rownames(contribution_study) <- studies
    colnames(contribution_study) <- colnames(contribution_output)
    for(study in studies){
      #If there is only one row in the contribution matrix for this study, copy it
      if(n_arms[study] == 2){
        contribution_study[which(rownames(contribution_study) == study), ] <- contribution_output[which(rownames(contribution_output) == study), ]
      #If there is more than one row for this study, add up the rows
      } else if(n_arms[study] > 2){
        contribution_study[which(rownames(contribution_study) == study), ] <- colSums(contribution_output[which(rownames(contribution_output) == study), ])
      }
    }
    contribution_output <- contribution_study
  }
  
  #For some configurations the column names are not carried through, so put them back in
  if(is.null(colnames(contribution_output))){
    colnames(contribution_output) <- colnames(X)
  }
  
  if(!full_output){
    return(round(contribution_output, digits = 2))
  } else if(full_output){
    if(effects_type == "Fixed"){
      if(cov_parameters %in% c("Unrelated", "Shared")){
        return(list(contribution = contribution_output,
                    V = V,
                    X = X,
                    Z = Z)
        )
      } else if(cov_parameters == "Exchangeable"){
        return(list(contribution = contribution_output,
                    V = V,
                    X = X,
                    Z = Z,
                    Lamnda_beta = Lambda_beta)
        )
      }
    } else if(effects_type == "Random"){
      if(cov_parameters %in% c("Unrelated", "Shared")){
        return(list(contribution = contribution_output,
                    V = V,
                    X = X,
                    Z = Z,
                    Lambda_tau = Lambda_tau)
        )
      } else if(cov_parameters == "Exchangeable"){
        return(list(contribution = contribution_output,
                    V = V,
                    X = X,
                    Z = Z,
                    Lambda_tau = Lambda_tau,
                    Lambda_beta = Lambda_beta)
        )
      }
    }
  }
}


