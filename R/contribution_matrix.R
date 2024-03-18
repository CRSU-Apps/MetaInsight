##############################################################################
# The notation within this file matches that in the document:
#   Supplementary_material(updated).pdf
# which is an appendix to the paper:
#   Graphs of study contributions and covariate distributions for network meta-regression, Donegan (2018)
#   Res Syn Meth. 2018;9:243–260.
##############################################################################


#' Get observed effect sizes and variances, as well as variances in the control arm.
#' 
#' @param data Input data in long format plus the column 'Treatment', a text version of 'T'.
#' @param treatments Vector of treatments with the reference treatment first.
#' @param outcome_type "Continuous" or "Binary".
#' @param outcome_measure "MD" (when outcome_type == "Continuous"), "OR", "RR" or "RD" (when outcome_type == "Binary").
#' @return List
#'  - 'effect_sizes' = data frame with columns 'Study', 'Treatment', 'Effect', 'Variance'.
#'  - 'control_variance' = data frame with columns 'Study', 'Treatment', 'Variance'.
GetEffectSizesAndVariances <- function(data, treatments, outcome_type, outcome_measure){
  #Data with only control treatment rows kept
  data_control <- KeepOrDeleteControlTreatment(data = data, treatments = treatments, keep_delete = "keep")
  #Data with control treatment rows deleted
  data_no_control <- KeepOrDeleteControlTreatment(data = data, treatments = treatments, keep_delete = "delete")
  #Create a wide version of data
  data_wide <- merge(data_control, data_no_control, by = "Study")
  
  if (outcome_type == "Binary") {
    
    #Create metafor_measure to pass to metafor::escalc
    if (outcome_measure == "OR") {
      metafor_measure <- "PLO"
    } else if (outcome_measure == "RR") {
      metafor_measure <- "PLN"
    } else if (outcome_measure == "RD") {
      metafor_measure <- "PR"
    } else {
      stop("When outcome_type == 'Binary', outcome_measure must be 'OR', 'RR', or 'RD'")
    }
    
    #Calculate treatment effect variances
    effect_sizes_raw <- metafor::escalc(measure = outcome_measure,
                                        ai = data_wide$R.x,
                                        ci = data_wide$R.y,
                                        n1i = data_wide$N.x,
                                        n2i = data_wide$N.y)
    effect_sizes <- data.frame(Study = data_no_control$Study,
                               Treatment = data_no_control$Treatment,
                               Effect = as.numeric(effect_sizes_raw$yi),
                               Variance = effect_sizes_raw$vi)
    
    #Calculate variance in the control arm
    control_outcome <- metafor::escalc(measure = metafor_measure,
                                       xi = data_control$R,
                                       ni = data_control$N)
    control_variance <- data.frame(Study = data_control$Study,
                                   Treatment = data_control$Treatment,
                                   Variance = control_outcome$vi)
    
  } else if (outcome_type == "Continuous") {
    
    if (outcome_measure == "MD") {
      
      #Calculate treatment effect variances
      effect_sizes_raw <- metafor::escalc(measure = "MD",
                                          m1i = data_wide$Mean.x,
                                          m2i = data_wide$Mean.y,
                                          sd1i = data_wide$SD.x,
                                          sd2i = data_wide$SD.y,
                                          n1i = data_wide$N.x,
                                          n2i = data_wide$N.y)
      effect_sizes <- data.frame(Study = data_no_control$Study,
                                 Treatment = data_no_control$Treatment,
                                 Effect = as.numeric(effect_sizes_raw$yi),
                                 Variance = effect_sizes_raw$vi)
      
      #Calculate variance in the control arm
      control_outcome <- metafor::escalc(measure = "MN",
                                      mi = data_control$Mean,
                                      sdi = data_control$SD,
                                      ni = data_control$N)
      control_variance <- data.frame(Study = data_control$Study,
                                     Treatment = data_control$Treatment,
                                     Variance = control_outcome$vi)
    } else {
      stop("When outcome_type == 'Continuous', outcome_measure must be 'MD'") 
    }
  } else {
    stop("outcome_type must be 'Continuous' or 'Binary'")
  }
  return(list(effect_sizes = effect_sizes,
              control_variance = control_variance)
         )
}



#' Create the variance-covariance matrix of treatment effects.
#' 
#' @param data Input data in long format plus the column 'Treatment', a text version of 'T'.
#' @param studies Vector of studies.
#' @param treatments Vector of treatments with the reference treatment first.
#' @param outcome_type "Continuous" or "Binary".
#' @param outcome_measure "MD" (when outcome_type == "Continuous"), "OR", "RR" or "RD" (when outcome_type == "Binary")
#' @return V matrix.
CreateVMatrix <- function(data, studies, treatments, outcome_type, outcome_measure){
  #Number of studies
  n_studies <- length(studies)
  #Data with control treatment rows deleted
  data_no_control <- KeepOrDeleteControlTreatment(data = data, treatments = treatments, keep_delete = "delete")
  #Used as labels for the matrix rows and columns
  study_treatment_label <- paste0("(", data_no_control$Study, ")", data_no_control$Control, ":", data_no_control$Treatment)
  
  effect_sizes_and_variances <- GetEffectSizesAndVariances(data = data, treatments = treatments,
                                                           outcome_type = outcome_type, outcome_measure = outcome_measure)
  
  effect_sizes <- effect_sizes_and_variances$effect_sizes
  control_variance <- effect_sizes_and_variances$control_variance
  
  #Create the V matrix with treatment effect variances on the leading diagonal
  V <- diag(effect_sizes$Variance)
  rownames(V) <- study_treatment_label
  colnames(V) <- study_treatment_label
  #Number of non-control arms per study
  n_arms <- table(data_no_control$Study)
  #Insert covariances into multi-arm studies. The covariance is always the variance in the control arm.
  for (i in 1:n_studies){
    if (n_arms[studies[i]] > 1){
      #Create a small matrix for this study only, with the control variance everywhere
      mini_V <- matrix(control_variance$Variance[control_variance$Study == studies[i]],
                       nrow = n_arms[studies[i]],
                       ncol = n_arms[studies[i]])
      #Put the treatment effect variances on the leading diagonal
      diag(mini_V) <- effect_sizes$Variance[effect_sizes$Study == studies[i]]
      #Overwrite the submatrix in V corresponding to this study
      V[which(effect_sizes$Study == studies[i]), which(effect_sizes$Study == studies[i])] <- mini_V
    }
  }
  return(V)
}



#' Create the design matrix.
#' 
#' @param data Input data in long format plus the column 'Treatment', a text version of 'T'.
#' @param studies Vector of studies.
#' @param treatments Vector of treatments with the reference treatment first.
#' @param covar_centred Vector of centred covariate values, named by study.
#' @param cov_parameters "shared", "exchangeable", or "unrelated".
#' @return X matrix.
CreateXMatrix <- function(data, studies, treatments, covar_centred, cov_parameters){
  #Number of studies
  n_studies <- length(studies)
  #Number of treatments
  n_treatments <- length(treatments)
  #Reference treatment
  reference <- treatments[1]
  #Data with control treatment rows deleted
  data_no_control <- KeepOrDeleteControlTreatment(data = data, treatments = treatments, keep_delete = "delete")
  #Used as labels for the matrix rows
  study_treatment_label <- paste0("(", data_no_control$Study, ")", data_no_control$Control, ":", data_no_control$Treatment)
  
  #Create the design matrix with the right dimensions and zeros everywhere
  if (cov_parameters %in% c("unrelated", "exchangeable")) {
    X <- matrix(0, nrow = length(study_treatment_label), ncol = 2 * (n_treatments - 1))
    colnames(X) <- c(paste0(treatments[1], ":", treatments[-1], "_d"),
                     paste0(treatments[1], ":", treatments[-1], "_beta"))
  } else if (cov_parameters == "shared"){
    X <- matrix(0, nrow = length(study_treatment_label), ncol = n_treatments)
    colnames(X) <- c(paste0(treatments[1], ":", treatments[-1], "_d"),
                     "B")
  } else {
    stop("cov_parameters must be 'shared', 'exchangeable', or 'unrelated'")
  }
  rownames(X) <- study_treatment_label
  
  #Local function to extract the studies from a vector of strings that are in the format "(Study)Control_treatment:Treatment"
  GetStudies <- function(x){
    substr(x, start = 2, stop = unlist(gregexpr(")", x)) - 1)
  }
  
  #Local function to extract the second treatments from a vector of strings that are in the format "(Study)Control_treatment:Treatment2" or "Reference_treatment:Treatment2"
  GetTreatments <- function(x){
    substr(x, start = unlist(gregexpr(":", x)) + 1, stop = nchar(x))
  }
  
  #Populate the design matrix for unrelated or exchangeable covariate parameters
  if (cov_parameters %in% c("unrelated", "exchangeable")) {
    for (i in 1:length(data_no_control$Study)) {
      
      row_index <- which(GetStudies(rownames(X)) == data_no_control$Study[i]
                         & GetTreatments(rownames(X)) == data_no_control$Treatment[i])
      col_index_d_treat <- which(GetTreatments(colnames(X)) == paste0(data_no_control$Treatment[i], "_d"))
      col_index_d_control <- which(GetTreatments(colnames(X)) == paste0(data_no_control$Control[i], "_d"))
      col_index_beta_treat <- which(GetTreatments(colnames(X)) == paste0(data_no_control$Treatment[i], "_beta"))
      col_index_beta_control <- which(GetTreatments(colnames(X)) == paste0(data_no_control$Control[i], "_beta"))
      
      #If the study contains the reference treatment...
      if (data_no_control$Control[i] == reference) {
        #...put 1 in the treatment column...
        X[row_index, col_index_d_treat] <- 1
        #...and put the covariate value in the treatment's covariate column
        X[row_index, col_index_beta_treat] <- covar_centred[data_no_control$Study[i]]
        
        #If the study does not contain the reference treatment...
      } else if (data_no_control$Control[i] != reference) {
        #...put 1 in the treatment column...
        X[row_index, col_index_d_treat] <- 1
        #...put -1 in the control treatment column...
        X[row_index, col_index_d_control] <- -1
        #...put the covariate value in the treatment's covariate column...
        X[row_index, col_index_beta_treat] <- covar_centred[data_no_control$Study[i]]
        #...and put minus the covariate value in the control treatment's covariate column
        X[row_index, col_index_beta_control] <- -covar_centred[data_no_control$Study[i]]
      }
    }
  }
  
  #Populate the design matrix for shared covariate parameters
  if (cov_parameters == "shared") {
    
    col_index_beta <- which(GetTreatments(colnames(X)) == "B")
    
    for (i in 1:length(data_no_control$Study)) {
      
      row_index <- which(GetStudies(rownames(X)) == data_no_control$Study[i]
                         & GetTreatments(rownames(X)) == data_no_control$Treatment[i])
      col_index_d_treat <- which(GetTreatments(colnames(X)) == paste0(data_no_control$Treatment[i], "_d"))
      col_index_d_control <- which(GetTreatments(colnames(X)) == paste0(data_no_control$Control[i], "_d"))
      
      #If the study contains the reference treatment...
      if (data_no_control$Control[i] == reference) {
        #...put 1 in the treatment column...
        X[row_index, col_index_d_treat] <- 1
        #...and put the covariate value in the covariate column
        X[row_index, col_index_beta] <- covar_centred[data_no_control$Study[i]]
        
        #If the study does not contain the reference treatment...
      } else {
        #...put 1 in the treatment column...
        X[row_index, col_index_d_treat] <- 1
        #...put -1 in the control treatment column...
        X[row_index, col_index_d_control] <- -1
        #...and put 0 in the covariate column
        X[row_index, col_index_beta] <- 0
      }
    }
  }
  
  return(X)
}



#' Create the Z matrix.
#' 
#' @param treatments Vector of treatments with the reference treatment first.
#' @param cov_parameters "shared", "exchangeable", or "unrelated".
#' @return Z matrix.
CreateZMatrix <- function(treatments, cov_parameters){
  #Number of treatments
  n_treatments <- length(treatments)
  #The reference treatment
  reference <- treatments[1]
  #Create the variable treat1_treat2_label, which is all pairs of treatments in the form treatment1:treatment2, where treatment1 comes earlier in 'treatments' than treatment2. There are choose(n_treatments, 2) of these, and they are used as row names in the Z matrix
  treat1_treat2_label <- matrix(0, nrow = n_treatments, ncol = n_treatments)
  for (i in 1:n_treatments) {
    for (j in i:n_treatments) {
      treat1_treat2_label[i, j] <- paste0(treatments[i], ":", treatments[j])
    }
  }
  diag(treat1_treat2_label) <- 0
  treat1_treat2_label <- as.vector(treat1_treat2_label)
  treat1_treat2_label <- treat1_treat2_label[treat1_treat2_label != "0"]
  
  #Create the top left of the Z matrix with the right dimensions and zeros everywhere
  Z_top_left <- matrix(0, nrow = n_treatments * (n_treatments - 1) / 2, ncol = n_treatments - 1)
  rownames(Z_top_left) <- treat1_treat2_label
  colnames(Z_top_left) <- treatments[-1]
  
  #Populate the Z_top_left matrix
  for (i in 1:length(rownames(Z_top_left))) {
    #First treatment in the row
    first_treatment <- substr(rownames(Z_top_left)[i],
                              start = 1,
                              stop = unlist(gregexpr(":", rownames(Z_top_left)[i])) - 1)
    #Second treatment in the row
    second_treatment <- substr(rownames(Z_top_left)[i],
                               start = unlist(gregexpr(":", rownames(Z_top_left)[i])) + 1,
                               stop = nchar(rownames(Z_top_left)[i]))
    #If the first treatment in the row is the reference...
    if (first_treatment == reference) {
      #...put 1 in the column correseponding to the second treatment
      Z_top_left[i, which(colnames(Z_top_left) == second_treatment)] <- 1
    }
    #If the first treatment in the row is not the reference...
    else {
      #...put -1 in the column correseponding to the first treatment
      Z_top_left[i, which(colnames(Z_top_left) == first_treatment)] <- -1
      #...and put 1 in the column correseponding to the second treatment
      Z_top_left[i, which(colnames(Z_top_left) == second_treatment)] <- 1
    }
  }
  
  #Create the other three corners of the Z matrix
  if (cov_parameters %in% c("unrelated", "exchangeable")) {
    Z_top_right <- matrix(0, nrow = n_treatments * (n_treatments - 1) / 2, ncol = n_treatments - 1)
    Z_bottom_left <- Z_top_right
    Z_bottom_right <- Z_top_left
  } else if (cov_parameters == "shared") {
    Z_top_right <- matrix(0, nrow = n_treatments * (n_treatments - 1) / 2, ncol = 1)
    Z_bottom_left <- matrix(0, nrow = 1, ncol = n_treatments - 1)
    Z_bottom_right <- 1
  } else {
    stop("cov_parameters must be 'shared', 'exchangeable', or 'unrelated'")
  }
  
  #Create the Z matrix
  Z <- rbind(cbind(Z_top_left, Z_top_right), cbind(Z_bottom_left, Z_bottom_right))
  if (cov_parameters %in% c("unrelated", "exchangeable")) {
    rownames(Z) <- c(paste0(treat1_treat2_label, "_d"),
                     paste0(treat1_treat2_label, "_beta"))
    colnames(Z) <- c(paste0(treatments[1], ":", treatments[-1], "_d"),
                     paste0(treatments[1], ":", treatments[-1], "_beta"))
  } else if (cov_parameters == "shared") {
    rownames(Z) <- c(paste0(treat1_treat2_label, "_d"),
                     "B")
    colnames(Z) <- c(paste0(treatments[1], ":", treatments[-1], "_d"),
                     "B")
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
  data_no_control <- KeepOrDeleteControlTreatment(data = data, treatments = treatments, keep_delete = "delete")
  #Used as labels for the matrix rows and columns
  study_treatment_label <- paste0("(", data_no_control$Study, ")", data_no_control$Control, ":", data_no_control$Treatment)
  #Create the Lambda_tau matrix with var_d on the leading diagonal
  Lambda_tau <- diag(var_d, nrow = length(data_no_control$Study))
  rownames(Lambda_tau) <- study_treatment_label
  colnames(Lambda_tau) <- study_treatment_label
  #Number of non-control arms per study
  n_arms <- table(data_no_control$Study)
  #Insert covariances into multi-arm studies. The covariance is var_d/2.
  for (i in 1:n_studies) {
    if (n_arms[studies[i]] > 1) {
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



#' Check if the given matrix is singular, which would prevent the contribution matrix from being obtainable, and return an error if it is.
#' 
#' @param matrix The matrix that needs to be inverted to obtain the contribution matrix.
#' @return Error if the matrix is singular.
CheckSingularMatrix <- function(matrix){
  if (matrixcalc::is.singular.matrix(matrix)) {
    stop("Contribution matrix cannot be determined due to singular X^T V^{-1} X. Try changing fixed/random effects or shared/exchangeable/unrelated covariate parameter assumptions")
  }
}



#' Intermediate function called within CreateContributionMatrix(), dealing with the fixed effects, unrelated or shared cases.
#' 
#' @param X X matrix.
#' @param V V matrix.
#' @param Z Z matrix.
#' @param basic_or_all_parameters "basic" for one column per basic parameter, "all" for one column per parameter.
#' @return Raw contribution matrix (no absolute values, no percentages, not by study).
.RawContributionMatrixFixedUnrelatedShared <- function(X, V, Z, basic_or_all_parameters){
  CheckSingularMatrix(t(X) %*% solve(V) %*% X)

  if (basic_or_all_parameters == "all") {
    contribution <- Z %*% solve(t(X) %*% solve(V) %*% X) %*% t(X) %*% solve(V)
  } else if (basic_or_all_parameters == "basic") {
    contribution <- solve(t(X) %*% solve(V) %*% X) %*% t(X) %*% solve(V)
  } else {
    stop("basic_or_all_parameters must be 'basic' or 'all'")
  }
  return(contribution)
}



#' Intermediate function called within CreateContributionMatrix(), dealing with the fixed effects, exchangeable case.
#' 
#' @param X X matrix.
#' @param V V matrix.
#' @param Z Z matrix.
#' @param basic_or_all_parameters "basic" for one column per basic parameter, "all" for one column per parameter.
#' @param treatments Vector of treatments with the reference treatment first.
#' @param std_dev_beta Standard deviation of covariate parameters.
#' @return Raw contribution matrix (no absolute values, no percentages, not by study).
.RawContributionMatrixFixedExchangeable <- function(X, V, Z, basic_or_all_parameters, treatments, std_dev_beta){
  
  #Number of treatments
  n_treatments <- length(treatments)
  
  if (is.null(std_dev_beta)) {
    stop("Must specify 'std_dev_beta' when cov_parameters == 'exchangeable'")
  }
  
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
  
  #Indices for selecting submatrices in X_star
  row_top_index <- 1:nrow(X)
  row_bottom_index <- (nrow(X) + 1):nrow(X_star)
  col_left_index <- 1:ncol(X_d)
  col_middle_index <- (ncol(X_d) + 1):(ncol(X_d) + ncol(X_beta))
  col_right_index <- (ncol(X_d) + ncol(X_beta) + 1):ncol(X_star)
  
  #Populate the top left of X_star
  X_star[row_top_index, col_left_index] <- X_d
  #Populate the top middle of X_star
  X_star[row_top_index, col_middle_index] <- X_beta
  #Populate the bottom middle of X_star
  X_star[row_bottom_index, col_middle_index] <- diag(1, nrow = ncol(X_d))
  #Populate the bottom right of X_star
  X_star[row_bottom_index, col_right_index] <- -matrix(1, nrow = ncol(X_d), ncol = 1)
  
  CheckSingularMatrix(t(X_star) %*% solve(V_star) %*% X_star)
  
  A <- solve(t(X_star) %*% solve(V_star) %*% X_star) %*% t(X_star) %*% solve(V_star)
  A_top_left <- A[1:ncol(X_d), 1:nrow(X_d)]
  A_middle_left <- A[(ncol(X_d) + 1):(ncol(X_d) + ncol(X_beta)), 1:nrow(X_beta)]
  
  if (basic_or_all_parameters == "all") {
    contribution <- Z %*% rbind(A_top_left, A_middle_left)
  } else if (basic_or_all_parameters == "basic") {
    contribution <- rbind(A_top_left, A_middle_left)
  } else {
    stop("basic_or_all_parameters must be 'basic' or 'all'")
  }
  return(contribution)
}



#' Intermediate function called within CreateContributionMatrix(), dealing with the random effects, unrelated or shared cases.
#' 
#' @param X X matrix.
#' @param V V matrix.
#' @param Z Z matrix.
#' @param Lambda_tau Lambda_tau matrix.
#' @param basic_or_all_parameters "basic" for one column per basic parameter, "all" for one column per parameter.
#' @return Raw contribution matrix (no absolute values, no percentages, not by study).
.RawContributionMatrixRandomUnrelatedShared <- function(X, V, Z, Lambda_tau, basic_or_all_parameters){

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
    
    #Indices for selecting submatrices in X_star
    row_bottom_index <- (nrow(X) + 1):nrow(X_star)
    row_top_index <- 1:nrow(X)
    col_left_index <- 1:nrow(X)
    col_right_index <- (nrow(X) + 1):ncol(X_star)
    
    #Populate the top left of X_star
    X_star[row_top_index, col_left_index] <- diag(1, nrow = nrow(X))
    #Populate the bottom left of X_star
    X_star[row_bottom_index, col_left_index] <- diag(1, nrow = nrow(X))
    #Populate the bottom right of X_star
    X_star[row_bottom_index, col_right_index] <- -X
    
    CheckSingularMatrix(t(X_star) %*% solve(V_star) %*% X_star)
    
    A <- solve(t(X_star) %*% solve(V_star) %*% X_star) %*% t(X_star) %*% solve(V_star)
    A_bottom_left <- A[(nrow(X) + 1):nrow(A), 1:nrow(X)]
    
    if (basic_or_all_parameters == "all") {
      contribution <- Z %*% A_bottom_left
    } else if (basic_or_all_parameters == "basic") {
      contribution <- A_bottom_left
    } else {
      stop("basic_or_all_parameters must be 'basic' or 'all'")
    }
    return(contribution)
}
  
  

#' Intermediate function called within CreateContributionMatrix(), dealing with the random effects, exchangeable case.
#' 
#' @param X X matrix.
#' @param V V matrix.
#' @param Z Z matrix.
#' @param Lambda_tau Lambda_tau matrix.
#' @param basic_or_all_parameters "basic" for one column per basic parameter, "all" for one column per parameter.
#' @param treatments Vector of treatments with the reference treatment first.
#' @param std_dev_beta Standard deviation of covariate parameters.
#' @return Raw contribution matrix (no absolute values, no percentages, not by study).
.RawContributionMatrixRandomExchangeable <- function(X, V, Z, Lambda_tau, basic_or_all_parameters, treatments, std_dev_beta){
  if (is.null(std_dev_beta)) {
    stop("Must specify 'std_dev_beta' when cov_parameters == 'exchangeable'")
  }
  
  #Number of treatments
  n_treatments <- length(treatments)
  
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
  
  #Indices for selecting submatrices in X_star
  row_top_index <- 1:nrow(X)
  row_middle_index <- (nrow(X) + 1):(2 * nrow(X))
  row_bottom_index <- (2 * nrow(X) + 1):nrow(X_star)
  col_left_index <- 1:nrow(X)
  col_second_index <- (nrow(X) + 1):(nrow(X) + ncol(X_d))
  col_third_index <- (nrow(X) + ncol(X_d) + 1):(nrow(X) + 2 * ncol(X_d))
  col_right_index <- (nrow(X) + 2 * ncol(X_d) + 1):ncol(X_star)
  
  #Populate the top left of X_star
  X_star[row_top_index, col_left_index] <- diag(1, nrow = nrow(X))
  #Populate the middle left of X_star
  X_star[row_middle_index, col_left_index] <- diag(1, nrow = nrow(X))
  #Populate the middle second column of X_star
  X_star[row_middle_index, col_second_index] <- -X_d
  #Populate the middle third column of X_star
  X_star[row_middle_index, col_third_index] <- -X_beta
  #Populate the bottom third column of X_star
  X_star[row_bottom_index, col_third_index] <- diag(1, nrow = ncol(X_d))
  #Populate the bottom right column of X_star
  X_star[row_bottom_index, col_right_index] <- -matrix(1, nrow = ncol(X_d), ncol = 1)
  
  CheckSingularMatrix(t(X_star) %*% solve(V_star) %*% X_star)
  
  A <- solve(t(X_star) %*% solve(V_star) %*% X_star) %*% t(X_star) %*% solve(V_star)
  A_row2_left <- A[(ncol(X) + 1):(ncol(X) + ncol(X_d)), 1:nrow(X)]
  A_row3_left <- A[(ncol(X) + ncol(X_d) + 1):(ncol(X) + 2 * ncol(X_d)), 1:nrow(X)]
  
  if (basic_or_all_parameters == "all") {
    contribution <- Z %*% rbind(A_row2_left, A_row3_left)
  } else if (basic_or_all_parameters == "basic") {
    contribution <- rbind(A_row2_left, A_row3_left)
  } else {
    stop("basic_or_all_parameters must be 'basic' or 'all'")
  }
  return(contribution)
}

#' Create the contribution matrix in a convenient format.
#' 
#' @param data Input data in long format.
#' @param treatment_ids Data frame containing treatment IDs and names in columns named 'Number' and 'Label' respectively.
#' @param outcome_type "Continuous" or "Binary".
#' @param outcome_measure "MD", "OR", "RR" or "RD".
#' @param effects_type "fixed" or "random".
#' @param std_dev_d Between-study standard deviation. Only required when @param effects_type == "random". Defaults to NULL.
#' @param cov_parameters "shared", "exchangeable", or "unrelated".
#' @param cov_centre Centring value for the covariate, defaults to the mean.
#' @param std_dev_beta Standard deviation of covariate parameters. Only required when @param cov_parameters == "exchangeable". Defaults to NULL.
#' @param study_or_comparison_level "study" for study-level contributions, "comparison" for basic-comparison-level contributions.
#' @param absolute_or_percentage "percentage" for percentage contributions, "absolute" for absolute contributions.
#' @param basic_or_all_parameters "basic" for one column per basic parameter, "all" for one column per parameter. Defaults to "basic".
#' @param weight_or_contribution "weight" for coefficients or "contribution" for coefficients multiplied by observed treatment effects.
#' @return List of contributions
CalculateContributions <- function(
    data,
    treatment_ids,
    outcome_type,
    outcome_measure,
    effects_type,
    std_dev_d = NULL,
    cov_parameters,
    cov_centre = NULL,
    std_dev_beta = NULL,
    study_or_comparison_level,
    absolute_or_percentage,
    basic_or_all_parameters = "basic",
    weight_or_contribution) {
  
  contributions <- CreateContributionMatrix(
    data,
    treatment_ids,
    outcome_type,
    outcome_measure,
    effects_type,
    std_dev_d,
    cov_parameters,
    cov_centre,
    std_dev_beta,
    study_or_comparison_level,
    absolute_or_percentage,
    basic_or_all_parameters,
    weight_or_contribution,
    FALSE
  )
  
  freq <- frequentist(data, outcome_type, treatment_ids, outcome_measure, effects_type)
  d0 <- freq$d0
  
  reference_index <- 1
  reference <- treatment_ids$Label[treatment_ids$Number == reference_index]
  treatments <- treatment_ids$Label[treatment_ids$Label != reference]
  studies <- unique(data$Study)
  
  direct_contributions <- matrix(rep(NA, length(studies) * length(treatments)), length(studies), length(treatments))
  indirect_contributions <- matrix(rep(NA, length(studies) * length(treatments)), length(studies), length(treatments))
  relative_effects <- matrix(rep(NA, length(studies) * length(treatments)), length(studies), length(treatments))
  
  row.names(direct_contributions) <- studies
  row.names(indirect_contributions) <- studies
  row.names(relative_effects) <- studies
  colnames(direct_contributions) <- treatments
  colnames(indirect_contributions) <- treatments
  colnames(relative_effects) <- treatments
  
  for (treatment in treatments) {
    treatment_index <- treatment_ids$Number[treatment_ids$Label == treatment]
    for (study in row.names(contributions)) {
      contribution <- contributions[study, glue::glue("{reference}:{treatment}_d")]
      if (contribution == 0) {
        next
      }
      
      if (treatment %in% FindAllTreatments(data, treatment_ids, study)) {
        direct_contributions[study, treatment] <- contribution
      } else {
        indirect_contributions[study, treatment] <- contribution
      }
      
      treatment_effect <- d0$TE[
        (
          (d0$treat1 == reference_index & d0$treat2 == treatment_index) |
          (d0$treat1 == treatment_index & d0$treat2 == reference_index)
        ) &
        d0$Study == study
      ]
      if (length(treatment_effect) != 0) {
        relative_effects[study, treatment] <- treatment_effect
      }
    }
  }
  
  covariate_column_name = grep("^covar\\..*$", names(data), value = TRUE)[1]
  covariate_values <- data[[covariate_column_name]][match(studies, data$Study)]
  names(covariate_values) <- studies
  
  return(
    list(
      direct = direct_contributions,
      indirect = indirect_contributions,
      treatment_effect = relative_effects,
      covariate_value = covariate_values
    )
  )
}

#' Create the contribution matrix.
#' 
#' @param data Input data in long format.
#' @param treatment_ids Data frame containing treatment IDs and names in columns named 'Number' and 'Label' respectively.
#' @param outcome_type "Continuous" or "Binary".
#' @param outcome_measure "MD", "OR", "RR" or "RD".
#' @param effects_type "fixed" or "random".
#' @param std_dev_d Between-study standard deviation. Only required when @param effects_type == "random". Defaults to NULL.
#' @param cov_parameters "shared", "exchangeable", or "unrelated".
#' @param cov_centre Centring value for the covariate, defaults to the mean.
#' @param std_dev_beta Standard deviation of covariate parameters. Only required when @param cov_parameters == "exchangeable". Defaults to NULL.
#' @param study_or_comparison_level "study" for study-level contributions, "comparison" for basic-comparison-level contributions.
#' @param absolute_or_percentage "percentage" for percentage contributions, "absolute" for absolute contributions.
#' @param basic_or_all_parameters "basic" for one column per basic parameter, "all" for one column per parameter. Defaults to "basic".
#' @param weight_or_contribution "weight" for coefficients or "contribution" for coefficients multiplied by observed treatment effects.
#' @param full_output TRUE or FALSE, defaults to FALSE. See @return for details.
#' @return If @param full_output = FALSE:
#'           The contribution matrix.
#'         If @param full_output = TRUE:
#'           List
#'            - 'contribution' = contribution matrix.
#'            - 'V' = The treatment effect variance matrix.
#'            - 'X' = The design matrix.
#'            - 'Z' = The Z matrix.
#'            - 'Lambda_tau' = The Lambda_tau matrix (only included if @param effects_type == "random").
#'            - 'Lambda_beta' = The Lambda_beta matrix (only included if @param cov_parameters == "exchangeable").
CreateContributionMatrix <- function(data, treatment_ids, outcome_type, outcome_measure, effects_type, std_dev_d = NULL, cov_parameters, cov_centre = NULL, std_dev_beta = NULL, study_or_comparison_level, absolute_or_percentage, basic_or_all_parameters = "basic", weight_or_contribution, full_output = FALSE){
  #Create a text version of the treatment
  data$Treatment <- treatment_ids$Label[match(data$T, treatment_ids$Number)]
  #The unique studies
  studies <- unique(data$Study)
  #The unique treatments
  treatments <- treatment_ids$Label
  #Unduplicated covariate values (one per study)
  covariate <- unique(dplyr::select(data, starts_with(c("Study", "covar."))))$covar.
  #centred covariate values
  if (is.null(cov_centre)) {
    covar_centred <- covariate - mean(covariate)
  } else {
    covar_centred <- covariate - cov_centre
  }
  names(covar_centred) <- studies
  
  V <- CreateVMatrix(data = data,
                     studies = studies,
                     treatments = treatments,
                     outcome_type = outcome_type,
                     outcome_measure = outcome_measure)
  X <- CreateXMatrix(data = data,
                     studies = studies,
                     treatments = treatments,
                     covar_centred = covar_centred,
                     cov_parameters = cov_parameters)
  Z <- CreateZMatrix(treatments = treatments,
                     cov_parameters = cov_parameters)
  
  if (effects_type == "fixed") {
    if (cov_parameters %in% c("unrelated", "shared")) {
      contribution <- .RawContributionMatrixFixedUnrelatedShared(X = X, V = V, Z = Z,
                                                                 basic_or_all_parameters = basic_or_all_parameters)
    } else if (cov_parameters == "exchangeable") {
      contribution <- .RawContributionMatrixFixedExchangeable(X = X, V = V, Z = Z,
                                                             basic_or_all_parameters = basic_or_all_parameters,
                                                             treatments = treatments,
                                                             std_dev_beta = std_dev_beta)
    }
  } else if (effects_type == "random") {
    
    if (is.null(std_dev_d)) {
      stop("Must specify 'std_dev_d' when effects_type == 'random'")
    }
    
    Lambda_tau <- CreateLambdaTauMatrix(data = data, studies = studies, treatments = treatments, std_dev_d = std_dev_d)
    
    if (cov_parameters %in% c("unrelated", "shared")) {
      contribution <- .RawContributionMatrixRandomUnrelatedShared(X = X, V = V, Z = Z,
                                                                  Lambda_tau = Lambda_tau,
                                                                  basic_or_all_parameters = basic_or_all_parameters)
    } else if (cov_parameters == "exchangeable") {
      contribution <- .RawContributionMatrixRandomExchangeable(X = X, V = V, Z = Z,
                                                              Lambda_tau = Lambda_tau,
                                                              basic_or_all_parameters = basic_or_all_parameters,
                                                              treatments = treatments,
                                                              std_dev_beta = std_dev_beta)
    }

  } else{
    stop("effects_type must be 'fixed' or 'random'")
  }
  
  if (weight_or_contribution == "contribution") {
    effect_sizes <- GetEffectSizesAndVariances(data = data,
                                               treatments = treatments,
                                               outcome_type = outcome_type,
                                               outcome_measure = outcome_measure)$effect_sizes$Effect
    #Multiply the n-th row of 'contribution' by the n-th element of 'effect_sizes'
    contribution <- contribution * effect_sizes
  } else if (weight_or_contribution != "weight") {
    stop("weight_or_contribution must be 'weight' or 'contribution'")
  }
  
  contribution_abs <- abs(contribution)
  contribution_row_sums <- rowSums(contribution_abs)
  contribution_percent <- 100 * contribution_abs / contribution_row_sums
  
  #Select percentage contributions or absolute contributions
  if (absolute_or_percentage == "percentage") {
    contribution_output <- t(contribution_percent)
  } else if (absolute_or_percentage == "absolute") {
    contribution_output <- t(contribution_abs)
  } else {
    stop("absolute_or_percentage must be 'absolute' or 'percentage'") 
  }
  
  #Create a study level contribution matrix if required
  if (study_or_comparison_level == "study") {
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
    for (study in studies) {
      #If there is only one row in the contribution matrix for this study, copy it
      if (n_arms[study] == 2) {
        contribution_study[which(rownames(contribution_study) == study), ] <- contribution_output[which(rownames(contribution_output) == study), ]
      #If there is more than one row for this study, add up the rows
      } else if (n_arms[study] > 2) {
        contribution_study[which(rownames(contribution_study) == study), ] <- colSums(contribution_output[which(rownames(contribution_output) == study), ])
      }
    }
    contribution_output <- contribution_study
  } else if (study_or_comparison_level != "comparison") {
    stop("study_or_comparison_level must be 'study' or 'comparison'")
  }

  #For some configurations the column names are not carried through, so put them back in
  if (is.null(colnames(contribution_output))) {
    colnames(contribution_output) <- colnames(X)
  }
  
  if (!full_output) {
    return(round(contribution_output, digits = 2))
  } else{
    if (effects_type == "fixed") {
      if (cov_parameters %in% c("unrelated", "shared")) {
        return(list(contribution = contribution_output,
                    V = V,
                    X = X,
                    Z = Z)
        )
      } else if (cov_parameters == "exchangeable") {
        return(list(contribution = contribution_output,
                    V = V,
                    X = X,
                    Z = Z,
                    Lambda_beta = Lambda_beta)
        )
      }
    } else if (effects_type == "random") {
      if (cov_parameters %in% c("unrelated", "shared")) {
        return(list(contribution = contribution_output,
                    V = V,
                    X = X,
                    Z = Z,
                    Lambda_tau = Lambda_tau)
        )
      } else if (cov_parameters == "exchangeable") {
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


