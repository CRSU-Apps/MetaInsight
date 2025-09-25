#' @title covariate_regression
#' @description Does x
#' @param x x
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default NULL
#' @return NULL
#' @examples {
#' y <- covariate_regression(1)
#' }
#' @export
covariate_regression <- function(model_output,
                                 connected_data,
                                 covariate_column,
                                 covariate_name,
                                 treatment_df,
                                 outcome,
                                 outcome_measure,
                                 model_type,
                                 absolute_or_percentage,
                                 weight_or_contribution,
                                 treatment_or_covariate_effect,
                                 comparators,
                                 contribution_type,
                                 include_covariate,
                                 include_ghosts,
                                 include_extrapolation,
                                 include_confidence,
                                 confidence_opacity,
                                 include_contributions,
                                 contribution_multiplier,
                                 legend_position,
                                 logger = NULL){

  cov_parameters <- model_output$mtcResults$model$regressor$coefficient

  if (model_output$model == "random") {
    std_dev_d <- model_output$sumresults$summaries$quantiles["sd.d", "50%"]
  }

  if (cov_parameters == "exchangeable") {
    std_dev_beta <- model_output$sumresults$summaries$quantiles["reg.sd", "50%"]
  } else {
    std_dev_beta <- NULL
  }

  contribution_matrix <- CalculateContributions(
    data = connected_data,
    covariate_title = covariate_column,
    treatment_ids = treatment_df,
    outcome = outcome,
    outcome_measure = outcome_measure,
    effects_type = model_type,
    std_dev_d = std_dev_d,
    std_dev_beta = std_dev_beta,
    cov_parameters = cov_parameters,
    study_or_arm_level = "study",
    absolute_or_percentage = absolute_or_percentage,
    weight_or_contribution = weight_or_contribution,
    treatment_or_covariate_effect = treatment_or_covariate_effect)

  confidence_regions <- CalculateConfidenceRegions(model_output)

  CreateCompositeMetaRegressionPlot(
    model_output = model_output,
    treatment_df = treatment_df,
    outcome_measure = outcome_measure,
    comparators = comparators,
    contribution_matrix = contribution_matrix,
    contribution_type = contribution_type,
    confidence_regions = confidence_regions,
    include_covariate = include_covariate,
    include_ghosts = include_ghosts,
    include_extrapolation = include_extrapolation,
    include_confidence = include_confidence,
    confidence_opacity = confidence_opacity,
    include_contributions = include_contributions, #input$contributions != "None",
    contribution_multiplier = contribution_multiplier,
    legend_position = legend_position
  )

}

##############################################################################
# The notation within this file matches that in the document:
#   Supplementary_material(updated).pdf
# which is an appendix to the paper:
#   Graphs of study contributions and covariate distributions for network meta-regression, Donegan (2018)
#   Res Syn Meth. 2018;9:243–260.
##############################################################################


#' Get observed outcomes and variances.
#'
#' @param data Input data in long format plus the column 'Treatment', a text version of 'T'.
#' @param treatments Vector of treatments with the reference treatment first.
#' @param outcome_type "Continuous" or "Binary".
#' @param outcome_measure "MD" (when outcome_type == "Continuous"), "OR", "RR" or "RD" (when outcome_type == "Binary").
#' @return Data frame with columns 'Study', 'Treatment', 'Outcome', and 'Variance'.
GetOutcomesAndVariances <- function(data, treatments, outcome_type, outcome_measure){

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

    #Calculate outcomes and variances
    outcome_and_variance_raw <- metafor::escalc(measure = metafor_measure,
                                                xi = data$R,
                                                ni = data$N)

  } else if (outcome_type == "Continuous") {

    if (outcome_measure == "MD") {

      #Calculate outcomes and  variances
      outcome_and_variance_raw <- metafor::escalc(measure = "MN",
                                                  mi = data$Mean,
                                                  sdi = data$SD,
                                                  ni = data$N)
    } else {
      stop("When outcome_type == 'Continuous', outcome_measure must be 'MD'")
    }
  } else {
    stop("outcome_type must be 'Continuous' or 'Binary'")
  }

  outcome_and_variance <- data.frame(Study = data$Study,
                                     Treatment = data$Treatment,
                                     Outcome = as.vector(outcome_and_variance_raw$yi),
                                     Variance = as.vector(outcome_and_variance_raw$vi))

  return(outcome_and_variance)
}



#' Create the variance-covariance matrix of outcomes.
#'
#' @param data Input data in long format plus the column 'Treatment', a text version of 'T'.
#' @param studies Vector of studies.
#' @param treatments Vector of treatments with the reference treatment first.
#' @param outcome_type "Continuous" or "Binary".
#' @param outcome_measure "MD" (when outcome_type == "Continuous"), "OR", "RR" or "RD" (when outcome_type == "Binary")
#' @return V matrix.
CreateVMatrix <- function(data, studies, treatments, outcome_type, outcome_measure){
  variances <- GetOutcomesAndVariances(data = data, treatments = treatments,
                                       outcome_type = outcome_type, outcome_measure = outcome_measure)

  #Used as labels for the matrix rows and columns
  study_treatment_label <- paste0("(", variances$Study, ")", variances$Treatment)

  V <- diag(variances$Variance)
  rownames(V) <- study_treatment_label
  colnames(V) <- study_treatment_label

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
  #Add the control treatment to the data
  for (i in 1:length(data$Study)) {
    data$Control[i] <- treatments[min(match(data$Treatment[data$Study == data$Study[i]], treatments))]
  }
  #Used as labels for the matrix rows
  study_treatment_label <- paste0("(", data$Study, ")", data$Treatment)

  #Create the design matrix with the right dimensions and zeros everywhere
  if (cov_parameters %in% c("unrelated", "exchangeable")) {
    X <- matrix(0, nrow = length(study_treatment_label), ncol = n_studies + 2 * (n_treatments - 1))
    colnames(X) <- c(paste0(studies, "-eta"),
                     paste0(treatments[1], ":", treatments[-1], "-d"),
                     paste0(treatments[1], ":", treatments[-1], "-beta"))
  } else if (cov_parameters == "shared"){
    X <- matrix(0, nrow = length(study_treatment_label), ncol = n_studies + n_treatments)
    colnames(X) <- c(paste0(studies, "-eta"),
                     paste0(treatments[1], ":", treatments[-1], "-d"),
                     "B")
  } else {
    stop("cov_parameters must be 'shared', 'exchangeable', or 'unrelated'")
  }
  rownames(X) <- study_treatment_label

  #Local function to extract the studies from a vector of strings that are in the format "(Study)Treatment"
  GetStudies <- function(x){
    substr(x, start = 2, stop = unlist(gregexpr(")", x)) - 1)
  }

  #Local function to extract the studies from a vector of strings that are in the format "Study-eta"
  GetStudiesFromColumn <- function(x){
    substr(x, start = 1, stop = unlist(gregexpr("-eta", x)) - 1)
  }

  #Local function to extract the treatments from a vector of strings that are in the format "(Study)Treatment"
  GetTreatments <- function(x){
    substr(x, start = unlist(gregexpr(")", x)) + 1, stop = nchar(x))
  }

  #Local function to extract the treatments from a vector of strings that are in the format "Reference:Treatment"
  GetTreatmentsFromColumn <- function(x){
    substr(x, start = unlist(gregexpr(":", x)) + 1, stop = nchar(x))
  }

  #Populate the design matrix for unrelated or exchangeable covariate parameters
  if (cov_parameters %in% c("unrelated", "exchangeable")) {
    for (i in 1:length(data$Study)) {

      row_index <- which(GetStudies(rownames(X)) == data$Study[i]
                         & GetTreatments(rownames(X)) == data$Treatment[i])
      col_index_eta <- which(GetStudiesFromColumn(colnames(X)) == data$Study[i])
      col_index_d_treat <- which(GetTreatmentsFromColumn(colnames(X)) == paste0(data$Treatment[i], "-d"))
      col_index_d_control <- which(GetTreatmentsFromColumn(colnames(X)) == paste0(data$Control[i], "-d"))
      col_index_beta_treat <- which(GetTreatmentsFromColumn(colnames(X)) == paste0(data$Treatment[i], "-beta"))
      col_index_beta_control <- which(GetTreatmentsFromColumn(colnames(X)) == paste0(data$Control[i], "-beta"))

      #Put 1 in the eta column corresponding to this study
      X[row_index, col_index_eta] <- 1

      #If this treatment is the control treatment then leave all other values at 0

      #If this treatment is not the control treatment...
      if (data$Treatment[i] != data$Control[i]) {
        #If the study contains the reference treatment...
        if (data$Control[i] == reference) {
          #...put 1 in the treatment column...
          X[row_index, col_index_d_treat] <- 1
          #...and put the covariate value in the treatment's covariate column
          X[row_index, col_index_beta_treat] <- covar_centred[data$Study[i]]

          #If the study does not contain the reference treatment...
        } else if (data$Control[i] != reference) {
          #...put 1 in the treatment column...
          X[row_index, col_index_d_treat] <- 1
          #...put -1 in the control treatment column...
          X[row_index, col_index_d_control] <- -1
          #...put the covariate value in the treatment's covariate column...
          X[row_index, col_index_beta_treat] <- covar_centred[data$Study[i]]
          #...and put minus the covariate value in the control treatment's covariate column
          X[row_index, col_index_beta_control] <- -covar_centred[data$Study[i]]
        }
      }
    }
  }

  #Populate the design matrix for shared covariate parameters
  if (cov_parameters == "shared") {

    col_index_beta <- which(GetTreatmentsFromColumn(colnames(X)) == "B")

    for (i in 1:length(data$Study)) {

      row_index <- which(GetStudies(rownames(X)) == data$Study[i]
                         & GetTreatments(rownames(X)) == data$Treatment[i])
      col_index_eta <- which(GetStudiesFromColumn(colnames(X)) == data$Study[i])
      col_index_d_treat <- which(GetTreatmentsFromColumn(colnames(X)) == paste0(data$Treatment[i], "-d"))
      col_index_d_control <- which(GetTreatmentsFromColumn(colnames(X)) == paste0(data$Control[i], "-d"))

      #Put 1 in the eta column corresponding to this study
      X[row_index, col_index_eta] <- 1

      #If this treatment is the control treatment then leave all other values at 0

      #If this treatment is not the control treatment...
      if (data$Treatment[i] != data$Control[i]) {
        #If the study contains the reference treatment...
        if (data$Control[i] == reference) {
          #...put 1 in the treatment column...
          X[row_index, col_index_d_treat] <- 1
          #...and put the covariate value in the covariate column
          X[row_index, col_index_beta] <- covar_centred[data$Study[i]]

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
  }

  return(X)
}



#' Create the Z matrix.
#'
#' @param studies Vector of studies.
#' @param treatments Vector of treatments with the reference treatment first.
#' @param cov_parameters "shared", "exchangeable", or "unrelated".
#' @return Z matrix.
CreateZMatrix <- function(studies, treatments, cov_parameters){
  #Number of studies
  n_studies <- length(studies)
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

  #Create the middle of the Z matrix with the right dimensions and zeros everywhere
  Z_middle <- matrix(0, nrow = n_treatments * (n_treatments - 1) / 2, ncol = n_treatments - 1)
  rownames(Z_middle) <- treat1_treat2_label
  colnames(Z_middle) <- treatments[-1]

  #Populate the Z_middle matrix
  for (i in 1:length(rownames(Z_middle))) {
    #First treatment in the row
    first_treatment <- substr(rownames(Z_middle)[i],
                              start = 1,
                              stop = unlist(gregexpr(":", rownames(Z_middle)[i])) - 1)
    #Second treatment in the row
    second_treatment <- substr(rownames(Z_middle)[i],
                               start = unlist(gregexpr(":", rownames(Z_middle)[i])) + 1,
                               stop = nchar(rownames(Z_middle)[i]))
    #If the first treatment in the row is the reference...
    if (first_treatment == reference) {
      #...put 1 in the column correseponding to the second treatment
      Z_middle[i, which(colnames(Z_middle) == second_treatment)] <- 1
    }
    #If the first treatment in the row is not the reference...
    else {
      #...put -1 in the column correseponding to the first treatment
      Z_middle[i, which(colnames(Z_middle) == first_treatment)] <- -1
      #...and put 1 in the column correseponding to the second treatment
      Z_middle[i, which(colnames(Z_middle) == second_treatment)] <- 1
    }
  }

  #Create the bottom right of the Z matrix
  if (cov_parameters %in% c("unrelated", "exchangeable")) {
    Z_bottom_right <- Z_middle
  } else if (cov_parameters == "shared") {
    Z_bottom_right <- matrix(1)
  } else {
    stop("cov_parameters must be 'shared', 'exchangeable', or 'unrelated'")
  }

  #Create the top left of the Z matrix
  Z_top_left <- diag(1, nrow = n_studies)

  #Create the Z matrix
  Z <- as.matrix(Matrix::bdiag(Z_top_left, Z_middle, Z_bottom_right))

  if (cov_parameters %in% c("unrelated", "exchangeable")) {
    rownames(Z) <- c(paste0(studies, "-eta"),
                     paste0(treat1_treat2_label, "-d"),
                     paste0(treat1_treat2_label, "-beta"))
    colnames(Z) <- c(paste0(studies, "-eta"),
                     paste0(treatments[1], ":", treatments[-1], "-d"),
                     paste0(treatments[1], ":", treatments[-1], "-beta"))
  } else if (cov_parameters == "shared") {
    rownames(Z) <- c(paste0(studies, "-eta"),
                     paste0(treat1_treat2_label, "-d"),
                     "B")
    colnames(Z) <- c(paste0(studies, "-eta"),
                     paste0(treatments[1], ":", treatments[-1], "-d"),
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
  #Between study-variance
  var_d <- std_dev_d^2
  #Labels for the matrix rows and columns
  study_treatment_label <- paste0("(", data$Study, ")", data$Treatment)
  #Create the Lambda_tau matrix
  Lambda_tau <- diag(var_d, nrow = length(data$Study))
  rownames(Lambda_tau) <- study_treatment_label
  colnames(Lambda_tau) <- study_treatment_label

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
#' @return The weight matrix.
.WeightMatrixFixedUnrelatedShared <- function(X, V, Z, basic_or_all_parameters){
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
#' @param studies Vector of studies.
#' @param treatments Vector of treatments with the reference treatment first.
#' @param std_dev_beta Standard deviation of covariate parameters.
#' @return The weight matrix.
.WeightMatrixFixedExchangeable <- function(X, V, Z, basic_or_all_parameters, studies, treatments, std_dev_beta){

  #Number of studies
  n_studies <- length(studies)

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
  V_star[(nrow(V) + 1):nrow(V_star), (ncol(V) + 1):ncol(V_star)] <- Lambda_beta

  #The left section of X, corresponding to the studies and treatment effects
  X_d <- X[, 1:(n_studies + n_treatments - 1)]
  #The right section of X, corresponding to the covariate parameters
  X_beta <- X[, (n_studies + n_treatments):ncol(X)]

  #Create X_star with the correct dimensions and 0 everywhere
  X_star <- matrix(0, nrow = nrow(X) + ncol(X_beta), ncol = ncol(X) + 1)

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
  X_star[row_bottom_index, col_middle_index] <- diag(1, nrow = ncol(X_beta))
  #Populate the bottom right of X_star
  X_star[row_bottom_index, col_right_index] <- -matrix(1, nrow = ncol(X_beta), ncol = 1)

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
#' @return The weight matrix.
.WeightMatrixRandomUnrelatedShared <- function(X, V, Z, Lambda_tau, basic_or_all_parameters){

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
#' @param studies Vector of studies.
#' @param treatments Vector of treatments with the reference treatment first.
#' @param std_dev_beta Standard deviation of covariate parameters.
#' @return The weight matrix.
.WeightMatrixRandomExchangeable <- function(X, V, Z, Lambda_tau, basic_or_all_parameters, studies, treatments, std_dev_beta){
  if (is.null(std_dev_beta)) {
    stop("Must specify 'std_dev_beta' when cov_parameters == 'exchangeable'")
  }

  #Number of studies
  n_studies <- length(studies)

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

  #The left section of X, corresponding to the studies and treatment effects
  X_d <- X[, 1:(n_studies + n_treatments - 1)]
  #The right section of X, corresponding to the covariate parameters
  X_beta <- X[, (n_studies + n_treatments):ncol(X)]

  #Create X_star with the correct dimensions and 0 everywhere
  X_star <- matrix(0, nrow = 2 * nrow(X) + ncol(X_beta), ncol = nrow(X) + ncol(X_d) + ncol(X_beta) + 1)

  #Indices for selecting submatrices in X_star
  row_top_index <- 1:nrow(X)
  row_middle_index <- (nrow(X) + 1):(2 * nrow(X))
  row_bottom_index <- (2 * nrow(X) + 1):nrow(X_star)
  col_left_index <- 1:nrow(X)
  col_second_index <- (nrow(X) + 1):(nrow(X) + ncol(X_d))
  col_third_index <- (nrow(X) + ncol(X_d) + 1):(nrow(X) + ncol(X_d) + ncol(X_beta))
  col_right_index <- (nrow(X) + ncol(X_d) + ncol(X_beta) + 1):ncol(X_star)

  #Populate the top left of X_star
  X_star[row_top_index, col_left_index] <- diag(1, nrow = nrow(X))
  #Populate the middle left of X_star
  X_star[row_middle_index, col_left_index] <- diag(1, nrow = nrow(X))
  #Populate the middle second column of X_star
  X_star[row_middle_index, col_second_index] <- -X_d
  #Populate the middle third column of X_star
  X_star[row_middle_index, col_third_index] <- -X_beta
  #Populate the bottom third column of X_star
  X_star[row_bottom_index, col_third_index] <- diag(1, nrow = ncol(X_beta))
  #Populate the bottom right column of X_star
  X_star[row_bottom_index, col_right_index] <- -matrix(1, nrow = ncol(X_beta), ncol = 1)

  CheckSingularMatrix(t(X_star) %*% solve(V_star) %*% X_star)

  A <- solve(t(X_star) %*% solve(V_star) %*% X_star) %*% t(X_star) %*% solve(V_star)
  A_row2_left <- A[(ncol(X) + 1):(ncol(X) + ncol(X_d)), 1:nrow(X)]
  A_row3_left <- A[(ncol(X) + ncol(X_d) + 1):(ncol(X) + ncol(X_d) + ncol(X_beta)), 1:nrow(X)]

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
#' @param covariate_title Title of covariate column in data.
#' @param treatment_ids Data frame containing treatment IDs and names in columns named 'Number' and 'Label' respectively.
#' @param outcome_type "Continuous" or "Binary".
#' @param outcome_measure "MD", "OR", "RR" or "RD".
#' @param effects_type "fixed" or "random".
#' @param std_dev_d Between-study standard deviation. Only required when @param effects_type == "random". Defaults to NULL.
#' @param cov_parameters Type of regression coefficient. One of: "shared", "exchangeable", or "unrelated".
#' @param cov_centre Centring value for the covariate, defaults to the mean.
#' @param std_dev_beta Standard deviation of covariate parameters. Only required when @param cov_parameters == "exchangeable". Defaults to NULL.
#' @param study_or_arm_level "study" for study-level contributions, "arm" for arm-level contributions.
#' @param absolute_or_percentage "percentage" for percentage contributions, "absolute" for absolute contributions.
#' @param basic_or_all_parameters "basic" for one column per basic parameter, "all" for one column per parameter. Defaults to "basic".
#' @param weight_or_contribution "weight" for coefficients or "contribution" for coefficients multiplied by observed treatment effects.
#' @param treatment_or_covariate_effect Whether contributions are for treatment effect or covariate effect. One of: "Treatment Effect", "Covariate Effect".
#' @return List of contributions:
#' - "direct"
#'   - Matrix of direct contributions to the regression. Rows are studies, columns are treatments
#' - "indirect"
#'   - Matrix of indirect contributions to the regression. Rows are studies, columns are treatments
#' - "relative_effect"
#'   - Matrix of relative effects of treatments compared to the reference. Rows are studies, columns are treatments
#' - "covariate_value"
#'   - Vector of covariate values from the studies.
CalculateContributions <- function(
    data,
    covariate_title,
    treatment_ids,
    outcome_type,
    outcome_measure,
    effects_type,
    std_dev_d = NULL,
    cov_parameters,
    cov_centre = NULL,
    std_dev_beta = NULL,
    study_or_arm_level,
    absolute_or_percentage,
    basic_or_all_parameters = "basic",
    weight_or_contribution,
    treatment_or_covariate_effect) {

  contributions <- CreateContributionMatrix(
    data = data,
    treatment_ids = treatment_ids,
    outcome_type = outcome_type,
    outcome_measure = outcome_measure,
    effects_type = effects_type,
    std_dev_d = std_dev_d,
    cov_parameters = cov_parameters,
    cov_centre = cov_centre,
    std_dev_beta = std_dev_beta,
    study_or_arm_level = study_or_arm_level,
    absolute_or_percentage = absolute_or_percentage,
    basic_or_all_parameters = basic_or_all_parameters,
    weight_or_contribution = weight_or_contribution,
    full_output = FALSE
  )

  if (outcome_type == "Binary") {
    d0 <- meta::pairwise(treat = T, event = R, studlab = Study, n = N, data = data)
  } else if (outcome_type == "Continuous") {
    d0 <- meta::pairwise(treat = T, mean = Mean, sd = SD, studlab = Study, n = N, data = data)
  } else {
    stop(glue::glue("Outcome type '{outcome_type}' is not supported. Please use 'Binary' or 'Continuous'"))
  }

  #Switch the treatment effects to match the rest of the app.
  d0$TE <- -d0$TE

  reference_index <- 1
  reference <- treatment_ids$Label[treatment_ids$Number == reference_index]
  treatments <- treatment_ids$Label[treatment_ids$Label != reference]
  studies <- unique(data$Study)

  direct_contributions <- matrix(
    nrow = length(studies),
    ncol = length(treatments),
    dimnames = list(
      studies,
      treatments
    )
  )
  indirect_contributions <- matrix(
    nrow = length(studies),
    ncol = length(treatments),
    dimnames = list(
      studies,
      treatments
    )
  )
  relative_effects <- matrix(
    nrow = length(studies),
    ncol = length(treatments),
    dimnames = list(
      studies,
      treatments
    )
  )

  if (!treatment_or_covariate_effect %in% c("Treatment Effect", "Covariate Effect")) {
    stop(glue::glue("Contribution type '{}' not recognised. Please use one of: 'Treatment Effect', 'Covariate Effect'"))
  }

  if (treatment_or_covariate_effect == "Treatment Effect") {
    column_format <- "{reference}:{treatment}-d"
  } else {
    if (!cov_parameters %in% c("shared", "unrelated", "exchangeable")) {
      stop(glue::glue("Regression coefficient '{cov_parameters}' not recognised. Please use one of: 'shared', 'unrelated', 'exchangeable'"))
    }

    if (cov_parameters == "shared") {
      column_format <- "B"
    } else if (cov_parameters %in% c("unrelated", "exchangeable")) {
      column_format <- "{reference}:{treatment}-beta"
    }
  }

  for (treatment in treatments) {
    treatment_index <- treatment_ids$Number[treatment_ids$Label == treatment]
    for (study in rownames(contributions)) {
      contribution <- contributions[study, glue::glue(column_format)]
      if (contribution == 0) {
        next
      }

      if (all(c(treatment, reference) %in% FindAllTreatments(data, treatment_ids, study))) {
        direct_contributions[study, treatment] <- contribution
      } else {
        indirect_contributions[study, treatment] <- contribution
      }

      treatment_effect <- d0$TE[d0$treat1 == reference_index & d0$treat2 == treatment_index & d0$Study == study]

      if (length(treatment_effect) != 0) {
        relative_effects[study, treatment] <- treatment_effect
      }
    }
  }

  covariate_values <- data[[covariate_title]][match(studies, data$Study)]
  names(covariate_values) <- studies

  return(
    list(
      direct = direct_contributions,
      indirect = indirect_contributions,
      relative_effect = relative_effects,
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
#' @param study_or_arm_level "study" for arm-level contributions, "comparison" for basic-comparison-level contributions.
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
CreateContributionMatrix <- function(data, treatment_ids, outcome_type, outcome_measure, effects_type, std_dev_d = NULL, cov_parameters, cov_centre = NULL, std_dev_beta = NULL, study_or_arm_level, absolute_or_percentage, basic_or_all_parameters = "basic", weight_or_contribution, full_output = FALSE){

  #Create a text version of the treatment
  data$Treatment <- treatment_ids$Label[match(data$T, treatment_ids$Number)]
  #The unique studies
  studies <- unique(data$Study)
  #The unique treatments
  treatments <- treatment_ids$Label
  #Unduplicated covariate values (one per study)
  covariate <- unique(dplyr::select(data, starts_with(c("Study", "covar."))))$covar.
  if (any(is.na(covariate))) {
    stop("Missing covariate values are not allowed")
  }
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
  Z <- CreateZMatrix(studies = studies,
                     treatments = treatments,
                     cov_parameters = cov_parameters)

  if (effects_type == "fixed") {
    if (cov_parameters %in% c("unrelated", "shared")) {
      contribution <- .WeightMatrixFixedUnrelatedShared(X = X, V = V, Z = Z,
                                                        basic_or_all_parameters = basic_or_all_parameters)
    } else if (cov_parameters == "exchangeable") {
      contribution <- .WeightMatrixFixedExchangeable(X = X, V = V, Z = Z,
                                                     basic_or_all_parameters = basic_or_all_parameters,
                                                     studies = studies,
                                                     treatments = treatments,
                                                     std_dev_beta = std_dev_beta)
    }
  } else if (effects_type == "random") {

    if (is.null(std_dev_d)) {
      stop("Must specify 'std_dev_d' when effects_type == 'random'")
    }

    Lambda_tau <- CreateLambdaTauMatrix(data = data, studies = studies, treatments = treatments, std_dev_d = std_dev_d)

    if (cov_parameters %in% c("unrelated", "shared")) {
      contribution <- .WeightMatrixRandomUnrelatedShared(X = X, V = V, Z = Z,
                                                         Lambda_tau = Lambda_tau,
                                                         basic_or_all_parameters = basic_or_all_parameters)
    } else if (cov_parameters == "exchangeable") {
      contribution <- .WeightMatrixRandomExchangeable(X = X, V = V, Z = Z,
                                                      Lambda_tau = Lambda_tau,
                                                      basic_or_all_parameters = basic_or_all_parameters,
                                                      studies = studies,
                                                      treatments = treatments,
                                                      std_dev_beta = std_dev_beta)
    }

  } else{
    stop("effects_type must be 'fixed' or 'random'")
  }

  if (weight_or_contribution == "contribution") {
    outcomes <- GetOutcomesAndVariances(data = data,
                                        treatments = treatments,
                                        outcome_type = outcome_type,
                                        outcome_measure = outcome_measure)$Outcome
    #Multiply the n-th column of 'contribution' by the n-th element of 'outcomes'
    contribution <- t(t(contribution) * outcomes)
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
  if (study_or_arm_level == "study") {
    #Number of studies
    n_studies <- length(studies)
    #Number of arms per study
    n_arms <- table(data$Study)

    #Local function to extract the studies from a vector of strings that are in the format "(Study)Treatment"
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
      contribution_study[which(rownames(contribution_study) == study), ] <- colSums(contribution_output[which(rownames(contribution_output) == study), ])
    }
    contribution_output <- contribution_study
  } else if (study_or_arm_level != "arm") {
    stop("study_or_arm_level must be 'study' or 'arm'")
  }

  #For some configurations the column names are not carried through, so put them back in
  if (is.null(colnames(contribution_output))) {
    colnames(contribution_output) <- colnames(X)
  }

  if (!full_output) {
    return(contribution_output)
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

#' Calculate the confidence regions within direct evidence for the regression model.
#'
#' @param model_output Return from `CovariateModelOutput()`.
#'
#' @return list of confidence region objects and confidence interval objects.
#' Regions cover treatments with a non-zero covariate range of direct contributions,
#' intervals cover treatments with a single covariate value from direct contributions.
#' Any treatment with no direct contributions will not be present in either list.
#' Each is a list of data frames for each treatment name. Each data frame contains 3 columns:
#' - cov_value: The covariate value at which the confidence region is calculated.
#' - lower: the 2.5% quantile.
#' - upper: the 97.5% quantile.
#' Each data frame in "regions" contains 11 rows creating a 10-polygon region.
#' Each data frame in "intervals" contains a single row at the covariate value of that single contribution.
CalculateConfidenceRegions <- function(model_output) {
  mtc_results <- model_output$mtcResults
  reference_name <- model_output$reference_name

  confidence_regions <- list()
  confidence_intervals <- list()

  for (treatment_name in model_output$comparator_names) {
    parameter_name <- glue::glue("d.{reference_name}.{treatment_name}")
    cov_min <- model_output$covariate_min[treatment_name]
    cov_max <- model_output$covariate_max[treatment_name]

    if (is.na(cov_min)) {
      confidence_intervals[[treatment_name]] <- data.frame(cov_value = NA, lower = NA, upper = NA)
      confidence_regions[[treatment_name]] <- data.frame(cov_value = NA, lower = NA, upper = NA)
    } else if (cov_min == cov_max) {
      interval <- .FindConfidenceInterval(mtc_results, reference_name, cov_min, parameter_name)
      df <- data.frame(cov_value = cov_min, lower = interval["2.5%"], upper = interval["97.5%"])

      # Strip out the row names
      rownames(df) <- NULL

      # Add to regions list
      confidence_intervals[[treatment_name]] <- df
      confidence_regions[[treatment_name]] <- data.frame(cov_value = NA, lower = NA, upper = NA)
    } else {
      df <- data.frame()
      for (cov_value in seq(from = cov_min, to = cov_max, length.out = 11)) {
        interval <- .FindConfidenceInterval(mtc_results, reference_name, cov_value, parameter_name)
        df <- rbind(
          df,
          data.frame(cov_value = cov_value, lower = interval["2.5%"], upper = interval["97.5%"])
        )
      }

      # Strip out the row names
      rownames(df) <- NULL

      # Add to regions list
      confidence_regions[[treatment_name]] <- df
      confidence_intervals[[treatment_name]] <- data.frame(cov_value = NA, lower = NA, upper = NA)
    }
  }

  return(
    list(
      regions = confidence_regions,
      intervals = confidence_intervals
    )
  )
}

#' Find the confidence interval at a given covariate value.
#'
#' @param mtc_results Meta-analysis object from which to find confidence interval.
#' @param reference_name Name of reference treatment.
#' @param cov_value Covariate value at which to find the confidence interval.
#' @param parameter_name Name of the parameter for which to get the confidence interval.
#'
#' @return Named vector of "2.5%" and "97.5" quantiles.
.FindConfidenceInterval <- function(mtc_results, reference_name, cov_value, parameter_name) {
  rel_eff <- gemtc::relative.effect(mtc_results, reference_name, covariate = cov_value)
  rel_eff_summary <- summary(rel_eff)
  return(rel_eff_summary$summaries$quantiles[parameter_name, c("2.5%", "97.5%")])
}


# Zero-width space before "Other" to pin it to the first item in the list
regression_ghost_name = "\"Other\""

#' Create a composite meta-regression plot which comprises plots showing direct and indirect evidence.
#'
#' @param model_output GEMTC model results found by calling `CovariateModelOutput()`.
#' @param treatment_df Data frame containing treatment IDs (Number), sanitised names (Label), and original names (RawLabel).
#' @param outcome_measure Outcome measure of analysis (OR, RR, RD or MD)
#' @param comparators Vector of names of comparison treatments to plot in colour.
#' @param contribution_matrix Contributions from function `CalculateContributions()`.
#' @param contribution_type Type of contribution, used to calculate sizes for the study contribution circles.
#' @param confidence_regions List of confidence region data frames from function `CalculateConfidenceRegions()`.
#' @param include_covariate TRUE if the value of the covariate is to be plotted as a vertical line. Defaults to FALSE.
#' @param include_ghosts TRUE if all other comparator studies should be plotted in grey in the background of the plot. Defaults to FALSE.
#' @param include_extrapolation TRUE if regression lines should be extrapolated beyond the range of the given data. These will appear as dashed lines.
#' Defaults to FALSE.
#' @param include_confidence TRUE if the confidence regions should be plotted for the specified comparators. These will be partially transparent regions.
#' Defaults to FALSE.
#' @param confidence_opacity The opacity of the confidence regions. Can be any value between 0 and 1, inclusive. Defaults to 0.2.
#' @param include_contributions TRUE if the contributions should be plotted as a circle for each study. Defaults to TRUE.
#' @param contribution_multiplier Factor by which to scale the sizes of the study contribution circles. Defaults to 1.0.
#' @param legend_position String informing the position of the legend. Acceptable values are:
#' - "BR" - Bottom-right of the plot area
#' - "BL" - Bottom-left of the plot area
#' - "TR" - Top-right of the plot area
#' - "TL" - Top-left of the plot area
#'
#' @return Created ggplot2 object.
CreateCompositeMetaRegressionPlot <- function(
    model_output,
    treatment_df,
    outcome_measure,
    comparators,
    contribution_matrix,
    contribution_type,
    confidence_regions,
    include_covariate = FALSE,
    include_ghosts = FALSE,
    include_extrapolation = FALSE,
    include_confidence = FALSE,
    confidence_opacity = 0.2,
    include_contributions = TRUE,
    contribution_multiplier = 1.0,
    legend_position = "BR") {

  direct_plot <- CreateMainRegressionPlot(
    model_output = model_output,
    treatment_df = treatment_df,
    outcome_measure = outcome_measure,
    comparators = comparators,
    contribution_matrix = contribution_matrix,
    contribution_type = contribution_type,
    confidence_regions = confidence_regions,
    include_covariate = include_covariate,
    include_ghosts = include_ghosts,
    include_extrapolation = include_extrapolation,
    include_confidence = include_confidence,
    confidence_opacity = confidence_opacity,
    include_contributions = include_contributions,
    contribution_multiplier = contribution_multiplier,
    legend_position = legend_position
  )

  if (!include_contributions) {
    return(direct_plot)
  }

  indirect_plot <- CreateIndirectContributionPlot(
    model_output = model_output,
    treatment_df = treatment_df,
    comparators = comparators,
    contribution_matrix = contribution_matrix,
    contribution_type = contribution_type,
    include_covariate = include_covariate,
    include_ghosts = include_ghosts,
    contribution_multiplier = contribution_multiplier
  )

  # Find the x-axis ranges of the 2 plots
  x_range_1 <- ggplot_build(direct_plot)$layout$panel_params[[1]]$x.range
  x_range_2 <- ggplot_build(indirect_plot)$layout$panel_params[[1]]$x.range

  # Find the largest range covered by either plot
  x_min = min(x_range_1[1], x_range_2[1])
  x_max = max(x_range_1[2], x_range_2[2])

  # Scale both plots to cover the full x-axis range
  direct_plot <- direct_plot + coord_cartesian(xlim = c(x_min, x_max))
  indirect_plot <- indirect_plot + coord_cartesian(xlim = c(x_min, x_max))

  # Create composite plot by placing the indirect plot atop the direct plot
  plot <- ggpubr::ggarrange(
    indirect_plot, direct_plot,
    heights = c(1, 4),
    align = "v",
    ncol = 1
  )

  return(plot)
}


#' Create a covariate regression plot where multiple comparisons can be plotted, and the contributions from each study are shown as circles.
#'
#' @param model_output GEMTC model results found by calling `CovariateModelOutput()`.
#' @param treatment_df Data frame containing treatment IDs (Number), sanitised names (Label), and original names (RawLabel).
#' @param outcome_measure Outcome measure of analysis (OR, RR, RD, MD)
#' @param comparators Vector of names of comparison treatments to plot in colour.
#' @param contribution_matrix Contributions from function `CalculateContributions()`.
#' @param contribution_type Name of the type of contribution, used to calculate sizes for the study contribution circles.
#' @param confidence_regions List of confidence region data frames from function `CalculateConfidenceRegions()`.
#' @param include_covariate TRUE if the value of the covariate is to be plotted as a vertical line. Defaults to FALSE.
#' @param include_ghosts TRUE if all other comparator studies should be plotted in grey in the background of the plot. Defaults to FALSE.
#' @param include_extrapolation TRUE if regression lines should be extrapolated beyond the range of the given data. These will appear as dashed lines.
#' Defaults to FALSE.
#' @param include_confidence TRUE if the confidence regions should be plotted for the specified comparators. These will be partially transparent regions.
#' Defaults to FALSE.
#' @param confidence_opacity The opacity of the confidence regions. Can be any value between 0 and 1, inclusive. Defaults to 0.2.
#' @param include_contributions TRUE if the contributions should be plotted as a circle for each study. Defaults to TRUE.
#' @param contribution_multiplier Multiplication factor by which to scale the sizes of the study contribution circles. Defaults to 1.0.
#' @param legend_position String informing the position of the legend. Acceptable values are:
#' - "BR" - Bottom-right of the plot area
#' - "BL" - Bottom-left of the plot area
#' - "TR" - Top-right of the plot area
#' - "TL" - Top-left of the plot area
#' Defaults to "BR"
#'
#' @return Created ggplot2 object.
CreateMainRegressionPlot <- function(
    model_output,
    treatment_df,
    outcome_measure,
    comparators,
    contribution_matrix,
    contribution_type,
    confidence_regions,
    include_covariate = FALSE,
    include_ghosts = FALSE,
    include_extrapolation = FALSE,
    include_confidence = FALSE,
    confidence_opacity = 0.2,
    include_contributions = TRUE,
    contribution_multiplier = 1.0,
    legend_position = "BR") {

  reference = model_output$reference_name
  comparators <- sort(comparators)
  all_comparators <- model_output$comparator_names

  # Set up basic plot
  plot <- .SetupMainRegressionPlot(
    reference = treatment_df$RawLabel[treatment_df$Label == reference],
    comparators = comparators,
    outcome_measure = outcome_measure,
    include_ghosts = include_ghosts && length(comparators) < length(all_comparators),
    confidence_opacity = confidence_opacity,
    legend_position = legend_position
  )

  # Plot the ghost regression lines for the comparators
  if (include_ghosts) {
    ghosts <-  all_comparators[!all_comparators %in% comparators]

    if (include_contributions) {
      plot <- .PlotDirectContributionCircles(plot, model_output, treatment_df, reference, ghosts, contribution_matrix, contribution_type, contribution_multiplier, ghosted = TRUE)
    }
    plot <- .PlotRegressionLines(plot, model_output, contribution_matrix, treatment_df, reference, ghosts, include_extrapolation, ghosted = TRUE)
  }

  if (length(comparators) > 0) {
    if (include_confidence) {
      plot <- .PlotConfidenceRegions(plot, confidence_regions, comparators, confidence_opacity)
    }
    if (include_contributions) {
      plot <- .PlotDirectContributionCircles(plot, model_output, treatment_df, reference, comparators, contribution_matrix, contribution_type, contribution_multiplier)
    }
    plot <- .PlotRegressionLines(plot, model_output, contribution_matrix, treatment_df, reference, comparators, include_extrapolation)
  }

  # Plot a vertical line at the covariate value
  if (include_covariate) {
    plot <- plot +
      geom_vline(
        xintercept = model_output$covariate_value,
        color = "black"
      )
  }

  return(plot)
}

#' Setup the main components of the plot panel.
#'
#' @param reference Name of the reference treatment.
#' @param comparators Vector of names of comparison treatments to plot.
#' @param outcome_measure Outcome measure of analysis (OR, RR, RD, MD)
#' @param include_ghosts TRUE if all other comparator studies should be plotted in grey in the background of the plot. Defaults to FALSE.
#' @param confidence_opacity The opacity of the confidence regions. Can be any value between 0 and 1, inclusive. Defaults to 0.2.
#' @param legend_position String informing the position of the legend. Acceptable values are:
#' - "BR" - Bottom-right of the plot area
#' - "BL" - Bottom-left of the plot area
#' - "TR" - Top-right of the plot area
#' - "TL" - Top-left of the plot area
#'
#' @return Created ggplot2 object.
.SetupMainRegressionPlot <- function(reference, comparators, outcome_measure, include_ghosts, confidence_opacity, legend_position) {
  # Set up basic plot
  plot <- ggplot() +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "#777777", fill = NA, linewidth = 2),

      axis.line = element_blank(),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),

      legend.position = c(
        ifelse(grepl("L", legend_position), .01, .99),
        ifelse(grepl("B", legend_position), .02, .99)
      ),
      legend.justification = c(
        ifelse(grepl("L", legend_position), "left", "right"),
        ifelse(grepl("B", legend_position), "bottom", "top")
      ),

      legend.margin = margin(6, 6, 6, 6),
      legend.box.background = element_rect(colour = "black", linewidth = 0.5, fill = "#ffffffaa"),
      legend.text = element_text(size = 12)
    ) +
    xlab("Covariate Value") +
    ylab(glue::glue("Relative Effect vs {reference} ({outcome_measure})"))

  # Log scale for OR & RR
  if (outcome_measure %in% c("OR", "RR")) {
    plot <- plot +
      scale_y_continuous(labels = function(x) { signif(exp(x), digits = 2) })
  }

  plot <- SetupRegressionPlotColours(
    plot = plot,
    comparators = comparators,
    include_ghosts = include_ghosts,
    include_confidence = TRUE,
    confidence_opacity = confidence_opacity
  )

  return(plot)
}

#' Plot the confidence regions and intervals on the plot.
#'
#' @param plot object to which to add elements.
#' @param confidence_regions List of confidence region data frames from function `CalculateConfidenceRegions()`.
#' @param comparators Vector of names of comparison treatments to plot.
#'
#' @return The modified ggplot2 object.
.PlotConfidenceRegions <- function(plot, confidence_regions, comparators, confidence_opacity) {
  regions <- .FormatRegressionConfidenceRegion(confidence_regions$regions, comparators)
  intervals <- .FormatRegressionConfidenceRegion(confidence_regions$intervals, comparators)

  plot <- plot +
    geom_ribbon(
      data = regions,
      mapping = aes(
        x = covariate_value,
        ymin = y_min,
        ymax = y_max,
        fill = Treatment
      ),
      show.legend = FALSE
    ) +
    geom_linerange(
      data = intervals,
      mapping = aes(
        x = covariate_value,
        ymin = y_min,
        ymax = y_max,
        color = Treatment
      ),
      linewidth = 2,
      alpha = confidence_opacity,
      show.legend = FALSE
    )

  return(plot)
}

#' Plot the contribution circles for direct evidence on the plot.
#'
#' @param plot ggplot2 object to which to add elements.
#' @param model_output GEMTC model results found by calling `CovariateModelOutput()`.
#' @param treatment_df Data frame containing treatment IDs (Number), sanitised names (Label), and original names (RawLabel).
#' @param reference Name of reference treatment.
#' @param comparators Vector of names of comparison treatments to plot.
#' @param contribution_matrix Contributions from function `CalculateContributions()`.
#' @param contribution_type Name of the type of contribution, used to calculate sizes for the study contribution circles.
#' @param contribution_multiplier Multiplication factor by which to scale the sizes of the study contribution circles. Defaults to 1.0.
#' @param ghosted TRUE if studies should be plotted in grey. Defaults to FALSE.
#'
#' @return The modified ggplot2 object.
.PlotDirectContributionCircles <- function(plot, model_output, treatment_df, reference, comparators, contribution_matrix, contribution_type, contribution_multiplier, ghosted = FALSE) {
  contributions = .FindDirectRegressionContributions(model_output, reference, comparators, contribution_matrix, contribution_type)

  if (nrow(contributions) == 0) {
    return(plot)
  }

  contributions$Treatment <- sapply(contributions$Treatment, function(treatment) { treatment_df$RawLabel[treatment_df$Label == treatment] })

  if (ghosted) {
    contributions$Treatment <- rep(regression_ghost_name, length(contributions$Treatment))
  }

  plot <- plot +
    geom_point(
      data = contributions,
      mapping = aes(
        x = covariate_value,
        y = relative_effect,
        color = Treatment,
        stroke = 1.5
      ),
      shape = 1,
      size = contributions$contribution * contribution_multiplier,
      show.legend = FALSE
    )

  return(plot)
}

#' Plot the regression lines on the plot.
#'
#' @param plot ggplot2 object to which to add elements.
#' @param model_output GEMTC model results found by calling `CovariateModelOutput()`.
#' @param contribution_matrix Contributions from function `CalculateContributions()`.
#' @param treatment_df Data frame containing treatment IDs (Number), sanitised names (Label), and original names (RawLabel).
#' @param reference Name of reference treatment.
#' @param comparators Vector of names of comparison treatments to plot.
#' @param extrapolate TRUE if regression lines should be extrapolated beyond the range of the data. These will be plotted as dashed lines.
#' Defaults to FALSE.
#' @param ghosted TRUE if studies should be plotted in grey. Defaults to FALSE.
#'
#' @return The modified ggplot2 object.
.PlotRegressionLines <- function(plot, model_output, contribution_matrix, treatment_df, reference, comparators, extrapolate, ghosted = FALSE) {
  # Create data frame
  lines = data.frame(
    Treatment = sapply(comparators, function(comparator) { treatment_df$RawLabel[treatment_df$Label == comparator] }),
    intercept = model_output$intercepts[comparators],
    slope = model_output$slopes[comparators],
    start_x = model_output$covariate_min[comparators],
    end_x = model_output$covariate_max[comparators]
  )

  if (ghosted) {
    lines$Treatment <- rep(regression_ghost_name, length(lines$Treatment))
  }

  if (extrapolate) {
    # Dashed lines outside of data ranges
    plot <- plot +
      geom_abline(
        data = lines,
        mapping = aes(
          intercept = intercept,
          slope = slope,
          color = Treatment
        ),
        linewidth = 1,
        linetype = "dashed",
        show.legend = FALSE
      )
  }

  # Solid lines within data ranges
  plot <- plot +
    geom_segment(
      data = lines,
      mapping = aes(
        x = start_x,
        y = intercept + slope * start_x,
        xend = end_x,
        yend = intercept + slope * end_x,
        color = Treatment
      ),
      linewidth = 1.2,
      show.legend = !ghosted
    )

  return(plot)
}

#' Find the contributions to the regression analysis.
#'
#' @param model_output GEMTC model results found by calling `CovariateModelOutput()`.
#' @param reference Name of reference treatment.
#' @param comparator Name of comparison treatment for which to find the contributions.
#' @param contribution_matrix Contributions from function `CalculateContributions()`.
#' @param contribution_type Name of the type of contribution to find.
#'
#' @return Data frame containing contribution details. Each row represents a study contributing to a given treatment. Columns are:
#' - Treatment: The treatment for which this contribution relates.
#' - covariate_value: Value of the covariate for this study.
#' - relative_effect Relative effect for this study.
#' - contribution: Size of contribution for this study.
.FindDirectRegressionContributions <- function(model_output, reference, comparator, contribution_matrix, contribution_type) {
  treatments <- c()
  covariate_values <- c()
  relative_effects <- c()
  contributions <- c()

  for (treatment in comparator) {
    for (study in row.names(contribution_matrix$direct)) {
      direct_contribution <- contribution_matrix$direct[study, treatment]

      if (is.na(direct_contribution)) {
        next
      }

      treatments <- c(treatments, treatment)
      covariate_values <- c(covariate_values, contribution_matrix$covariate_value[study])
      relative_effects <- c(relative_effects, contribution_matrix$relative_effect[study, treatment])
      contributions <- c(contributions, direct_contribution)
    }
  }

  return(
    data.frame(
      Treatment = treatments,
      covariate_value = covariate_values,
      relative_effect = relative_effects,
      contribution = contributions
    )
  )
}

#' Format the confidence regions for the regression analysis into a plottable data frame.
#'
#' @param confidence_regions List of confidence region data frames from function `CalculateConfidenceRegions()`.
#' @param comparator Name of comparison treatment for which to find the contributions.
#'
#' @return Data frame containing contribution details. Each row represents a confidence interval at a specific covariate value, for a given treatment. Columns are:
#' - Treatment: The treatment for which this confidence interval relates.
#' - covariate_value: Value of the covariate for this interval
#' - y_min Relative effect of the lower end of this interval
#' - y_max: Relative effect of the upper end of this interval
.FormatRegressionConfidenceRegion <- function(confidence_regions, comparator) {

  treatments <- c()
  covariate_values <- c()
  y_mins <- c()
  y_maxs <- c()

  for (treatment_name in comparator) {
    treatment_covariate_values <- confidence_regions[[treatment_name]]$cov_value
    treatment_y_mins <- confidence_regions[[treatment_name]]$lower
    treatment_y_maxs <- confidence_regions[[treatment_name]]$upper

    treatments <- c(treatments, rep(treatment_name, length(treatment_covariate_values)))
    covariate_values <- c(covariate_values, treatment_covariate_values)
    y_mins <- c(y_mins, treatment_y_mins)
    y_maxs <- c(y_maxs, treatment_y_maxs)
  }

  confidence_df <- data.frame(
    Treatment = treatments,
    covariate_value = covariate_values,
    y_min = y_mins,
    y_max = y_maxs
  )

  # Return an empty data frame with correct column names if all of the rows are NA
  if (all(is.na(confidence_df$covariate_value))) {
    return(data.frame(matrix(nrow = 0, ncol = 4, dimnames = list(NULL, c("Treatment", "covariate_value", "y_min", "y_max")))))
  }

  return(confidence_df)
}

#' Setup the main components of the plot panel.
#'
#' @param reference Name of the reference treatment.
#' @param comparators Vector of names of comparison treatments to plot.
#' @param include_ghosts TRUE if all other comparator studies should be plotted in grey in the background of the plot.
#' @param include_confidence TRUE if all other comparator studies should be plotted in grey in the background of the plot.
#' @param confidence_opacity The opacity of the confidence regions. Can be any value between 0 and 1, inclusive. Defaults to 1.
#'
#' @return Created ggplot2 object.
SetupRegressionPlotColours <- function(plot, comparators, include_ghosts, include_confidence, confidence_opacity = 1) {
  # Ensure that enough colours are always provided, by cycling the given colours
  base_colours <- c("#bb0000", "#bba000", "#00bb00", "#00bbbb", "#0000bb", "#bb00bb",
                    "#ff5555", "#ffa000", "#44ff44", "#55ffff", "#7744ff", "#ff00ff")
  colours <- rep(base_colours, ceiling(length(comparators) / length(colours)))[1:length(comparators)]

  # Set the colours
  if (include_ghosts) {
    colours <- c("#eeeeee", colours)
  }

  plot <- plot +
    scale_colour_manual(values = colours)

  # Only include fills if confidence regions included
  if (include_confidence) {
    opacity_hex = format(
      as.hexmode(as.integer(confidence_opacity * 255)),
      width = 2
    )
    fills <- paste0(base_colours, opacity_hex)
    fills <- rep(fills, ceiling(length(comparators) / length(fills)))[1:length(comparators)]

    plot <- plot +
      scale_fill_manual(values = fills, guide = "none")
  }

  return(plot)
}


#' Create a covariate regression plot where multiple comparisons can be plotted, and the contributions from each study are shown as circles.
#'
#' @param model_output GEMTC model results found by calling `CovariateModelOutput()`.
#' @param treatment_df Data frame containing treatment IDs (Number), sanitised names (Label), and original names (RawLabel).
#' @param comparators Vector of names of comparison treatments to plot in colour.
#' @param contribution_matrix Contributions from function `CalculateContributions()`.
#' @param contribution_type Type of contribution, used to calculate sizes for the study contribution circles.
#' @param include_covariate TRUE if the value of the covariate is to be plotted as a vertical line. Defaults to FALSE.
#' @param include_ghosts TRUE if all other comparator studies should be plotted in grey in the background of the plot. Defaults to FALSE.
#' @param contribution_multiplier Factor by which to scale the sizes of the study contribution circles. Defaults to 1.0.
#'
#' @return Created ggplot2 object.
CreateIndirectContributionPlot <- function(
    model_output,
    treatment_df,
    comparators,
    contribution_matrix,
    contribution_type,
    include_covariate = FALSE,
    include_ghosts = FALSE,
    contribution_multiplier = 1.0) {

  reference = model_output$reference_name
  comparators <- sort(comparators)
  all_comparators <- model_output$comparator_names

  # Set up basic plot
  plot <- .SetupIndirectContributionPlot(
    reference = treatment_df$RawLabel[treatment_df$Label == reference],
    comparators = comparators,
    include_ghosts = include_ghosts && length(comparators) < length(all_comparators)
  )

  # Plot the ghost regression lines for the comparators
  if (include_ghosts) {
    ghosts <-  all_comparators[!all_comparators %in% comparators]
    plot <- .PlotIndirectContributionCircles(plot, model_output, treatment_df, reference, ghosts, contribution_matrix, contribution_type, contribution_multiplier, ghosted = TRUE)
  }

  if (length(comparators) > 0) {
    plot <- .PlotIndirectContributionCircles(plot, model_output, treatment_df, reference, comparators, contribution_matrix, contribution_type, contribution_multiplier)
  }

  if (include_covariate) {
    plot <- plot +
      geom_vline(
        xintercept = model_output$covariate_value,
        color = "black"
      )
  }

  return(plot)
}

#' Setup the main components of the plot panel.
#'
#' @param reference Name of the reference treatment.
#' @param comparators Vector of names of comparison treatments to plot.
#' @param include_ghosts TRUE if all otherc omparator studies should be plotted in grey in the background of the plot. Defaults to FALSE.
#'
#' @return Created ggplot2 object.
.SetupIndirectContributionPlot <- function(reference, comparators, include_ghosts) {
  # Set up basic plot
  plot <- ggplot() +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "#777777", fill = NA, linewidth = 2),

      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 14)
    ) +
    ylab("Indirect\nEvidence")

  plot <- SetupRegressionPlotColours(
    plot = plot,
    comparators = comparators,
    include_ghosts = include_ghosts,
    include_confidence = FALSE
  )

  return(plot)
}

#' Plot the contribution circles on the plot.
#'
#' @param plot object to which to add elements.
#' @param model_output GEMTC model results found by calling `CovariateModelOutput()`.
#' @param treatment_df Data frame containing treatment IDs (Number), sanitised names (Label), and original names (RawLabel).
#' @param reference Name of reference treatment.
#' @param comparators Vector of names of comparison treatments to plot.
#' @param contribution_type Type of contribution, used to calculate sizes for the study contribution circles.
#' @param contribution_multiplier Factor by which to scale the sizes of the study contribution circles. Defaults to 1.0.
#' @param ghosted TRUE if studies should be plotted in grey. Defaults to FALSE.
#'
#' @return The modified ggplot2 object.
.PlotIndirectContributionCircles <- function(plot, model_output, treatment_df, reference, comparators, contribution_matrix, contribution_type, contribution_multiplier, ghosted = FALSE) {
  contributions = .FindIndirectRegressionContributions(model_output, reference, comparators, contribution_matrix, contribution_type)

  if (nrow(contributions) == 0) {
    return(plot)
  }

  contributions$Treatment <- sapply(contributions$Treatment, function(treatment) { treatment_df$RawLabel[treatment_df$Label == treatment] })

  if (ghosted) {
    contributions$Treatment <- rep(regression_ghost_name, length(contributions$Treatment))
  }

  plot <- plot +
    geom_point(
      data = contributions,
      mapping = aes(
        x = covariate_value,
        y = 0,
        color = Treatment,
        stroke = 1.5
      ),
      shape = 1,
      size = contributions$contribution * contribution_multiplier,
      show.legend = FALSE
    )

  return(plot)
}

#' Find the contributions to the regression analysis.
#'
#' @param model_output GEMTC model results found by calling `CovariateModelOutput()`.
#' @param reference Name of reference treatment.
#' @param comparator Name of comparison treatment for which to find the contributions.
#' @param contribution_type Type of contribution to find.
#'
#' @return Data frame containing contribution details. Each row represents a study contributing to a given treatment. Columns are:
#' - Treatment: The treatment for which this contribution relates.
#' - covariate_value: Value of the covariate for this study.
#' - contribution: Size of contribution for this study.
.FindIndirectRegressionContributions <- function(model_output, reference, comparator, contribution_matrix, contribution_type) {
  treatments <- c()
  covariate_values <- c()
  contributions <- c()

  for (treatment in comparator) {
    for (study in row.names(contribution_matrix$indirect)) {
      indirect_contribution <- contribution_matrix$indirect[study, treatment]

      if (is.na(indirect_contribution)) {
        next
      }

      treatments <- c(treatments, treatment)
      covariate_values <- c(covariate_values, contribution_matrix$covariate_value[study])
      contributions <- c(contributions, indirect_contribution)
    }
  }

  return(
    data.frame(
      Treatment = treatments,
      covariate_value = covariate_values,
      contribution = contributions
    )
  )
}


