#' Converts long data to wide if necessary, then applies freq_wrap().
#'
#' @param non_covariate_data Input dataset with any covariates removed.
#' @inheritParams common_params
#' @return List containing:
#'  \item{net1}{list. NMA results from netmeta::netmeta()}
#'  \item{lstx}{character. Vector of treatment labels}
#'  \item{ntx}{numeric. Number of treatments}
#'  \item{d0}{dataframe.Data in contrast form}
#'  \item{d1}{dataframe.Same as 'd0' but with treatment labels}
#' @export
frequentist <- function(non_covariate_data, outcome, treatment_df, outcome_measure, model_type, reference_treatment) {
  if (FindDataShape(non_covariate_data) == "long") {
    wide_data <- LongToWide(long_data = non_covariate_data, outcome = outcome)
  } else {
    wide_data <- non_covariate_data
  }

  ntx <- nrow(treatment_df)
  d0 <- contrastform.df(wide_data, outcome_measure, outcome)    # transform data to contrast form
  d1 <- labelmatching.df(d1 = d0, ntx = ntx, treatment_df = treatment_df) #matching treatment labels to treatment code
  net1 <- freq.df(model_type = model_type, outcome_measure = outcome_measure, dataf = d1, reference_treatment = reference_treatment) # NMA of all studies

  return(list(
    net1 = net1,
    lstx = treatment_df$Label,
    ntx = ntx,
    d0 = d0,
    d1 = d1))

}

#' Puts data in contrast form using netmeta::pairwise().
#'
#' @param wide_data non-covariate data in a wide format
#' @inheritParams common_params
#' @return Input data in contrast form.
contrastform.df <- function(wide_data, outcome_measure, outcome) {

  #Create a list of columns of the variables to be passed to meta::pairwise()
  treat_list <- CreateListOfWideColumns(wide_data = wide_data, column_prefix = "T")
  n_list <-  CreateListOfWideColumns(wide_data = wide_data, column_prefix = "N")

  if (outcome == 'Continuous') {

    mean_list <-  CreateListOfWideColumns(wide_data = wide_data, column_prefix = "Mean")
    sd_list <-  CreateListOfWideColumns(wide_data = wide_data, column_prefix = "SD")

    d1 <- meta::pairwise(treat = treat_list,
                         n = n_list,
                         mean = mean_list,
                         sd = sd_list,
                         data = wide_data,
                         sm = outcome_measure,
                         studlab = wide_data$Study)
  } else if (outcome == 'Binary') {

    event_list <-  CreateListOfWideColumns(wide_data = wide_data, column_prefix = "R")

    d1 <- meta::pairwise(treat = treat_list,
                         event = event_list,
                         n = n_list,
                         data = wide_data,
                         sm = outcome_measure,
                         studlab = wide_data$Study)
  } else {
    stop("outcome must be 'Continuous' or 'Binary'")
  }
  return(d1)
}

#' Adds treatment labels to contrast data.
#'
#' @param d1 Data in contrast form, typically created by contrastform.df().
#' @param ntx = Number of treatments.
#' @inheritParams common_params
#' @return Input data with the columns treat1 and treat2 now labelled.
labelmatching.df <- function(d1, ntx, treatment_df) {

  d1$treat1 <- factor(d1$treat1,
                      levels = 1:ntx,
                      labels = as.character(treatment_df$Label))
  d1$treat2 <- factor(d1$treat2,
                      levels = 1:ntx,
                      labels = as.character(treatment_df$Label))
  return(d1)
}

#' Frequentist NMA.
#'
#' @param dataf Data in contrast form with treatment labels, typically output from labelmatching.df().
#' @inheritParams common_params
#' @return NMA results from netmeta::netmeta().
freq.df <- function(model_type, outcome_measure, dataf, reference_treatment) {
  net1 <- netmeta::netmeta(TE = dataf$TE, seTE = dataf$seTE, treat1 = dataf$treat1, treat2 = dataf$treat2, studlab = dataf$studlab, subset=NULL,
                  sm = outcome_measure, level = 0.95, level.ma = 0.95, random = (model_type == "random"),
                  common = (model_type == "fixed"), reference.group = reference_treatment, all.treatments = NULL, seq = NULL,
                  tau.preset = NULL, tol.multiarm = 0.05, tol.multiarm.se = 0.2, warn = TRUE)
  return(net1)
}

