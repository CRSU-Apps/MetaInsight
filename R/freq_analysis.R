#' @title Fit a frequentist model
#' @description Fits a frequentist model with `netmeta::netmeta()`
#'
#' @param non_covariate_data Input dataset with any covariates removed.
#' @inheritParams common_params
#' @return List containing:
#'  \item{netmeta}{list. NMA results from netmeta::netmeta()}
#'  \item{pairwise}{dataframe. Results from meta::pairwise() but with treatment labels}
#'  \item{pairwise_reversed}{dataframe. pairwise as if the treatments had been the other way round}
#' @keywords internal
#' @export
frequentist <- function(non_covariate_data, outcome, treatments, outcome_measure, effects, reference_treatment) {
  if (FindDataShape(non_covariate_data) == "long") {
    wide_data <- LongToWide(long_data = non_covariate_data, outcome = outcome)
  } else {
    wide_data <- non_covariate_data
  }

  # transform data to contrast form
  pairwise <- contrastform.df(wide_data, outcome_measure, outcome)
  reverse_pairwise <- contrastform.df(wide_data, outcome_measure, outcome, reverse = TRUE)
  # matching treatment labels to treatment code
  labelled_pairwise <- labelmatching.df(pairwise, treatments)
  labelled_reverse_pairwise <- labelmatching.df(reverse_pairwise, treatments)

  netmeta <- netmeta::netmeta(TE = labelled_pairwise$TE,
                          seTE = labelled_pairwise$seTE,
                          treat1 = labelled_pairwise$treat1,
                          treat2 = labelled_pairwise$treat2,
                          studlab = labelled_pairwise$studlab,
                          data = labelled_pairwise,
                          subset=NULL,
                          sm = outcome_measure,
                          level = 0.95,
                          level.ma = 0.95,
                          random = (effects == "random"),
                          common = (effects == "fixed"),
                          reference.group = reference_treatment,
                          all.treatments = NULL,
                          seq = NULL,
                          tau.preset = NULL,
                          tol.multiarm = 0.05,
                          tol.multiarm.se = 0.2,
                          warn = TRUE)

  return(list(
    netmeta = netmeta,
    pairwise = labelled_pairwise,
    reverse_pairwise = labelled_reverse_pairwise))

}

#' Puts data in contrast form using netmeta::pairwise().
#'
#' @param wide_data non-covariate data in a wide format
#' @param reverse Reverses the order of the treatments if TRUE. Defaults to FALSE.
#' @inheritParams common_params
#' @return Input data in contrast form.
#' @noRd
contrastform.df <- function(wide_data, outcome_measure, outcome, reverse = FALSE) {

  #Selects the reverse function 'rev' or the identity function 'c'.
  ReverseFunction <- switch(
    as.character(reverse),
    "TRUE" = rev,
    "FALSE" = c
  )
  
  #Create a list of columns of the variables to be passed to meta::pairwise()
  treat_list <- CreateListOfWideColumns(wide_data = wide_data, column_prefix = "T")
  n_list <-  CreateListOfWideColumns(wide_data = wide_data, column_prefix = "N")

  if (outcome == 'continuous') {

    mean_list <-  CreateListOfWideColumns(wide_data = wide_data, column_prefix = "Mean")
    sd_list <-  CreateListOfWideColumns(wide_data = wide_data, column_prefix = "SD")

    d1 <- meta::pairwise(
      treat = ReverseFunction(treat_list),
      n = ReverseFunction(n_list),
      mean = ReverseFunction(mean_list),
      sd = ReverseFunction(sd_list),
      data = wide_data,
      sm = outcome_measure,
      studlab = wide_data$Study
    )

  } else if (outcome == 'binary') {

    event_list <-  CreateListOfWideColumns(wide_data = wide_data, column_prefix = "R")

    d1 <- meta::pairwise(
      treat = ReverseFunction(treat_list),
      event = ReverseFunction(event_list),
      n = ReverseFunction(n_list),
      data = wide_data,
      sm = outcome_measure,
      studlab = wide_data$Study
    )
  } else {
    stop("outcome must be 'continuous' or 'binary'")
  }
  return(d1)
}

#' Adds treatment labels to contrast data.
#'
#' @param pairwise Data in contrast form, typically created by contrastform.df().
#' @inheritParams common_params
#' @return Input data with the columns treat1 and treat2 now labelled.
#' @noRd
labelmatching.df <- function(pairwise, treatments) {

  ntx <- nrow(treatments)

  pairwise$treat1 <- factor(pairwise$treat1,
                      levels = 1:ntx,
                      labels = as.character(treatments$Label))
  pairwise$treat2 <- factor(pairwise$treat2,
                      levels = 1:ntx,
                      labels = as.character(treatments$Label))
  return(pairwise)
}
