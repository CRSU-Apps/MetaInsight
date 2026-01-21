#' @title Common parameters
#' @name common_params
#' @param treatment_df dataframe. Treatments
#' @param outcome character. Outcome type for the dataset. Either `Binary` or
#' `Continuous`.
#' @param outcome_measure character. Outcome measure of the dataset. Either
#' `OR`, `RR` or `RD` when `outcome` is `Binary` or `MD` or `SMD` when
#' `outcome` is `Continuous`
#' @param ranking_option character. `good` if the treatment effect is desirable, else `bad`
#' @param freq list. NMA results created by freq_wrap().
#' @param reference_treatment character. The reference treatment of the dataset
#' @param model_type character. Type of model to fit, either `random` or `fixed`
#' @param connected_data dataframe. Input data set created by `setup_configure()` or `setup_exclude`
#' @param seed numeric. Seed used to fit the model
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default `NULL`
#' @param async Whether or not the function is being used asynchronously. Default `FALSE`
#' @keywords internal
NULL

#' @title Return svg
#' @name return-svg
#' @keywords internal
#' @return html. Contains the svg string to generate the plot. This can be saved
#' using `write_plot()`.
NULL
