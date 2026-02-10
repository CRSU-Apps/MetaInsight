#' @title Common parameters
#' @name common_params
#' @param treatments dataframe. Treatments
#' @param outcome character. Outcome type for the dataset. Either `binary` or
#' `continuous`.
#' @param outcome_measure character. Outcome measure of the dataset. Either
#' `OR`, `RR` or `RD` when `outcome` is `binary` or `MD` or `SMD` when
#' `outcome` is `continuous`
#' @param connected_data dataframe. Input data set created by `setup_configure()` or `setup_exclude`
#' @param ranking_option character. `good` if the treatment effect is desirable, else `bad`
#' @param reference_treatment character. The reference treatment of the dataset
#' @param effects character. Type of model to fit, either `random` or `fixed`
#' @param configured_data list. Input dataset created by `setup_configure()` or `setup_exclude()`
#' @param seed numeric. Seed used to fit the models.
#' @param xmin numeric. Minimum x-axis value. Default `NULL` in which case it is calculated internally
#' @param xmax numeric. Maximum x-axis value. Default `NULL` in which case it is calculated internally
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
