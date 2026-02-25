#' Produce a summary dataframe of the studies using.
#' Inspired by `BUGSnet::net.tab()`
#'
#' @inheritParams common_params
#' @return dataframe of characteristics
#' @export
summary_char <- function(configured_data, logger = NULL) {

  check_param_classes(c("configured_data"), c("configured_data"), logger)

  df <- ReplaceTreatments(configured_data$connected_data, configured_data$treatments)
  connections <- netmeta::netconnection(configured_data$freq$net1, details.disconnected = TRUE)

  arm_summary <- df |>
    dplyr::group_by(.data$Study) |>
    dplyr::summarise(arms = dplyr::n())

  n_interventions <- length(unique(df$T))
  n_studies <- length(unique(df$Study))
  n_patients <- sum(df$N)
  # I think this is right - a fully connected network
  n_pairwise <- (n_interventions * (n_interventions - 1) / 2)
  n_pairwise_direct <- length(connections$comparisons)
  connected <- ifelse(connections$n.subnets == 1, "Yes", "No")
  n_two_arm <- sum(arm_summary$arms == 2)
  n_multi_arm <- sum(arm_summary$arms > 2)

  values <- c(n_interventions,
              n_studies,
              n_patients,
              n_pairwise,
              n_pairwise_direct,
              connected,
              n_two_arm,
              n_multi_arm)

  descriptions <- c("Number of interventions",
                    "Number of studies",
                    "Total number of patients",
                    "Total possible pairwise comparisons",
                    "Total number of pairwise comparisons with direct data",
                    "Is the network connected?",
                    "Number of two-arm studies",
                    "Number of multi-arm studies")

  if (configured_data$outcome == "binary"){
    total_events <- sum(df$R)
    non_zero_events <- df |>
      dplyr::group_by(.data$Study) |>
      dplyr::summarise(min_events = min(.data$R)) |>
      dplyr::filter(min_events > 0) |>
      nrow()

    descriptions <- append(descriptions, c("Total number of events", "Number of studies with no zero events"))
    values <- append(values, c(total_events, non_zero_events))

    trt_summary <- df |>
      dplyr::mutate(outcome = .data$R / .data$N) |>
      dplyr::group_by(.data$T) |>
      dplyr::summarise(n_studies = dplyr::n(),
                       n_events = sum(.data$R),
                       n_patients = sum(.data$N),
                       min_outcome = round(min(.data$R / .data$N), 3),
                       max_outcome = round(max(.data$R / .data$N), 3),
                       average_outcome = round(.data$n_events / .data$n_patients, 3)
      )

    pair_summary <- data.frame()

    for (pair in connections$comparisons) {
      treatments <- unlist(strsplit(pair, ":"))

      row <- df |>
        dplyr::group_by(.data$Study) |>
        dplyr::filter(all(treatments %in% .data$T)) |>
        dplyr::filter(.data$T == treatments[1] | .data$T == treatments[2] ) |>
        dplyr::mutate(treatment_pair = gsub(":", " vs. ", pair)) |>
        dplyr::group_by(.data$treatment_pair) |>
        dplyr::summarise(n_studies = dplyr::n_distinct(.data$Study),
                         n_patients = sum(.data$N),
                         n_outcomes = sum(.data$R),
                         proportion = round(.data$n_outcomes / .data$n_patients, 3)
        )
      pair_summary <- rbind(pair_summary, row)
    }
  }

  if (configured_data$outcome == "continuous"){
    average_outcome <- (sum(df$Mean * df$N) / sum(df$N)) |> round(2)

    descriptions <- append(descriptions, "Average outcome")
    values <- append(values, average_outcome)

    trt_summary <- df |>
      dplyr::group_by(.data$T) |>
      dplyr::summarise(n_studies = dplyr::n(),
                       n_patients = sum(.data$N),
                       min_outcome = min(.data$Mean),
                       max_outcome = max(.data$Mean),
                       average_outcome = round(sum(.data$Mean * .data$N) / sum(.data$N), 2)
      )

    pair_summary <- data.frame()

    for (pair in connections$comparisons) {
      treatments <- unlist(strsplit(pair, ":"))

      row <- df |>
        dplyr::group_by(.data$Study) |>
        dplyr::filter(all(treatments %in% .data$T)) |>
        dplyr::filter(.data$T == treatments[1] | .data$T == treatments[2] ) |>
        dplyr::mutate(treatment_pair = gsub(":", " vs. ", pair)) |>
        dplyr::group_by(.data$treatment_pair) |>
        dplyr::summarise(n_studies = dplyr::n_distinct(.data$Study),
                         n_patients = sum(.data$N))

      pair_summary <- rbind(pair_summary, row)
    }
  }

  network_summary <- data.frame(descriptions, values)

  return(list(network = network_summary,
              treatments = trt_summary,
              pairs = pair_summary))
}
