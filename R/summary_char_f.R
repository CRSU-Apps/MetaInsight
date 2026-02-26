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
    zero_events <- df |>
      dplyr::group_by(.data$Study) |>
      dplyr::summarise(any_zero = any(.data$R == 0),
                       all_zero = all(.data$R == 0))

    any_zero <- sum(zero_events$any_zero)
    all_zero <- sum(zero_events$all_zero)
    no_zero <- n_studies - any_zero

    event_descriptions <- c("Total number of events",
                            "Number of studies with no zero events",
                            "Number of studies with at least one zero event",
                            "Number of studies with all zero events")

    descriptions <- append(descriptions, event_descriptions)
    values <- append(values, c(total_events, no_zero, any_zero, all_zero))

    trt_summary <- df |>
      dplyr::mutate(outcome = .data$R / .data$N) |>
      dplyr::group_by(.data$T) |>
      dplyr::summarise(n_studies = dplyr::n(),
                       n_events = sum(.data$R),
                       n_patients = sum(.data$N),
                       min_outcome = signif(min(.data$R / .data$N), 3),
                       average_outcome = signif(.data$n_events / .data$n_patients, 3),
                       max_outcome = signif(max(.data$R / .data$N), 3)
      )

    colnames(trt_summary) <- c("Treatment",
                               "Number of studies",
                               "Number of events",
                               "Number of patients",
                               "Minimum outcome",
                               "Average outcome",
                               "Maximum outcome")

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
                         proportion = signif(.data$n_outcomes / .data$n_patients, 3)
        )
      pair_summary <- rbind(pair_summary, row)
    }
    colnames(pair_summary) <- c("Treatment comparison",
                                "Number of studies",
                                "Number of patients",
                                "Number of outcomes",
                                "Proportion")
  }

  if (configured_data$outcome == "continuous"){
    average_outcome <- (sum(df$Mean * df$N) / sum(df$N)) |> signif(3)

    descriptions <- append(descriptions, "Average outcome")
    values <- append(values, average_outcome)

    trt_summary <- df |>
      dplyr::group_by(.data$T) |>
      dplyr::summarise(n_studies = dplyr::n(),
                       n_patients = sum(.data$N),
                       min_outcome = signif(min(.data$Mean), 3),
                       average_outcome = signif(sum(.data$Mean * .data$N) / sum(.data$N), 3),
                       max_outcome = signif(max(.data$Mean), 3)
      )

    colnames(trt_summary) <- c("Treatment",
                               "Number of studies",
                               "Number of patients",
                               "Minimum outcome",
                               "Average outcome",
                               "Maximum outcome")

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
    colnames(pair_summary) <- c("Treatment comparison",
                                "Number of studies",
                                "Number of patients")
  }

  network_summary <- data.frame(descriptions, values)
  colnames(network_structure) <- c("Characteristic", "Value")

  return(list(network = network_summary,
              treatments = trt_summary,
              pairs = pair_summary))
}
