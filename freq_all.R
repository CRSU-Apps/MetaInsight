freq_all <- function(data, metaoutcome, treatment_list, outcome_measure, model, excluded){
  data_wide <- entry.df(data,metaoutcome)    #transform data to wide form
  treat_list <- treatment_label(treatment_list)
  freq_wrap(data_wide, treat_list, model, outcome_measure, metaoutcome,
            ref_alter(data, metaoutcome, excluded, treatment_list)$ref_all)  # use the selfdefined function, freq_wrap
}