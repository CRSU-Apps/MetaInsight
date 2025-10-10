### Function taken and adapted from BUGSnet GitHub ###
network.structure <- function(data.nma, my_order = NA) {     # data.nma is a BUGSnetData item, created using data.prep(arm.data=longdata, varname.t = "T", varname.s="Study")

  # not sure why, but can't replace with native pipes
  library(magrittr)

  # Bind Variables to function
  from <- NULL
  to <- NULL
  trt <- NULL
  flag <- NULL
  mtchvar <- NULL


  trial <- rlang::quo(!!as.name(data.nma$varname.s))
  varname.t.quo <- rlang::quo(!!as.name(data.nma$varname.t))

  # Change underscores if present (as when my_order is based on rank results, it'll already have underscores removed)
  data.nma$arm.data$T <- data.nma$arm.data$T
  data.nma$treatments$T <- data.nma$treatments$T

  studytrt <- data.nma$arm.data %>%
    dplyr::select(data.nma$varname.s, data.nma$varname.t) %>%
    tidyr::nest(data = c(data.nma$varname.t))      # nest treatments within each study

  cnt <- data.nma$arm.data %>%
    dplyr::select(data.nma$varname.s, data.nma$varname.t) %>%
    dplyr::count(data.nma$varname.s)     # number of treatments within each study

  tmp1 <- dplyr::bind_cols(studytrt, cnt) %>%
    dplyr::filter(n > 1)   # removes single arm studies

  if (rlang::is_empty(my_order)) {
    pairs <- tmp1[1, "data"] %>%
      unlist %>%
      sort %>%
      combn(2)    # each set of treatment pairs is put in alphabetical order
  } else {
    pairs <- unlist(tmp1[1, "data"])[order(match(unlist(tmp1[1, "data"]), my_order))] %>%
      combn(2) # orders according to 'my_order'
  }

  for(i in 2:nrow(tmp1)){
    if (rlang::is_empty(my_order)) {
      pairs <- tmp1[i, "data"] %>%
        unlist %>%
        sort %>%
        combn(2) %>%
        cbind(pairs)
    } else {
      pairs <- unlist(tmp1[i, "data"])[order(match(unlist(tmp1[i, "data"]), my_order))] %>%
        combn(2) %>%
        cbind(pairs)
    }
  }

  # data of each pairwise comparison and number of trials
  pairs2 <- data.frame(
    from = pairs[1, ],
    to = pairs[2, ]) %>%
      dplyr::group_by(from, to) %>%
      dplyr::mutate(edge.weight = max(1:dplyr::n())) %>%   # counts number of pairwise comparisons
      dplyr::arrange(from, to) %>%
      dplyr::distinct() %>%
      dplyr::mutate(mtchvar = 1)

  studylabs <- studytrt %>%
    dplyr::group_by(!! trial) %>%
    dplyr::mutate(trt = paste(unlist(data), collapse = ';')) %>%
    dplyr::select(!!trial, trt) %>%
    dplyr::mutate(mtchvar = 1)

  edges <- dplyr::left_join(pairs2, studylabs, by = "mtchvar") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      flag = ifelse(
        stringr::str_detect(trt, stringr::coll(from)) & stringr::str_detect(trt, stringr::coll(to)),
        1,
        0
      )
    ) %>%
    dplyr::filter(flag == 1) %>%
    dplyr::select(-c(mtchvar, flag, trt)) %>%
    tidyr::nest(data = c(!!trial)) %>%          # nests the studies for which belonged to each treatment comparison
    dplyr::group_by(from, to) %>%
    dplyr::mutate(study = paste(unlist(data), collapse = ', \n')) %>%
    dplyr::select(-data)

  return(edges)
}
