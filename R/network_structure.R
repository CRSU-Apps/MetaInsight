### Function taken and adapted from BUGSnet GitHub ###
network.structure <- function(data.nma, my_order=NA) {     # data.nma is a BUGSnetData item, created using data.prep(arm.data=longdata, varname.t = "T", varname.s="Study")
  
  # Bind Variables to function
  from <- NULL
  to <- NULL
  trt <- NULL
  flag <- NULL
  mtchvar <- NULL
  
  
  trial <- rlang::quo(!! as.name(data.nma$varname.s))
  varname.t.quo <- rlang::quo(!! as.name(data.nma$varname.t))
  
  # Change underscores if present (as when my_order is based on rank results, it'll already have underscores removed)
  data.nma[["arm.data"]][["T"]] <- str_wrap(gsub("_", " ", data.nma[["arm.data"]][["T"]]), width=10)
  data.nma[["treatments"]][["T"]] <- str_wrap(gsub("_", " ", data.nma[["treatments"]][["T"]]), width=10)
  
  studytrt <- data.nma$arm.data %>%
    select(data.nma$varname.s, data.nma$varname.t) %>%
    tidyr::nest(data=c(data.nma$varname.t))      # nest treatments within each study
  
  cnt <- data.nma$arm.data %>%
    select(data.nma$varname.s, data.nma$varname.t) %>%
    dplyr::count(data.nma$varname.s)     # number of treatments within each study
  
  tmp1 <- bind_cols(studytrt, cnt) %>%
    filter(n>1)   # removes single arm studies
  
  if (is.empty(my_order)) {
    pairs <- tmp1[1,"data"] %>% unlist %>% sort %>% combn(2)    # each set of treatment pairs is put in alphabetical order
  } else {
    pairs <- unlist(tmp1[1,"data"])[order(match(unlist(tmp1[1,"data"]),my_order))] %>% combn(2) # orders according to 'my_order'
  }
    
  for(i in 2:nrow(tmp1)){
    if (is.empty(my_order)) {
      pairs <- tmp1[i,"data"] %>% unlist %>% sort %>% combn(2) %>% cbind(pairs)
    } else {
      pairs <- unlist(tmp1[i,"data"])[order(match(unlist(tmp1[i,"data"]),my_order))] %>% combn(2) %>% cbind(pairs)
    }
  }
  
  # data of each pairwise comparison and number of trials
  pairs2 <- data.frame(from = pairs[1,],
                       to = pairs[2,]) %>%
    group_by(from, to) %>%
    plotly::mutate(edge.weight = max(1:n())) %>%   # counts number of pairwise comparisons
    arrange(from, to) %>%
    distinct() %>%
    plotly::mutate(mtchvar = 1)
  
  studylabs <- studytrt %>%
    group_by(!! trial) %>%
    plotly::mutate(trt = paste( unlist(data), collapse=';')) %>%
    select(!! trial, trt) %>%
    plotly::mutate(mtchvar = 1)
  
  edges <- left_join(pairs2, studylabs, by="mtchvar") %>%
    ungroup() %>%
    plotly::mutate(flag = ifelse(stringr::str_detect(trt, stringr::coll(from)) &
                           stringr::str_detect(trt, stringr::coll(to)), 1, 0)) %>%
    filter(flag == 1) %>%
    select(-c(mtchvar, flag, trt)) %>%
    nest(data=c(!! trial)) %>%          # nests the studies for which belonged to each treatment comparison
    group_by(from, to) %>%
    plotly::mutate(study = paste(unlist(data), collapse=', \n')) %>%
    select(-data)
  
  #return(list("edges"=edges, "nodes"=nodes))
  return(edges)
}