#' Creates a summary forest plot of all the studies.
#'
#' @param freq list. NMA results created by freq_wrap().
#' @param outcome_measure character. "MD", "SMD", "OR", "RR", or "RD".
#' @param header numeric. Multiplier for size of text in treatment contrast headers.
#' @param title numeric. Multiplier for size of title.
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default `NULL`
#'
#' @return Plot created by groupforest.df().
#' @export
summary_study <- function(freq, outcome_measure, header, title, logger = NULL) {

  check_param_classes(c("freq", "outcome_measure", "header", "title"),
                      c("list", "character", "numeric", "numeric"), logger)

  if (!outcome_measure %in% c("MD", "SMD", "OR", "RR", "RD")){
    logger %>% writeLog(type = "error", "outcome_measure must be either MD, SMD, OR, RR or RD")
    return()
  }

  d1 = freq$d0
  ntx = freq$ntx
  lstx = freq$lstx
  outcome = outcome_measure
  HeaderSize = header
  TitleSize = title

  text_label <- character()
  n_stud <- integer()
  for (i in 1:ntx) {
    for (j in 1:ntx) {
      if (nrow(d1[(d1$treat1 == i & d1$treat2 == j),]) > 0) {
        text_label <- c(paste(lstx[i], "vs", lstx[j]), text_label)
        n_stud <- c(n_stud, nrow(d1[(d1$treat1 == i & d1$treat2 == j),]))
      }
    }
  }
  gaps <- integer(length(n_stud))
  n_stud <- rev(n_stud)
  for (k in 1:length(n_stud)) {
    if (k == 1) {
      gaps[k] <- n_stud[k] + 1
    }
    else {
      gaps[k] <- gaps[k-1] + n_stud[k] + 2
    }
  }
  lines <- rev(c(1:(nrow(d1) + 2 * length(text_label) - 1)))
  lines <- lines[!lines %in% gaps]
  lines <- lines[!lines %in% (gaps + 1)]

  d1 <- d1[order(d1$treat1, d1$treat2, d1$StudyID), ] #ensuring the ordering is correct

  if (outcome == "OR" | outcome =="RR" ){
    fplot <- metafor::forest(d1$TE, sei = d1$seTE, slab = paste(d1$Study), subset = order(d1$treat1, d1$treat2),
                             ylim = c(1, nrow(d1) + 2 * length(text_label) + 2), rows = lines, atransf = exp,
                             at = log(c(0.01, 1, 10, 100)), xlab = paste("Observed ", outcome), efac = 0.5
    )
  } else {
    fplot <- metafor::forest(d1$TE, sei = d1$seTE, slab = paste(d1$Study), subset = order(d1$treat1, d1$treat2),
                             ylim = c(1, nrow(d1) + 2 * length(text_label) + 2), rows = lines,
                             xlab = paste("Observed ",outcome), efac = 0.5)
  }
  graphics::text(fplot$xlim[1], gaps, pos = 4, font = 4, text_label, cex = HeaderSize)
  graphics::title("Individual study results (with selected studies excluded) grouped by treatment comparison", cex.main = TitleSize)

  return(invisible(fplot))

}
