#' Produce deviance plots
#' @param model Bayesian model produced by bayes_model
#' @inheritParams common_params
#' @return list containing:
#'  \item{deviance_mtc}{results from `gemtc::mtc.deviance()` for model$mtcResults}
#'  \item{deviance_ume}{results from `gemtc::mtc.deviance()` for UME model}
#'  \item{scat_plot}{plotly object}
#'  \item{stem_plot}{plotly object}
#'  \item{lev_plot}{plotly object}
#'
#' @export
bayes_deviance <- function(model, async = FALSE){

  if (!inherits(model, "bayes_model")){
    return(async |> asyncLog(type = "error", "model must be an object created by bayes_model()"))
  }

  deviance <- gemtc::mtc.deviance(model$mtcResults)
  scat <- scat_plot(model, deviance, model$model_type, model$outcome_measure)

  list(
    deviance_mtc = deviance,
    deviance_ume = scat$y,
    scat_plot = scat$p,
    stem_plot = stem_plot(deviance),
    lev_plot = lev_plot(deviance))
}

#' UME scatter plot
#' @param model Bayesian model produced by bayes_model
#' @param deviance Output produced by `gemtc::mtc.deviance()`
#' @param model_type Model effects type. "random" or "fixed".
#' @param outcome_measure Outcome measure being analysed: one of "OR". "RR", "MD".
scat_plot <- function(model, deviance, model_type, outcome_measure) {
  if (outcome_measure == "MD") {
    like <- "normal"
    link <- "identity"
  } else if (outcome_measure == "OR" || outcome_measure == "RR") {
    like <- "binom"
    link <- ifelse (outcome_measure == "OR", "logit", "log")
  } else {
    stop("outcome_measure must be 'MD', 'OR', or 'RR'")
  }

  c <- data.frame(deviance$dev.ab)
  c$names <- rownames(c)

  ume <- gemtc::mtc.model(network = model$mtcNetwork,
                         type = "ume",
                         linearModel = model_type,
                         likelihood = like,
                         link = link,
                         dic = TRUE)
  ume_results <- gemtc::mtc.run(ume)
  y <- gemtc::mtc.deviance(ume_results)
  inc <- data.frame(y$dev.ab)
  inc$names <- rownames(inc)
  all <- merge(c, inc, by = "names")

  names(all)[names(all) == "X1.x"] <- "NMAmodel_arm1"
  names(all)[names(all) == "X1.y"] <- "UMEmodel_arm1"
  names(all)[names(all) == "X2.x"] <- "NMAmodel_arm2"
  names(all)[names(all) == "X2.y"] <- "UMEmodel_arm2"

  k <- all[, names(all) != "names"]  #### to define the maximum range of the equality line: find the maximum number of the dev data in the dataset.
  j <- max(k, na.rm = TRUE)
  m <- c(0, 1, j)
  n <- c(0, 1, j)

  dline <- data.frame(m, n)

  p = plotly::plot_ly() |>   # plot
    plotly::add_trace(data = dline, x = ~m, y = ~n, type = 'scatter', mode = 'lines',
              line = list(color = '#45171D'))
  p = p |>
    plotly::add_trace(data = all, x = ~NMAmodel_arm1, y = ~UMEmodel_arm1, type = 'scatter', mode = 'markers',
              marker = list(size = 4, color = '#CAEFD1',
                            line = list(color = 'rgb(0,128,0)',
                                        width = 2)),
              hoverinfo='text',
              text=~paste('</br> Author', all$names,
                          '</br> Arm 1',
                          '</br> Deviance from NMA model:',round(NMAmodel_arm1, digits = 2),
                          '</br> Deviance from UME model:',round(UMEmodel_arm1, digits = 2)
              ))
  p = p |>
    plotly::add_trace(
      x = ~NMAmodel_arm2, y = ~UMEmodel_arm2, type = 'scatter', mode = 'markers',
      marker = list(size = 4, color = '#CAEFD1',
                    line = list(color = 'rgb(0,128,0)',
                                width = 2)),
      hoverinfo = 'text',
      text = ~paste('</br> Author', all$names,
                    '</br> Arm 2',
                    '</br> Deviance from NMA model:', round(NMAmodel_arm2, digits = 2),
                    '</br> Deviance from UME model:', round(UMEmodel_arm2, digits = 2)

      ))|>
    plotly::layout(showlegend = FALSE, xaxis = list(title = "Deviance from NMA model"),
           yaxis = list(title = "Deviance from UME inconsistency model"))

  if (ncol(c) > 3) {
    p = p |>
      plotly::add_trace(data = all,
                x = ~X3.x, y = ~X3.y, type = 'scatter', mode = 'markers',
                marker = list(size = 4, color = '#CAEFD1',
                              line = list(color = 'rgb(0,128,0)',
                                          width = 2)),
                hoverinfo = 'text',
                text = ~paste('</br>', all$names,
                              '</br> Arm 3',
                              '</br> Deviance from NMA model:', round(X3.x, digits = 2),
                              '</br> Deviance from UME model:', round(X3.y, digits = 2)))}
  if (ncol(c) > 4) {
    p = p |>
      plotly::add_trace(data = all,
                x = ~X4.x, y = ~X4.y, type = 'scatter', mode = 'markers',
                marker = list(size = 4, color = '#CAEFD1',
                              line = list(color = 'rgb(0,128,0)',
                                          width = 2)),
                hoverinfo = 'text',
                text = ~paste('</br>', all$names,
                              '</br> Arm 4',
                              '</br> Deviance from NMA model:', round(X4.x, digits = 2),
                              '</br> Deviance from UME model:', round(X4.y, digits = 2)))}
  if (ncol(c) > 5) {
    p = p |>
      plotly::add_trace(data = all,
                x = ~X5.x, y = ~X5.y, type = 'scatter', mode = 'markers',
                marker = list(size = 4, color = '#CAEFD1',
                              line = list(color = 'rgb(0,128,0)',
                                          width = 2)),
                hoverinfo = 'text',
                text = ~paste('</br>', all$names,
                              '</br> Arm 5',
                              '</br> Deviance from NMA model:', round(X5.x, digits = 2),
                              '</br> Deviance from UME model:', round(X5.y, digits = 2)))}
  if (ncol(c) > 6) {
    p = p |>
      plotly::add_trace(data = all,
                x = ~X6.x, y = ~X6.y, type = 'scatter', mode = 'markers',
                marker = list(size = 4, color = '#CAEFD1',
                              line = list(color = 'rgb(0,128,0)',
                                          width = 2)),
                hoverinfo = 'text',
                text = ~paste('</br>', all$names,
                              '</br> Arm 6',
                              '</br> Deviance from NMA model:', round(X6.x, digits = 2),
                              '</br> Deviance from UME model:', round(X6.y, digits = 2)))}

  return(list(p = p, y = y))
}

#' Stem plot
#' @param deviance Output produced by `gemtc::mtc.deviance()`
stem_plot <- function(deviance) {
  c <- data.frame(deviance$dev.ab)
  c$names <- rownames(c)
  tpl <- deviance[['dev.ab']]
  study <- matrix(rep(1:nrow(tpl), times = ncol(tpl)), nrow = nrow(tpl), ncol = ncol(tpl))
  study <- t(study)[t(!is.na(tpl))]
  devbar <- t(deviance[['dev.ab']])[t(!is.na(tpl))]
  title <- "Per-arm residual deviance"
  xlab <- "Arm"
  k <- rowSums(!is.na(tpl))
  studynames <- rep(c$names, k)
  v <- 1:length(devbar)
  sep <- study%%2
  d <- data.frame(v, devbar, sep, study, studynames)
  xl <- list(
    title = xlab,
    range= c(0, length(devbar) + 5),
    tick0 = 0,
    dtick = 5,
    zeroline = TRUE,
    showline = TRUE
  )
  yl <- list(
    title = "Residual deviance",
    range = c(0, ceiling(devbar)),
    autorange = TRUE,
    tick0 = 0,
    dtick = 0.5,
    zeroline = TRUE,
    showline = TRUE
  )
  p <- plotly::plot_ly(data = d, x = ~v, y = ~devbar)
  for (i in 1:length(devbar)) {
    p = p |>
      plotly::add_segments(x = i,
                   xend = i,
                   y = 0,
                   yend = devbar[i],
                   marker = list(color = 'white',
                                 line = list(color = 'white')
                   ),
                   line = list(color = 'black',
                               width = 1)
      )
  }
  p = p|>
    plotly::add_trace(data = d, x = ~v, y = ~devbar, type = 'scatter', mode = 'markers',
              marker = list(size = 4,
                            color = '#CAEFD1',
                            line = list(color = 'rgb(0,128,0)',
                                        width = 2)
              ),
              symbol = ~sep, symbols = c('circle', 'o'),
              hoverinfo = 'text',
              text = ~paste('</br> Study', d$studynames,
                            '</br> Deviance from NMA model:', round(d$devbar, digits = 2)
              )) |>
    plotly::layout(
      xaxis = xl, yaxis = yl, showlegend = FALSE)
  return(p)
}



#' Creates a plot of leverage versus residual deviance, by study.
#'
#' @param deviance gemtc::mtc.deviance() object.
#' @return Leverage vs residual deviance plot.
lev_plot <- function(deviance) {
  fit.ab <- apply(deviance[['fit.ab']], 1, sum, na.rm = TRUE)
  dev.ab <- apply(deviance[['dev.ab']], 1, sum, na.rm = TRUE)
  lev.ab <- dev.ab - fit.ab
  fit.re <- deviance[['fit.re']]
  dev.re <- deviance[['dev.re']]
  lev.re <- dev.re - fit.re
  nd <- c(deviance[['nd.ab']], deviance[['nd.re']])
  w <- sqrt(c(dev.ab, dev.re) / nd)
  lev <- c(lev.ab, lev.re) / nd
  d <- data.frame(w, lev)
  d$names <- rownames(d)

  a <- seq(from = 0, to = 3, by = 0.05)
  b1 <- 1 - a^2
  b2 <- 2 - a^2
  b3 <- 3 - a^2
  b4 <- 4 - a^2
  parabola <- data.frame(a, b1, b2, b3, b4)

  xlab = "Square root of average residual deviance across the arms for each study"
  'sqrt(average(residual deviance for arm 1, residual deviance for arm 2...))'
  ylab = "Average leverage across the arms for each study"

  xl <- list(
    title = xlab,
    range = c(0, max(c(w, 2.5))),
    tick0 = 0,
    dtick = 0.5,
    zeroline = TRUE,
    showgrid = TRUE
  )
  yl <- list(
    title=ylab,
    range=c(0, max(c(lev, 4))),
    tick0 = 0,
    dtick = 1,
    zeroline = TRUE,
    showgrid = TRUE
  )

  p <- plotly::plot_ly(parabola, x = ~a) |>
    plotly::add_trace(y = b1, mode = 'lines', line = list(color = 'black', width = 1), hoverinfo = 'skip') |>
    plotly::add_trace(y = b2, mode = 'lines', line = list(color = 'black', width = 1), hoverinfo = 'skip') |>
    plotly::add_trace(y = b3, mode = 'lines', line = list(color = 'black', width = 1), hoverinfo = 'skip') |>
    plotly::add_trace(y = b4, mode = 'lines', line = list(color = 'black', width = 1), hoverinfo = 'skip') |>
    plotly::add_trace(data = d, x = ~w, y = ~lev,
              marker = list(size = 4,
                            color = '#CAEFD1',
                            line = list(color = 'rgb(0,128,0)',
                                        width = 2)
              ),
              hoverinfo = 'text',
              text = ~paste('</br> Study:', d$names,
                            '</br> Deviance',round(d$w, digits = 2),
                            '</br> Leverage',round(d$lev, digits = 2)
              )) |>
    plotly::layout(
      xaxis = xl, yaxis = yl, showlegend = FALSE, title = "Leverage versus residual deviance")
  return(p)
}
