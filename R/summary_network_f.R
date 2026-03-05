#' Produce a plot of the network using `netmeta::netgraph()`
#'
#' @param style character. The plot to produce, either `netgraph` or `netplot`
#' @param label_size numeric. The size of labels in the plots. Default of `1`.
#' @param title character. Title of plot. Default of no title.
#' @inheritParams common_params
#' @inherit return-svg return
#' @examples
#' configured_data_path <- system.file("extdata", "configured_data.Rds", package = "metainsight")
#' configured_data <- readRDS(configured_data_path)
#'
#' summary_network(configured_data = configured_data,
#'                 style = "netgraph")
#'
#' @export

summary_network <- function(configured_data, style, label_size = 1, title = "", logger = NULL){

  check_param_classes(c("configured_data", "style", "label_size", "title"),
                      c("configured_data", "character", "numeric", "character"), logger)

  if (!(style %in% c("netgraph", "netplot"))){
    logger |> writeLog(type = "error", "style must be either netgraph or netplot")
    return()
  }

  n_trt <- configured_data$freq$ntx
  if (n_trt < 5){
    height_and_width <- 5
  } else {
    height_and_width  <- 5 + sqrt(n_trt-5)
  }

  svg <- svglite::xmlSVG({
    if (style == "netgraph"){
      netmeta::netgraph(configured_data$freq$net1, lwd = 2, number.of.studies = TRUE, plastic = FALSE, points = TRUE,
                               cex = label_size, cex.points = 2, col.points = 1, col = 8, pos.number.of.studies = 0.43,
                               col.number.of.studies = "forestgreen", col.multiarm = "white",
                               bg.number.of.studies = "white")
      title(title)

    } else if (style == "netplot"){
      netmeta::netgraph(configured_data$freq$net1,
                        adj = 0.5,
                        scale = 1,
                        cex = 1,
                        lwd.max = max(configured_data$freq$net1$k.trts) ,
                        cex.points = configured_data$freq$net1$k.trts,
                        number.of.studies = FALSE,
                        col = "grey",
                        points.max = 20,
                        col.points = "#f69c54",
                        thickness = "number.of.studies",
                        rescale.thickness = I,  # avoid sqrt rescaling
                        rescale.pointsize = I,
                        plastic = FALSE,
                        points = TRUE,
                        multiarm = FALSE,
                        start = "circle",
                        rotate = 90)
    }
  },
  width = height_and_width,
  height = height_and_width,
  web_fonts = list(
    arimo = "https://fonts.googleapis.com/css2?family=Arimo:wght@400;700&display=swap")
  ) |> crop_svg()

  return(svg)

}

#' Creates network connectivity info displayed under network plots
#'
#' @param freq List of NMA results created by freq_wrap().
#' @return Vector summarising network connectivity created by netmeta::netconnection().
#' @export
make_netconnect <- function(freq) {
  d1 <- freq$d1
  nc <- netmeta::netconnection(treat1 = d1$treat1, treat2 = d1$treat2, studLab = d1$studlab, data = NULL)
  # keep only the parts we want, match ensures the order
  summary <- nc[match(c("k", "m", "n", "d", "n.subnets"), names(nc))]
  return(unlist(summary))
}
