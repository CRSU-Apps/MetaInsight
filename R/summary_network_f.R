#' Produce either a netgraph or netplot
#'
#' @param freq list. Created by `frequentist()`
#' @param bugsnet dataframe. Created by `bugsnetdata()`
#' @param style character. The plot to produce, either `netgraph` or `netplot`
#' @param label_size numeric. The size of labels in the plots. Default of 1.
#' @param title character. Title of plot. Default of no title.
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default `NULL`
#' @return List containing:
#'  \item{svg}{character. SVG code to produce the plot}
#'  \item{height}{numeric. Plot height in pixels}
#'  \item{width}{numeric. Plot width in pixels}
#'
#' @export

summary_network <- function(freq, bugsnet, style, label_size = 1, title = "", logger = NULL){

  check_param_classes(c("freq", "bugsnet", "style", "label_size", "title"),
                      c("list", "data.frame", "character", "numeric", "character"), logger)

  if (!(style %in% c("netgraph", "netplot"))){
    logger |> writeLog(type = "error", "style must be either netgraph or netplot")
    return()
  }

  height <- 5
  width <- 5


  svg <- svglite::xmlSVG({
    if (style == "netgraph"){
      netmeta::netgraph(freq$net1, lwd = 2, number.of.studies = TRUE, plastic = FALSE, points = TRUE,
                               cex = label_size, cex.points = 2, col.points = 1, col = 8, pos.number.of.studies = 0.43,
                               col.number.of.studies = "forestgreen", col.multiarm = "white",
                               bg.number.of.studies = "white")
      title(title)

    } else if (style == "netplot"){
      # I have removed an order = NULL parameter here (SS)
      data.rh <- BUGSnet::data.prep(arm.data = bugsnet, varname.t = "T", varname.s = "Study")
      BUGSnet::net.plot(data.rh, node.scale = 3, edge.scale = 1.5, node.lab.cex = label_size,
                               layout.params = NULL)
      title(title)
    }
  },
  width = width,
  height = height,
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
