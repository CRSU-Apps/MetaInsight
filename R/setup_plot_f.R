#' Create an n*n grid of numbered squares on a plot.
#'
#' @param number Number of squares per side
#'
#' @return ggplot2 plot object
#' @export
setup_plot <- function(number) {
  size <- 4
  gap <- 1
  period <- size + gap
  
  data <- data.frame(
    xmin = period * rep(0:(number - 1), number),
    ymin = unlist(
      lapply(
        0:(number - 1),
        function(n) {
          return(period * rep(n, number))
        }
      )
    ),
    number = 1:(number * number)
  )
  
  plot <- ggplot(data) +
    theme_void() +
    theme(
      legend.position = "none"
    ) +
    coord_fixed() +
    geom_rect(
      aes(
        xmin = xmin,
        ymin = ymin,
        xmax = xmin + size,
        ymax = ymin + size
      )
    ) +
    geom_text(
      aes(
        label = number,
        x = xmin + size / 2,
        y = ymin + size / 2,
        vjust = "center",
        hjust = "center",
        color = "red"
      ),
      size = 50 / number
    )
  
  return(plot)
}
