#' Create an n*m grid of numbered squares on a plot.
#'
#' @param number_1 Number of squares per row
#' @param number_2 Number of squares per column
#'
#' @return ggplot2 plot object
#' @export
maths_plot_matrix <- function(number_1, number_2) {
  size <- 4
  gap <- 1
  period <- size + gap
  
  data <- data.frame(
    xmin = period * rep(0:(number_1 - 1), number_2),
    ymin = unlist(
      lapply(
        0:(number_2 - 1),
        function(n) {
          return(period * rep(n, number_1))
        }
      )
    ),
    number = 1:(number_1 * number_2)
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
      size = 50 / max(number_1, number_2)
    )
  
  return(plot)
}
