#' ScatterPlay
#'
#' Manually scatter data points to see how correlation and regression are affected
#'
#' @export
#' @examples
#' scatter_play()
scatter_play <- function() {
  # create a blank plot (use and then remove a fake point)
  points <- data.frame(x=c(-99), y=c(-99))
  plot(points, xlim=c(-5,50), ylim=c(-5,50))
  points <- data.frame(x=c(), y=c())

  cat("Click on the plot to add points; [Esc] to quit")
  repeat {
    location <- locator(1)

    if (is.null(location)) break
    points <- rbind(points, location)
    plot(points, xlim=c(-5,50), ylim=c(-5,50),
         pch = 16, col=rgb(0.5, 0.5, 0.5, 0.5))

    if (nrow(points) < 2) next
    model <- lm(y ~ x, points)
    abline(model, col=rgb(0.0, 0.0, 0.5, 0.5), lwd=2)
    text(0, 50, paste("r: ", round(cor(points)[2], 2)), adj=0)
    text(0, 45, paste("y = ", round(model$coefficients[2], 2), " + ", round(model$coefficients[1], 2)), adj=0)
  }
}
