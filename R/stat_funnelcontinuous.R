#' Add funnel plot boundaries.
#'
#' @param step Step to use in drawing the boundary
#' (default = \code{0.5}).
#' @param ci_limits The limit to be used in computing the boundary
#' (default = \code{0.95}).
#'
#' @return ggplot stat
#' @export
#'
#' @examples

stat_funnelcontinuous <- function(mapping = NULL, data = NULL, geom = "ribbon", group = ci_cat,
                             position = "identity", na.rm = FALSE, show.legend = NA,
                             inherit.aes = TRUE, ci_limits = 0.95, step = 0.5, ...) {
  ggplot2::layer(
    stat = StatFunnelContinuous, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ci_limits = ci_limits, step = step, ...)
  )
}

StatFunnelContinuous <- ggplot2::ggproto("StatFunnelContinuous", ggplot2::Stat,
                               required_aes = c("x", "y", "sd"),

                               compute_group = function(data, scales, ci_limits, step) {

                                 n <- max(data$x)

                                 sd_wm <-
                                   data %>%
                                   dplyr::summarise(sd_wm = weighted.mean(sd, x)) %>%
                                   dplyr::pull(sd_wm)

                                 theta  <- weighted.mean(data$y, data$x)
                                 n <- max(data$x)

                                 step <- step
                                 p  <- (1 - ci_limits) / 2

                                 upper  <- theta + qnorm(1-p) * sd_wm / sqrt(seq(1,n, step))
                                 lower  <- theta + qnorm(p) * sd_wm / sqrt(seq(1,n, step))

                                 data.frame (
                                   x = seq(1, n, step),
                                   ymin = upper,
                                   ymax = lower)

                               }
)
