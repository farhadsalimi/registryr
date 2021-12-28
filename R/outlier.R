#' Identifies outliers using a robust z-score
#' @description This function takes one argument, the number of median absolute deviations outside which to assign a particular data point as an outlier.
#'
#' @param x the numeric vector of interest
#' @param n the number of median absolute deviations from the median outside which to assign a datum as an outlier (default = 3)
#'
#' @return the outlier status (1, 0)
#' @export
#'
#' @examples
outlier <- function(x, n = 3) {
  output <-
    !dplyr::between(x,
                    median(x, na.rm = TRUE) - n*mad(x, na.rm = TRUE),
                    median(x, na.rm = TRUE) + n*mad(x, na.rm = TRUE)
                    )
  as.numeric(output)
}
