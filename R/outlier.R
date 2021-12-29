outlier_detect <- function(x, n) {
  output <-
    !dplyr::between(x,
                    median(x, na.rm = TRUE) - n*mad(x, na.rm = TRUE),
                    median(x, na.rm = TRUE) + n*mad(x, na.rm = TRUE)
    )
  as.numeric(output)
}

#' Identifies outliers using a robust z-score
#' @description This function takes a dataframe and one argument (i.e. the number of median absolute deviations outside which to assign a particular data point as an outlier).
#'
#' @param data the data frame for which outliers need to be found
#' @param n the number of median absolute deviations from the median outside which to assign a datum as an outlier (default = 3)
#'
#' @return the dataframe with outlier status (1, 0) for all the numerical variables
#' @export
#'
#' @examples
#'


outlier <- function(data, n = 3) {
  data %>%
    mutate(across(where(is.numeric),
                  outlier_detect,
                  n,
                  .names = "outlier_{.col}"))%>%
    dplyr::filter(if_any(contains("outlier"), ~ .x == 1))
}

