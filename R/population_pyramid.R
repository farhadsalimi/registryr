#' Create a population pyramid plot
#' @description This function assumes three variables in the dataset: 1. sex (i.e. 'Male' vs 'Female)
#' 2. age_group 3. n (count)
#'
#' @param data dataframe which must include three variables (sex, age_group, and n)

#'
#' @return pyramid plot as a ggplot2 object
#' @export
#'
#' @examples
#'
population_pyramid <- function(data) {

  data <-
    data %>%
    dplyr::mutate(n = if_else(sex == "Female", -n, n))

  dummy_data <-
    tibble::tribble(~sex,  ~age_group,  ~n,
            "Female", data$age_group[1], -max(abs(data$n)),
            "Male", data$age_group[1], max(abs(data$n)))

  data %>%
    ggplot2::ggplot(aes(x = age_group, y = n, fill = sex)) +
    ggplot2::geom_col() +
    ggpol::facet_share(~sex,
                       dir = "h",
                       scales = "free",
                       reverse_num = TRUE
    ) +
    ggplot2::coord_flip() +
    ggplot2::geom_blank(data = dummy_data) +
    cowplot::theme_cowplot() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = NULL, y = NULL)
}
