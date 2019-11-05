#' Make funnel plots
#'
#' Create a funnel plot from a dataframe containing at least two columns (i.e numerator and denominator)
#' It is currently working only with binary outcome, continuous outcomes will be supported in future
#'
#' @param data dataframe containing the numertaors and denominators
#' @param outcome string health outcome (e.g. "inpatient mortality")
#' @param num the name of the column in the dataframe which includes the numerator (e.g. numerator)
#' @param denom the name of the column in the dataframe which includes the denominator (e.g. denominator)
#' @param bm the benchamrk value
#' @param ci1 the alpha related to the first line of control limits (e.g. 0.95)
#' @param ci2 the alpha related to the second line of control limits (e.g 0.998)
#' @param flip_col reverse the colours of the circles
#'
#' @return ggplot
#' @export
#'
#' @examples
#'
fplot <- function(data, outcome, num, denom, bm, ci1 = 0.95, ci2 = 0.998, flip_col = FALSE) {

  funnel_input <-
    data %>%
    dplyr::mutate(rate = 100*{{ num }}/{{ denom }}) %>%
    dplyr::rename(n = {{ num }}, d = {{ denom }})

  funnel_data <- funnelR::fundata(
    input = funnel_input,
    benchmark = bm,
    alpha = ci1,
    alpha2 = ci2,
    step = 1
  )

  funnel_input <-
    funnel_input %>%
    dplyr::left_join(funnel_data) %>%
    dplyr::mutate(
      status = dplyr::if_else(
        rate < 100 * lo, "below",
        dplyr::if_else(rate > 100 * up, "above", "within"),
      ),
      status = factor(status) %>% forcats::fct_relevel("above", "within", "below")
    ) %>%
    tidytidbits::execute_if(flip_col == TRUE,
      mutate(status = forcats::fct_rev(status))
      )

  funnel_data <-
    funnel_data %>%
    tidyr::gather(key = "key", value = "percent", up:lo2) %>%
    dplyr::mutate(
      percent = 100 * percent,
      ci = dplyr::case_when(
        key == "up" | key == "lo" ~ paste0(100 * ci1, "% control limits"),
        key == "up2" | key == "lo2" ~ paste0(100 * ci2, "% control limits")
      )
    )

  ## visualise
  ggplot2::ggplot(data = funnel_input, ggplot2::aes(x = d, y = 100 * n / d)) +
    ggplot2::geom_hline(data = funnel_data, ggplot2::aes(yintercept = 100 * benchmark), colour = "red") +
    ggplot2::geom_line(
      data = funnel_data,
      ggplot2::aes(x = d, y = percent, group = key, linetype = ci)
    ) +
    ggplot2::geom_point(
      ggplot2::aes(fill = status),
      shape = 21,
      size = 2,
      colour = "black",
      show.legend = F
    ) +
    cowplot::theme_cowplot(font_family = "serif") +
    # geom_text(
    #   #data = funnel_input %>% dplyr::filter(status != "within"),
    #   data = funnel_input,
    #   aes(
    #     label = hospital_code_lumped,
    #     colour = status),
    #   size = 3,
    #   hjust = 0,
    #   nudge_x = 15,
    #   show.legend = FALSE
    #   ) +

    ggplot2::coord_cartesian(ylim = c(0, 100 * max(funnel_input$n / funnel_input$d))) +
    # scale_y_continuous(breaks = seq(0 , 100*max(funnel_input$n/funnel_input$d), round(100*max(funnel_input$n/funnel_input$d)/5))) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      x = "Number of surgeries",
      y = glue::glue("{stringr::str_to_sentence(outcome)} (%)"),
      subtitle = glue::glue("Postoperative {outcome} (mean {round(100*bm,1)} %)")
    ) +
    ggplot2::scale_fill_manual(values = c("#F8766D",  "#00BFC4", "#7CAE00")) +
    {NULL}

}
