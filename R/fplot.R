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
#' @param cols the colours used for the points above, within and below the control first control limits
#' @param site_name the sites to be highlighted
#'
#' @return ggplot
#' @export
#'
#' @examples
#'
fplot <- function(data, outcome, num, denom, bm, ci1 = 0.95, ci2 = 0.998,
                  cols = c("#F8766D",  "#00BFC4", "#7CAE00"),
                  site_name) {

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
      status = factor(status) %>%
        forcats::fct_expand("above", "within", "below") %>%
        forcats::fct_relevel("above", "within", "below")
    ) %>%
    # tidytidbits::execute_if(flip_col == TRUE,
    #   mutate(status = forcats::fct_rev(status))
    #   )
    {.}

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
    ggplot2::geom_hline(data = funnel_data, ggplot2::aes(yintercept = 100 * benchmark),
                        col = "dark blue") +
    ggplot2::geom_line(
      data = funnel_data,
      ggplot2::aes(x = d, y = percent, group = key, linetype = ci),
      col = "black"
    ) +
    ggplot2::geom_point(
      ggplot2::aes_(col = ~status,
                   size = as.name(site_name),
                   shape = as.name(site_name))
      # show.legend = F
    ) +
    # ggplot2::geom_point(
    #   ggplot2::aes_(
    #     # col = as.name(site_name),
    #     size = as.name(site_name)),
    #   show.legend = T
    # ) +
    cowplot::theme_cowplot(
      # font_family = "serif"
      ) +
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
      x = "Count",
      y = "Outcome (%)"
      # subtitle = glue::glue("Postoperative {stringr::str_to_lower(outcome)} (mean {round(100*bm,1)} %)")
    ) +
    ggplot2::scale_colour_manual(
      values = cols) +
    ggplot2::scale_size_manual(values = c(2, 3)) +
    ggplot2::guides(colour = FALSE,
                    size = "legend",
                    shape = "legend") +
    {NULL}

}
