#' Make funnel plots
#'
#' Create a funnel plot from a dataframe containing at least two columns (i.e numerator and denominator)
#' It is currently working only with binary outcome, continuous outcomes will be supported in future
#'
#' @param data dataframe containing the numertaors and denominators
#' @param outcome string health outcome (e.g. "inpatient mortality")
#' @param outcome_type type of the outcome variables (i.e. "binary" or "continuous")
#' @param site_id string health service (e.g. "site_code")
#' @param adj_vars vector with the adjusting variables (e.g. c("sex", "age"))
#'
#' @return the risk adjusted dataset
#' @export
#'
#' @examples
#'
risk_adjust <- function(data, site_id, outcome, outcome_type, adj_vars) {
  formula <- glue::glue("{outcome} ~ {glue::glue_collapse(adj_vars, ' + ')}")
  outcome <- rlang::sym(outcome)
  site_id <- rlang::sym(site_id)

  if(outcome_type == "binary") {
    model <- glm(formula, family = "binomial", data = data)

    data <-
      data %>%
      modelr::add_predictions(model, type = "response")

    bm <-
      data %>%
      dplyr::summarise(rate = sum(!!outcome) / n()) %>%
      dplyr::pull(rate)

    data_grouped <-
      data %>%
      dplyr::group_by(!!site_id) %>%
      dplyr::summarise(
        sum_pred = sum(pred),
        n_outcome = sum(!!outcome),
        n_cases = n(),
        n_outcome_adjusted = ((n_outcome / sum_pred) * bm) * n_cases
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        n_outcome_adjusted = if_else(n_outcome_adjusted > n_cases,
                                      as.numeric(n_cases),
                                      round(n_outcome_adjusted)
        )
      )

    data_grouped <- data_grouped %>% dplyr::select(site_id, n_outcome, n_outcome_adjusted)
    return(data_grouped)
  }

  if(outcome_type == "continuous") {
    model <- glm(formula, family = "gaussian", data = data)

    data %>%
      modelr::add_predictions(model) %>%
      modelr::add_residuals(model) %>%
      dplyr::select(site_id, outcome, pred, resid) %>%
      mutate(mean_outcome_overall := mean(!!outcome)) %>%
      group_by(!!site_id, mean_outcome_overall) %>%
      summarise(
        n_cases = n(),
        mean_outcome := mean(!!outcome),
        mean_outcome_pred = mean(pred),
        sd_outcome = sd(resid)
      ) %>%
      ungroup() %>%
      mutate(
        mean_outcome_adj = (mean_outcome/mean_outcome_pred)*mean_outcome_overall
      ) %>%
      select(site_id, mean_outcome, mean_outcome_adj, sd_outcome)
  }
}
