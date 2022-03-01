#' Convert an arsenal::tableby object to a kable
#'
#' Create a funnel plot from a dataframe containing at least two columns (i.e numerator and denominator)
#' It is currently working only with binary outcome, continuous outcomes will be supported in future
#'
#' @param tableby tableby object
#' @param label_translation A named list (or vector) where the name is the label in the output to be replaced in the pretty rendering by the character string value for the named element of the list, e.g., list(age = "Age(Years)", meansd = "Mean(SD)")
#' @param caption Title/caption for the table, defaulting to NULL (no title).
#'@param ref_label The table reference label
#' @return a kable object
#' @export
#'
#' @examples
#'
tableby_to_kable <- function(tableby, label_translation, caption, ref_label) {
  table <-
    tableby %>%
    summary(labelTranslations = label_translation,
            text = TRUE) %>%
    as.data.frame()

  na_rows <-
    table %>%
    janitor::clean_names() %>%
    tibble::rownames_to_column() %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), dplyr::na_if, "")) %>%
    tidyr::drop_na() %>%
    dplyr::pull(rowname)

  bolds <- table %>%
    janitor::clean_names() %>%
    tibble::rownames_to_column() %>%
    dplyr::select(rowname) %>%
    dplyr::mutate(bold = rowname %nin% na_rows) %>%
    dplyr::pull(bold)

  table %>%
    kableExtra::kbl(booktabs = TRUE,
        linesep = "",
        caption = caption,
        label = ref_label) %>%
    kableExtra::column_spec(1, bold = bolds)
}
