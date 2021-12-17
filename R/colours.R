# Code adapted from https://github.com/rfortherestofus/omni/blob/master/R/colors.R

corrp_palettes <- list(

  `rsr` = c("#3137f5", "#3f93f2", "#5ef2f2", "#51f5a0", "#51663d", "#adac5c", "#cf9136"),

  `bcca` = c("#3137f5", "#3f93f2", "#5ef2f2", "#51f5a0", "#51663d", "#adac5c", "#cf9136"),

  `excel` = c("#3137f5", "#3f93f2", "#5ef2f2", "#51f5a0", "#51663d", "#adac5c", "#cf9136"),

  `acfdr` = c("#3137f5", "#3f93f2", "#5ef2f2", "#51f5a0", "#51663d", "#adac5c", "#cf9136")
)

usethis::use_data(corrp_palettes,
                  overwrite = TRUE)


#' Return function to interpolate a CORRP colour palette
#'
#' @param palette Character name of the registry (e.g. bcca, excel, acfdr, etc)
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#' @keywords internal
#'
corrp_pal <- function(palette = "Main", reverse = FALSE, ...) {
  pal <- corrp_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}


#' Discrete color scale based on CORRP colors
#'
#' @param palette Character name of palette in corrp_palettes (e.g. "bcca", "excel", "acfdr", etc)
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale()
#'
#' @export
#' @example
#' iris %>%
#' group_by(Species) %>%
#' summarise(sepal_length_mean = mean(Sepal.Length)) %>%
#' ggplot(aes(x = Species, y = sepal_length_mean, fill = Species)) +
#' geom_bar(stat = "identity") +
#' coord_flip() +
#' scale_fill_corrp_discrete(palette = 'bcca')
#'
scale_color_corrp_discrete <- function(palette , reverse = FALSE, ...) {

  pal <- corrp_pal(palette = palette, reverse = reverse)

  ggplot2::discrete_scale("colour", paste0("corrp_", palette), palette = pal, ...)

}



#' Continuous color scale based on corrp colors
#'
#' @param palette Character name of palette in corrp_palettes ("Main" or "Blues")
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to scale_color_gradientn()
#'
scale_color_corrp_continuous <- function(palette, reverse = FALSE, ...) {

  pal <- corrp_pal(palette = palette, reverse = reverse)

  ggplot2::scale_color_gradientn(colors = pal(256), ...)

}


#' Discrete fill scale based on corrp colors
#'
#' @param palette Character name of palette in corrp_palettes ("Main" or "Blues")
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale()
#' @export
#'
scale_fill_corrp_discrete <- function(palette, reverse = FALSE, ...) {

  pal <- corrp_pal(palette = palette, reverse = reverse)

  ggplot2::discrete_scale("fill", paste0("corrp_", palette), palette = pal, ...)

}

#' Continuous fill scale based on corrp colors
#'
#' @param palette Character name of palette in corrp_palettes ("Main" or "Blues")
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to scale_color_gradientn()
#' @export
scale_fill_corrp_continuous <- function(palette, reverse = FALSE, ...) {

  pal <- corrp_pal(palette = palette, reverse = reverse)

  ggplot2::scale_fill_gradientn(colors = pal(256), ...)

}
