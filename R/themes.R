#' Change the ggplot2 theme to the ADNET theme
#'
#' @keywords internal
#'
#' @export
#'
theme_define <- function(registry) {
  if(!(registry %in% c("adnet"))) {
    stop(paste(registry, "is not a known registry! Please contact Farhad if you need to include a new theme"))
  }

  if(registry == "adnet") {
    thematic::thematic_rmd(
      bg = "white",
      fg = "black",
      accent = "#99C455",
      qualitative = c("#99C455", "#00A480", "#152144", "#D6773A", "#99C455", "#0066AD", "#70AD47"),
      font = thematic::font_spec(families = "Open Sans", install = TRUE))

    ggplot2::theme_set(cowplot::theme_cowplot())

    plot.new() # this is to avoid an error, should be fixed in the later versions of thematic

  }

}




