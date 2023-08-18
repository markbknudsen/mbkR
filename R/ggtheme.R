#' Change active `ggplot2` theme and default color palettes
#'
#' @return `NULL`
#' @export
#'
#' @examples
#' ggtheme()
ggtheme <- function(){
  library(ggplot2)
  ggplot2::theme_set(theme_bw())
  ggplot2::theme_update(strip.background = element_blank())
  options(ggplot2.discrete.colour = function(...) ggplot2::scale_colour_brewer(..., palette = "Set2"))
  options(ggplot2.discrete.fill = function(...) ggplot2::scale_fill_brewer(..., palette = "Set2"))
}
