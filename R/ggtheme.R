ggtheme <- function(){
  library(ggplot2)
  theme_set(theme_bw())
  theme_update(strip.background = element_blank())
  options(ggplot2.discrete.colour = function(...) scale_colour_brewer(..., palette = "Set2"))
  options(ggplot2.discrete.fill = function(...) scale_fill_brewer(..., palette = "Set2"))
}
