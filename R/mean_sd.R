mean_sd <- function(
    x,
    digits = 1,
    digits.mean = NULL,
    digits.sd = NULL
    # todo
    # NA.show = TRUE,
    # NA.name = "Missing"
){
  paste0(
    format_elementwise(base::mean(x, na.rm = TRUE), digits = ifelse(!is.null(digits.mean), digits.mean, digits)),
    " (",
    format_elementwise(stats::sd(x, na.rm = TRUE), digits = ifelse(!is.null(digits.sd), digits.sd, digits)),
    ")"
  )
}
