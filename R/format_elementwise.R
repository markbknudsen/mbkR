format_elementwise <- function(
  x,
  digits,
  scientific = FALSE,
  ...
){
  sapply(x, \(y) format(round(y, digits = digits), nsmall = digits, scientific = scientific, ...))
}
