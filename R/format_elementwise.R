#' Apply `format` elementwise
#'
#' @param x A vector.
#' @param digits Number of digits after the decimal place.
#' @param scientific Boolean. as in [format()].
#' @param ... Arguments passed to [format()].
#'
#' @return `x` after applying format elementwise.
#' @export
#'
#' @examples
#' x <- c(11, 1, 1.1, 1.11, 1.111111111)
#' format_elementwise(x, 1)
format_elementwise <- function(
  x,
  digits,
  scientific = FALSE,
  ...
){
  sapply(x, \(y) format(round(y, digits = digits), nsmall = digits, scientific = scientific, ...))
}
