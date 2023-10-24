#' Round numbers to a certain number of significant decimals.
#'
#' @param x A vector.
#' @param digits Number of significant digits after the decimal place.
#' @param scientific Boolean. as in [format()].
#' @param ... Arguments passed to [format()].
#'
#' @return `x` as a character rounded to `digits` significant decimal places.
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
  sapply(
    x,
    \(y) format(
      ifelse(abs(y) >= 1, round(y, digits = digits), signif(y, digits = digits)),
      nsmall = ifelse(abs(y) >= 1 | y == 0, digits, digits + ceiling(-log10(abs(y))) - 1),
      scientific = scientific,
      ...
    )
  )
}

