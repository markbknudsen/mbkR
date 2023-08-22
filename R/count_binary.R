#' Formatted count and percentage of 1's
#'
#' @param x A binary/boolean vector.
#' @param digits Number of digits used when formatting percentage.
#'
#' @return Length 1 character of the form `"n (pct%)"` with `n` and `pct`
#' replaced with number of occurences and percentage of 1's in `x`.
#' @export
#'
#' @examples
#' x <- rnorm(100)
#' count_binary(x > 1)
count_binary <- function(
  x,
  digits = 0
){
  if(any(!(x %in% c(0, 1)))) stop("x must be a binary or boolean vector.")
  s <- sum(x)
  paste0(
    s,
    if(s == 0) NULL
    else{
      paste0(
        " (",
        format_elementwise(s / length(x) * 100, digits = digits),
        "%)"
      )
    }
  )
}
