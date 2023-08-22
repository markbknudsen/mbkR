#' Formatted median and quantiles
#'
#' @param x A numeric vector.
#' @param q A length 2 numeric vector of probabilities for the quantiles.
#' @param q.sep A length 1 character to separate the quantiles.
#' @param digits,digits.median,digits.quantiles Number of digits to print. `digits`
#' applies to both median and quantiles, with the other options overriding if supplied.
#' @param NA.show Boolean. Whether to show count of missing values.
#' @param NA.name Name of row with count of missing values.
#' @param digits.NA Decimal digits for percentage of `NA`.
#'
#' @return If `NA.show` is `FALSE` then a length 1 character of the format
#' `"median, (q[1]q.sepq[2])"` with the median and given quantiles of `x`.
#'
#' If `NA.show` is `TRUE` then a data.table with two rows, the first row has `NA` in
#' the first column and the median and quantiles in the second column. The second row
#' has `NA.name` in the first column and the count and pct of missing values in
#' the second.
#' @import data.table
#' @export
#'
#' @examples
#' median_quantiles(c(rnorm(100), NA))
median_quantiles <- function(
    x,
    q = c(0.25, 0.75),
    q.sep = ", ",
    digits = 1,
    digits.median = NULL,
    digits.quantiles = NULL,
    NA.show = TRUE,
    NA.name = "Missing",
    digits.NA = 0
){
  if(!(length(q) == 2 & is.numeric(q))) stop("q must be numeric probability of length 2")
  else if(min(q) < 0 | max(q) > 1) stop("Elements of q must be probabilities, i.e. in [0, 1]")

  res <- paste0(
    format_elementwise(stats::median(x, na.rm = TRUE), digits = ifelse(!is.null(digits.median), digits.median, digits)),
    " (",
    format_elementwise(stats::quantile(x, probs = q[1], na.rm = TRUE), digits = ifelse(!is.null(digits.quantiles), digits.quantiles, digits)),
    q.sep,
    format_elementwise(stats::quantile(x, probs = q[2], na.rm = TRUE), digits = ifelse(!is.null(digits.quantiles), digits.quantiles, digits)),
    ")"
  )

  if(NA.show & any(is.na(x))){
    res <- rbind(
      data.table(res)[, .level := NA],
      data.table(
        .level = NA.name,
        res = count_binary(is.na(x), digits = digits.NA)
      ),
      fill = TRUE
    )
    setcolorder(res, c(".level", "res"))
  }
  return(res)
}
