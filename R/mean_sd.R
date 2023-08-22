#' Formatted mean and standard deviation
#'
#' @param x A numeric vector.
#' @param digits,digits.mean,digits.sd Number of digits to print. `digits`
#' applies to both mean and sd, with the other options overriding if supplied.
#' @param NA.show Boolean. Whether to show count of missing values.
#' @param NA.name Name of row with count of missing values.
#' @param digits.NA Decimal digits for percentage of `NA`.
#'
#' @return If `NA.show` is `FALSE` then a length 1 character of the format
#' `"mean, (sd)"` with the mean and sd of `x`.
#'
#' If `NA.show` is `TRUE` then a data.table with two rows, the first row has `NA` in
#' the first column and the mean and sd in the second column. The second row
#' has `NA.name` in the first column and the count and pct of missing values in
#' the second.
#' @import data.table
#' @export
#'
#' @examples
#' mean_sd(c(rnorm(100), NA))
mean_sd <- function(
    x,
    digits = 1,
    digits.mean = NULL,
    digits.sd = NULL,
    NA.show = TRUE,
    NA.name = "Missing",
    digits.NA = 0
){
  res <- paste0(
    format_elementwise(base::mean(x, na.rm = TRUE), digits = ifelse(!is.null(digits.mean), digits.mean, digits)),
    " (",
    format_elementwise(stats::sd(x, na.rm = TRUE), digits = ifelse(!is.null(digits.sd), digits.sd, digits)),
    ")"
  )

  if(NA.show){
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
