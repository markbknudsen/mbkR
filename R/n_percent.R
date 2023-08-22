#' Formatted count and percent of levels
#'
#' @param x A vector, usually factor or character.
#' @param digits Number of digits to use when formatting percentages.
#' @param NA.show Boolean. Whether to show count of missing values.
#' @param NA.name Name of row with count of missing values.
#'
#' @return A data.table with a column containing names of levels
#' and a column containing characters of the format `"n (pct%)"` where
#' `n` is the count of the level and `pct` is the percentage.
#' @import data.table
#' @export
#'
#' @examples
#' x <- c("A", "A", "B", "C")
#' n_percent(x)
n_percent <- function(
    x,
    digits = 0,
    NA.show = TRUE,
    NA.name = "Missing"
){
  if(is.factor(x)) levs <- levels(x)
  else levs <- unique(x)

  res <- lapply(
    levs,
    function(level){
      data.table(
        .level = level,
        count_binary(ifelse(is.na(x), 0, x == level))
      )
    }
  ) |> rbindlist()

  if(NA.show & any(is.na(x))){
    res <- rbind(
      res,
      data.table(
        .level = NA.name,
        count_binary(is.na(x), digits = digits)
      )
    )
    levs <- c(levs, NA.name)
  }
  res[, .level := factor(.level, levels = levs)]
}
