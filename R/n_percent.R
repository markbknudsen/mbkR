#' Formatted count and percent of levels
#'
#' @param x A vector, usually factor or character.
#' @param digits Number of digits to use when formatting percentages.
#' @param NA.action Character. How to deal with missing values in `x`.
#' Option `"category"` treats `NA` as its own level and reports the count. In that case,
#' option `"NA.name"` controls the name of this category.
#' Option `"omit"` simply disregards all `NA` values. Percentages will be of non-missing entries.
#' Option `"report.complete"` is like `"omit"`, except it reports the number and percentage of complete observations.
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
    NA.action = "category",
    NA.report.complete = "Complete, n (%)",
    NA.report.complete.name = "Categories, n (% of complete)",
    NA.name = "Missing"
){
  if(is.factor(x)) levs <- levels(x)
  else levs <- unique(x)

  if(NA.action == "category"){
    res <- lapply(
      levs,
      function(level){
        data.table(
          .level = level,
          count_binary(ifelse(is.na(x), 0, x == level), digits = digits)
        )
      }
    ) |> rbindlist()

    if(any(is.na(x))){
      res <- rbind(
        res,
        data.table(
          .level = NA.name,
          count_binary(is.na(x), digits = digits)
        )
      )
      levs <- c(levs, NA.name)
    }
  }
  else if(NA.action == "omit"){
    res <- lapply(
      levs,
      function(level){
        data.table(
          .level = level,
          count_binary(x[!is.na(x)] == level, digits = digits)
        )
      }
    ) |> rbindlist()
  }
  else if(NA.action == "report.complete"){
    res <- lapply(
      levs,
      function(level){
        data.table(
          .level = level,
          count_binary(x[!is.na(x)] == level, digits = digits)
        )
      }
    ) |> rbindlist()

    res <- rbind(
      cbind(
        .NA.level = NA.report.complete,
        data.table(
          .level = NA,
          count_binary(!is.na(x), digits = digits)
        )
      ),
      cbind(
        .NA.level = NA.report.complete.name,
        res
      )
    )
    res[, .NA.level := factor(.NA.level, levels = c(NA.report.complete, NA.report.complete.name))]
  }
  else{
    stop("Argument NA.action has value ", NA.action, " which is not allowed.")
  }

  res[, .level := factor(.level, levels = levs)]
}
