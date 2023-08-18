n_percent <- function(
    x,
    digits = 0,
    NA.show = TRUE,
    NA.name = "Missing"
){
  if(is.factor(x)) levs <- levels(x)
  else if(is.character(x)) levs <- unique(x)
  else stop("x must be factor or character.")

  count_level <- function(bool_vec){
    s <- sum(bool_vec)
    paste0(
      s,
      if(s == 0) NULL
      else{
        paste0(
          " (",
          format_elementwise(s / length(bool_vec) * 100, digits = digits),
          "%)"
        )
      }
    )
  }

  res <- lapply(
    levs,
    function(level){
      data.table::data.table(
        .level = level,
        count_level(ifelse(is.na(x), 0, x == level))
      )
    }
  ) |> data.table::rbindlist()

  if(NA.show & any(is.na(x))){
    res <- rbind(
      res,
      data.table::data.table(
        .level = NA.name,
        count_level(is.na(x))
      )
    )
    levs <- c(levs, NA.name)
  }
  res[, .level := factor(.level, levels = levs)]
}
