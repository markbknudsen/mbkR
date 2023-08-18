summarytable_worker <- function(
    data,
    ex,
    ex_name,
    .by = NULL,
    default.numeric = mean_sd,
    default.factor = n_percent,
    sep = ", ",
    total.column = is.null(.by),
    total.name = "Total"
){
  if(!data.table::is.data.table(data)) stop("data must be a data.table.")
  if(!total.column & is.null(.by)) stop("If .by is NULL, total.column must be TRUE.")
  else if(total.column & !is.null(.by)) stop("If total.column is TRUE then .by must be NULL.")

  ex_char <- as.character(c(ex))

  # If expression is column name, apply default method
  if(ex_char %in% names(data)){

    # If column is factor or character, apply default.factor function
    if(is.factor(data[[ex_char]]) | is.character(data[[ex_char]])){
      ex <- parse(text = paste0("default.factor(", ex_char, ")"))
      ex_char <- as.character(c(ex))
    }
    # If column is numeric, apply default.numeric function
    else if(is.numeric(data[[ex_char]])){
      ex <- parse(text = paste0("default.numeric(", ex_char, ")"))
      ex_char <- as.character(c(ex))
    }
    # Else, we do not know what to do
    else stop("No defaults for column ", ex_char, " of class ", class(data[[ex_char]]), ", must be factor, character or numeric.")
  }

  # Add result of calculation
  res <- data[, eval(ex), by = .by]
  names(res)[ncol(res)] <- ex_name

  # If the result contains more than 1 cell per group in .by, there must be a .level column to identify rows.
  if(res[, .(.bysize = .N), by = .by][, any(.bysize > 1)]){
    if(ncol(res) != length(.by) + 2) stop("Call ", ex_char, " returned more than 1 value per group in .by, but did not return a data.frame with 2 columns. In this case please return a data.frame with two columns: the first being named '.level', containing a unique identifier for each row (e.g. group levels) and the second one the value of interest (name irrelevant).")
    else if(names(res)[length(.by) + 1] != ".level") stop("Call ", ex_char, " returned a data.frame with 2 columns, but the first was not named '.level'")
  }
  # If creating total column, create dummy column .total for use with .by
  if(total.column){
    res[, .total := total.name]
    data.table::setcolorder(res, ".total")
    .by <- ".total"
  }
  # If result only contains 1 value, add dummy .level column
  if(ncol(res) == length(.by) + 1){
    res[, .level := ""]
    data.table::setcolorder(
      res,
      c(
        if(length(.by) > 0){
          names(res)[1:length(.by)]
        } else NULL,
        ".level",
        ex_name
      )
    )
  }

  # Cast according to .by
  res <- data.table::dcast(
    res,
    formula(
      paste0(
        ".level ~ ",
        paste0(.by, collapse = " + ")
      )
    ),
    value.var = ex_name,
    drop = FALSE,
    sep = sep
  )

  # If one row, change rowname to expression name
  if(nrow(res) == 1){
    res[, .level := ex_name]
  }
  # If more than one row, add empty row on top with expression name
  if(nrow(res) > 1){
    res <- rbind(
      data.table(.level = ex_name),
      res,
      fill = TRUE
    )
    res[1, 2:ncol(res) := ""]
  }

  res
}
