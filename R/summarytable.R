library(dplyr)
library(data.table)
library(Publish)
library(rlang)

data(Diabetes)
setDT(Diabetes)
Diabetes[
  ,
  `:=`(
    location = factor(location),
    AgeGroups = factor(AgeGroups),
    gender = factor(gender),
    frame = factor(frame)
  )
]

summarytable(Diabetes, `Weight, mean` = weight, `Age groups, N (%)` = AgeGroups)
summarytable(Diabetes, `Weight, mean` = weight, `Age groups, N (%)` = AgeGroups, .by = "location")
summarytable(Diabetes, `Weight, mean` = weight, `Age groups, N (%)` = AgeGroups, .by = "location", total.column = TRUE)
summarytable(Diabetes, `Weight, mean` = weight, `Age groups, N (%)` = AgeGroups, .by = c("location", "gender"))
summarytable(Diabetes, `Weight, mean` = weight, `Age groups, N (%)` = AgeGroups, .by = c("location", "gender"), total.column = TRUE)
summarytable(Diabetes, `Weight, mean` = weight, `Age groups, N (%)` = AgeGroups, .by = c("location", "gender", "frame"))


summarytable.worker <- function(
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
    setcolorder(res, ".total")
    .by <- ".total"
  }
  # If result only contains 1 value, add dummy .level column
  if(ncol(res) == length(.by) + 1){
    res[, .level := ""]
    setcolorder(
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
  res <- dcast(
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


summarytable <- function(
    data,
    ...,
    .by = NULL,
    flatten.by = FALSE,
    default.numeric = mean_sd,
    default.factor = n_percent,
    sep = ", ",
    total.column = FALSE,
    total.name = "Total",
    variable.name = "",
    return.list = FALSE
){
  if(!is.data.table(data)) data <- as.data.table()

  # Expressions for creating subtables
  .dots <- rlang::exprs(...)

  # We create one subtable for each expression and keep them here
  .tabs <- list()

  for(i in seq_along(.dots)){
    # Subtable
    .tabs[[i]] <- summarytable.worker(
      data,
      ex = .dots[[i]],
      ex_name = names(.dots)[i],
      .by = .by,
      default.numeric = default.numeric,
      default.factor = default.factor,
      sep = sep,
      total.name = total.name
    )

    # Add a total column if needed
    if(total.column & !is.null(.by)){
      .tabs[[i]] <- cbind(
        .tabs[[i]],
        summarytable.worker(
          data,
          ex = .dots[[i]],
          ex_name = names(.dots)[i],
          .by = NULL,
          default.numeric = default.numeric,
          default.factor = default.factor,
          sep = sep,
          total.column = TRUE,
          total.name = total.name
        )[, .level := NULL]
      )
    }
  }

  if(return.list) return(.tabs)
  else{
    tab <- rbindlist(.tabs)
    flextab <- tab |>
      flextable()

    # If !flatten.by, we create one row for each .by variable, merging cells horizontally as fitting
    if(!flatten.by & length(.by) > 1){
      header_names <- colnames(tab)[2:ncol(tab)]
      flextab <- flextab |>
        delete_part(part = "header")

      for(i in seq_along(.by)){
        levs <- unique(data[[.by[i]]])

        reg <- paste0("(", paste0(levs, collapse = "|"), ")")
        if(i < length(.by)) reg <- paste0(reg, sep)
        if(i > 1) reg <- paste0(sep, reg)
        col_levs <- stringr::str_match(header_names, reg)[, 2]
        if(i == length(.by) & total.column) col_levs[length(col_levs)] <- total.name
        flextab <- flextab |>
          add_header_row(values = c("", col_levs), colwidths = rep(1, ncol(tab)), top = FALSE)
      }
      flextab <- flextab |>
        merge_h(part = "header") |>
        align(i = 1:(length(.by) - 1), align = "center", part = "header")
    }

    # For each subtable
    n_rows <- 1
    for(i in seq_along(.tabs)){
      n_row_i <- nrow(.tabs[[i]])

      # Indent subtables with more than 1 row
      if(n_row_i > 1){
        flextab <- flextab |>
          prepend_chunks(i = (n_rows + 1):(n_rows + n_row_i - 1), j = 1, as_chunk("\t"))
      }
      # Apply background color to alternating subtables
      if(i %% 2 == 1){
        flextab <- flextab |>
          bg(i = n_rows:(n_rows + n_row_i - 1), bg = "lightgrey")
      }
      n_rows <- n_rows + nrow(.tabs[[i]])
    }

    # Change the label of the leftmost column
    flextab <- flextab |>
      set_header_labels(.level = variable.name)

    # Bold header
    flextab <- flextab |>
      bold(part = "header")

    # Border under header
    flextab <- flextab |>
      hline_bottom(part = "header", border = officer::fp_border(width = 1))

    # Inner borders
    flextab <- flextab |>
      border_inner(part = "body", border = officer::fp_border(color = "gray"))

    # Outer border
    flextab <- flextab |>
      border_outer()

    # Layout
    flextab <- flextab |>
      set_table_properties(layout = "autofit")

    flextab
  }
}


format_elementwise <- function(x, digits, scientific = FALSE, ...){
  sapply(x, \(y) format(round(y, digits = digits), nsmall = digits, scientific = scientific, ...))
}

n_percent <- function(
    x,
    digits = 1
){
  if(is.factor(x)) levs <- levels(x)
  else if(is.character(x)) levs <- unique(x)
  else stop("x must be factor or character.")

  res <- lapply(
    levs,
    function(level){
      data.table(.level = level, paste0(sum(x == level), " (", format_elementwise(mean(x == level) * 100, digits = digits), "%)"))
    }
  ) |> rbindlist()
  res[, .level := factor(.level, levels = levs)]
}

mean_sd <- function(
    x,
    digits.mean = 1,
    digits.sd = 1
){
  paste0(format_elementwise(mean(x, na.rm = TRUE), digits = digits.mean), " (", format_elementwise(sd(x, na.rm = TRUE), digits = digits.sd), ")")
}
