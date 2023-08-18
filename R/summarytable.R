library(data.table)
library(Publish)
library(rlang)
library(flextable)

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

Diabetes[sample(.N, 10), AgeGroups := NA]

summarytable(Diabetes, `Weight, mean` = weight, `Age groups, N (%)` = AgeGroups)
summarytable(Diabetes, `Weight, mean` = weight, `Age groups, N (%)` = AgeGroups, .by = "location")
summarytable(Diabetes, `Weight, mean` = weight, `Age groups, N (%)` = AgeGroups, .by = "location", total.column = TRUE)
summarytable(Diabetes, `Weight, mean` = weight, `Age groups, N (%)` = AgeGroups, .by = c("location", "gender"))
summarytable(Diabetes, `Weight, mean` = weight, `Age groups, N (%)` = AgeGroups, .by = c("location", "gender"), total.column = TRUE)
summarytable(Diabetes, `Weight, mean` = weight, `Age groups, N (%)` = AgeGroups, .by = c("location", "gender", "frame"))


tab <- summarytable(Diabetes, `Weight, mean` = weight, `Age groups, N (%)` = AgeGroups, .by = c("location", "gender"), total.column = TRUE)
save_table_docx(tab, path = "C:/Users/wrx638/Desktop/test_table.docx")

summarytable <- function(
    data,
    ...,
    .by = NULL,
    flatten.by = FALSE,
    sep = ", ",
    N.row = .N,
    N.row.header = TRUE,
    N.row.name = "n",
    default.numeric = mean_sd,
    default.factor = n_percent,
    total.column = FALSE,
    total.name = "Total",
    variable.name = "",
    indent.char = "  ",
    return.list = FALSE
){
  if(!is.data.table(data)) data <- data.table::as.data.table()

  # Expressions for creating subtables
  .dots <- rlang::exprs(...)

  N.row.expr <- rlang::enexpr(N.row)
  if(!is.null(N.row.expr)){
    .dots <- c(N.row.expr, .dots)
    names(.dots)[1] <- N.row.name
  }

  # We create one subtable for each expression and keep them here
  .tabs <- list()

  for(i in seq_along(.dots)){
    # Subtable
    .tabs[[i]] <- summarytable_worker(
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
        summarytable_worker(
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
    if(!is.null(N.row.expr) & N.row.header){
      N.row.values <- .tabs[[1]][, 2:ncol(.tabs[[1]])]
      .tabs <- .tabs[-1]
    }

    tab <- data.table::rbindlist(.tabs)

    flextab <- tab |>
      flextable::flextable()

    header_names <- colnames(tab)[2:ncol(tab)]

    # If !flatten.by, we create one row for each .by variable, merging cells horizontally as fitting
    if(!flatten.by & length(.by) > 1){
      flextab <- flextab |>
        flextable::delete_part(part = "header")

      for(i in seq_along(.by)){
        levs <- unique(data[[.by[i]]])

        reg <- paste0("(", paste0(levs, collapse = "|"), ")")

        if(i == 1) reg <- paste0("^", reg)
        else if(i > 1) reg <- paste0(sep, reg)

        if(i == length(.by)) reg <- paste0(reg, "$")
        else if(i < length(.by)) reg <- paste0(reg, sep)

        col_levs <- stringr::str_match(header_names, reg)[, 2]
        if(i == length(.by)){
          if(total.column) col_levs[length(col_levs)] <- total.name
          if(!is.null(N.row.expr) & N.row.header){
            header_names <- paste0(col_levs, " (", N.row.name, "=", N.row.values, ")")
          }
        }
        flextab <- flextab |>
          flextable::add_header_row(values = c("", col_levs), colwidths = rep(1, ncol(tab)), top = FALSE)
      }
      flextab <- flextab |>
        flextable::merge_h(part = "header") |>
        flextable::align(i = 1:(length(.by) - 1), align = "center", part = "header")
    }

    # For each subtable
    n_rows <- 1
    for(i in seq_along(.tabs)){
      n_row_i <- nrow(.tabs[[i]])

      # Indent subtables with more than 1 row
      if(n_row_i > 1){
        flextab <- flextab |>
          flextable::prepend_chunks(i = (n_rows + 1):(n_rows + n_row_i - 1), j = 1, flextable::as_chunk(indent.char))
      }
      # Apply background color to alternating subtables
      if(i %% 2 == 1){
        flextab <- flextab |>
          flextable::bg(i = n_rows:(n_rows + n_row_i - 1), bg = "lightgrey")
      }
      n_rows <- n_rows + nrow(.tabs[[i]])
    }

    # Change the label of the leftmost column, and possibly add N.row
    if(!is.null(N.row.expr) & N.row.header &(length(.by) <= 1)){
      header_names <- paste0(header_names, " (", N.row.name, "=", N.row.values, ")")
    }
    flextab <- flextab |>
      flextable::set_header_labels(values = c(variable.name, header_names))

    # Bold header
    flextab <- flextab |>
      flextable::bold(part = "header")

    # Border under header
    flextab <- flextab |>
      flextable::hline_bottom(part = "header", border = officer::fp_border(width = 1))

    # Inner borders
    flextab <- flextab |>
      flextable::border_inner(part = "all", border = officer::fp_border(color = "gray"))

    # Outer border of table
    flextab <- flextab |>
      flextable::border_outer()

    # Layout
    flextab <- flextab |>
      flextable::set_table_properties(layout = "autofit")

    flextab
  }
}
