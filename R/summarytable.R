#' Create a summary table
#'
#' @description
#' A function for easy creation of publication-ready summary tables, i.e. "table 1" in many
#' medical publications. The function has simple defaults and allows for user-defined summary functions.
#'
#' @details
#' The function allows for evaluating arbitrary user-supplied code as the RHS of `...`.
#' However, the result of such an expression must evaluate to either:
#'
#' * A scalar (like e.g. the `mean()` function).
#' * Equivalent to above, a `data.table` with one row and two columns, the first
#' column being `NA` and the second containing the scalar value.
#' * More generally, a data.table with multiple rows and columns. The last column contains
#' the values to display in the table, and the remaining columns contain information to identify
#' the cells, e.g. the levels of a factor. These columns should get finer going from left to right.
#' If the top row has `NA` identifier then the result of that row will not be indented in the final table.
#'
#' @param data A data.frame-like object.
#' @param ... Name-expression pairs `LHS = RHS` where LHS is the text to write in the
#' leftmost column of the table, and RHS is an R expression that will be evaluated in `data`,
#' see details and examples.
#' @param .by A character vector of column/variable names in `data` that the table should
#' stratify on, i.e. create one column per level of `.by` (or combination of levels if multiple
#' columns are given).
#' @param flatten.by Boolean. If `FALSE` (default), the table header will have one row for
#' each variable in `.by`. If TRUE, will condense these into a single row.
#' @param flatten.by.sep Character of length 1. If `flatten.by` is `TRUE`, this character
#' will separate levels of variables in `.by` in the header.
#' @param .by.NA.drop,.by.NA.name If there are `NA` values in the`.by` columns, a separate column
#' will be created for rows with missing values when `.by.NA.drop = FALSE`. The name of this column
#' is controlled by `.by.NA.name`
#' @param N.row An expression that gives the number observations in each group, which by
#' default is put in the header.
#' @param N.row.header Boolean. If TRUE (default), the result of `N.row` is put in the
#' header. Otherwise it is put as the first row of the table.
#' @param N.row.name Character of length 1. Name of `N.row` row, or the symbol put into
#' the header in front of the result.
#' @param default.numeric,default.factor A function that will be applied to a numeric or
#' factor/character variable in data when only its name is given as the RHS of an expression in `...`.
#' @param total.column Boolean. Only has effect if `.by` is not `NULL`, in which case
#' it controls if an column is added for the unstratified data (`TRUE`) or not (`FALSE`, the default).
#' @param total.name Character of length 1. Name of the total column (if present).
#' @param variable.name Character of length 1. The name of the leftmost column of the table
#' (the empty character `""` by default).
#' @param indent.char Character of length 1. Character to indent sublevels of table by.
#' Default is two spaces (`"  "`), use `"\t"` for a tab character, although this seems
#' to give rather ugly results when saving to .docx format.
#' @param color.cells Character. Color to apply to cells of alternating subtables.
#' Set `NULL` or `FALSE` for no coloring.
#' @param border.inner.v,border.inner.h Whether to add inner vertical and horizontal rules.
#' @param autofit Whether automatically resize the table. If `TRUE`, newlines will be ignored.
#' @param return.list Boolean. If `TRUE`, the function will return a list data.tables
#' instead of a formatted flextable.
#'
#' @return A formatted flextable object.
#' @seealso [save_table_docx()]
#' @import data.table
#' @export
#'
#' @examples
#' # Load the diamonds data
#' library(data.table)
#' library(ggplot2)
#'
#' data(diamonds)
#' setDT(diamonds)
#'
#' diamonds[sample(.N, 10), cut := NA][sample(.N, 10), carat := NA]
#'
#' # When only giving variables as RHS, the functions default.numeric or
#' # default.factor are applied as appropriate.
#' summarytable(
#'   diamonds,
#'   `Cut, n (%)` = cut,
#'   `Weight, mean (sd)` = carat
#' )
#'
#' # Other functions can be applied to columns freely as if they were variables
#' summarytable(
#'   diamonds,
#'   `Cut, n (%)` = cut,
#'   `Weight, mean (sd)` = carat,
#'   `Square root depth` = mean(sqrt(depth)),
#'   `Price in USD, median (Q1, Q3)` = median_quantiles(price)
#' )
#'
#' # We can stratify by discrete variables using .by
#' summarytable(
#'   diamonds,
#'   `Cut, n (%)` = cut,
#'   `Weight, mean (sd)` = carat,
#'   `Square root depth` = mean(sqrt(depth)),
#'   `Price in USD, median (Q1, Q3)` = median_quantiles(price),
#'   .by = "color"
#' )
#'
#' # We can add a total column to a stratified table
#' summarytable(
#'   diamonds,
#'   `Cut, n (%)` = cut,
#'   `Weight, mean (sd)` = carat,
#'   `Square root depth, mean` = mean(sqrt(depth)),
#'   `Price in USD, median (Q1, Q3)` = median_quantiles(price),
#'   .by = "color",
#'   total.column = TRUE
#' )
summarytable <- function(
    data,
    ...,
    .by = NULL,
    flatten.by = FALSE,
    flatten.by.sep = ", ",
    .by.NA.drop = FALSE,
    .by.NA.name = "Missing",
    N.row = .N,
    N.row.header = TRUE,
    N.row.name = "n",
    default.numeric = mean_sd,
    default.factor = n_percent,
    total.column = FALSE,
    total.name = "Total",
    variable.name = "",
    indent.char = "  ",
    color.cells = "lightgrey",
    border.inner.v = FALSE,
    border.inner.h = TRUE,
    autofit = TRUE,
    return.list = FALSE
){
  if(!is.data.table(data)) data <- data.table(data)

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
      .by.NA.drop = .by.NA.drop,
      .by.NA.name = .by.NA.name,
      default.numeric = default.numeric,
      default.factor = default.factor,
      flatten.by.sep = flatten.by.sep,
      total.name = total.name,
      indent.char = indent.char
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
          .by.NA.drop = .by.NA.drop,
          .by.NA.name = .by.NA.name,
          default.numeric = default.numeric,
          default.factor = default.factor,
          flatten.by.sep = flatten.by.sep,
          total.column = TRUE,
          total.name = total.name,
          indent.char = indent.char
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

    tab <- rbindlist(.tabs)

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
        else if(i > 1) reg <- paste0(flatten.by.sep, reg)

        if(i == length(.by)) reg <- paste0(reg, "$")
        else if(i < length(.by)) reg <- paste0(reg, flatten.by.sep)

        col_levs <- stringr::str_match(header_names, reg)[, 2]
        if(i == length(.by)){
          if(total.column) col_levs[length(col_levs)] <- total.name
          if(!is.null(N.row.expr) & N.row.header){
            header_names <- paste0(col_levs, " (", N.row.name, "=", N.row.values, ")")
          }
          else{
            header_names <- col_levs
          }
        }
        flextab <- flextab |>
          flextable::add_header_row(values = c("", col_levs), colwidths = rep(1, ncol(tab)), top = FALSE)
      }
      flextab <- flextab |>
        flextable::merge_h(part = "header", i = 1:(flextable::nrow_part(flextab, part = "header") - 1)) |>
        flextable::align(i = 1:(length(.by) - 1), align = "center", part = "header")
    }

    # For each subtable
    if(!is.null(color.cells) & !isFALSE(color.cells)){
      n_rows <- 1
      for(i in seq_along(.tabs)){
        n_row_i <- nrow(.tabs[[i]])

        # Apply background color to alternating subtables
        if(i %% 2 == 1){
          flextab <- flextab |>
            flextable::bg(i = n_rows:(n_rows + n_row_i - 1), bg = color.cells)
        }
        n_rows <- n_rows + nrow(.tabs[[i]])
      }
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
    if(border.inner.v){
      flextab <- flextab |>
        flextable::border_inner_v(part = "all", border = officer::fp_border(color = "gray"))
    }
    if(border.inner.h){
      flextab <- flextab |>
        flextable::border_inner_h(part = "all", border = officer::fp_border(color = "gray"))
    }

    # Outer border of table
    flextab <- flextab |>
      flextable::border_outer()

    # Layout
    if(autofit){
      flextab <- flextab |>
        flextable::set_table_properties(layout = "autofit")
    }

    flextab
  }
}
