#' Helper for summarytable
#'
#' @param data,.by,default.numeric,default.factor,flatten.by.sep,total.column,total.name,indent.char See `summarytable()`.
#' @param ex An expression to evaluate in `data`. The RHS of an argument of `...`
#' in a call to summarytable.
#' @param ex_name Character of length 1. The LHS of an argument of `...` in
#' a call to summarytable.
#'
#' @return A data.table.
#' @import data.table
summarytable_worker <- function(
    data,
    ex,
    ex_name,
    .by = NULL,
    .by.NA.drop = FALSE,
    .by.NA.name = "Missing",
    default.numeric = mean_sd,
    default.factor = n_percent,
    flatten.by.sep = ", ",
    total.column = is.null(.by),
    total.name = "Total",
    indent.char = "  "
){
  if(!is.data.table(data)) stop("data must be a data.table.")
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
  ncol_identifier <- ncol(res) - length(.by) - 1
  names(res)[ncol(res)] <- ex_name
  if(.by.NA.drop & !is.null(.by)){
    # Must make this check only if !is.null(.by)
    if(any(res[, is.na(get(.by))])){
      res <- res[!is.na(get(.by))]
    }
  }
  else if(!is.null(.by)){
    res[is.na(get(.by)), eval(.by) := .by.NA.name]
  }

  # Change everything not factor or character to character
  res[, names(res) := lapply(.SD, function(x) if(!is.character(x) & !is.factor(x)) as.character(x) else x)]

  # If the result contains more than 1 cell per group in .by, there must be a one or more columns to identify rows.
  if(res[, .(.bysize = .N), by = .by][, any(.bysize > 1)]){
    if(ncol_identifier == 0) stop("Call ", ex_char, " returned more than 1 value per group in .by, but did not return a data.frame with at least 2 columns. In this case please return a data.frame with at least two columns: the first ncol columns containing a unique identifier for each row (e.g. group levels) and the last one the value of interest.")
  }
  # If creating total column, create dummy column .total for use with .by
  if(total.column){
    res[, .total := total.name]
    setcolorder(res, ".total")
    .by <- ".total"
  }
  # If result contains no identifier, add dummy .level column
  if(ncol_identifier == 0){
    res[, .level := as.character(NA)]
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
    ncol_identifier <- 1
  }

  # Cast according to .by
  res <- dcast(
    res,
    formula(
      paste0(
        paste0(colnames(res)[(length(.by) + 1):(ncol(res) - 1)], collapse = " + "),
        " ~ ",
        paste0(.by, collapse = " + ")
      )
    ),
    value.var = ex_name,
    drop = TRUE,
    sep = flatten.by.sep
  )

  ncol_by <- ncol(res) - ncol_identifier

  # Recursive helper function for formatting multiple identifier columns
  hierarchy_helper <- function(res_df, hierarchy_level){
    levs_name <- colnames(res_df)[1]
    # Indent according to hierarchy
    if(!(res_df[, is.na(get(levs_name)[1])] & nrow(res_df) == 1)){
      res_df[][
        if(is.na(get(levs_name)[1])) 2:nrow(res_df) else 1:nrow(res_df),
        eval(levs_name) := paste0(
          paste0(rep(indent.char, hierarchy_level), collapse = ""),
          get(levs_name)
        )
      ]
    }

    # Stopping condition
    if(ncol(res_df) == (ncol_by + 1)){
      colnames(res_df)[1] <- ".level"
      return(res_df)
    }

    # Respect factor levels if possible
    if(is.factor(res_df[, 1])) levs <- res_df[, levels(get(levs_name))]
    else levs <- res_df[, unique(get(levs_name))]

    lapply(
      levs,
      function(l){
        hierarchy_dt <- hierarchy_helper(res_df[get(levs_name) == l, 2:ncol(res_df)], hierarchy_level = hierarchy_level + 1)

        # If one row with NA level, do not indent
        if(nrow(hierarchy_dt) == 1 & is.na(hierarchy_dt[1,1])){
          hierarchy_dt[1, 1] <- l
          hierarchy_dt
        }
        else{
          tmp_dt <- rbind(
            data.table(.level = l),
            hierarchy_dt,
            fill = TRUE
          )
          tmp_dt[1, 2:ncol(tmp_dt) := ""]
          tmp_dt
        }
      }
    ) |>
      rbindlist()
  }

  # Make the result only have 1 identifier column
  res <- hierarchy_helper(res, hierarchy_level = 1)

  # If first row has NA identifier, give it expression name as rowname
  if(res[1, is.na(.level)]){
    res[1, .level := ex_name]
  }
  # If not, add empty row on top with expression name
  else{
    res <- rbind(
      data.table(.level = ex_name),
      res,
      fill = TRUE
    )
    res[1, 2:ncol(res) := ""]
  }

  res
}

