#' Save flextable as .docx
#'
#' @param table A flextable object.
#' @param path A character containing the file destination.
#' @param orientation A character, either `"portrait"` or `"landscape"`.
#' @param fontname Character. Name of font to use (must be installed on system).
#' @param fontsize Numeric. Fontsize.
#'
#' @return Path to saved file.
#' @seealso [summarytable()]
#' @export
#'
#' @examples
#' # Load the diamonds data
#' library(data.table)
#' library(ggplot2)
#'
#' data(diamonds)
#'
#' tab <- summarytable(
#'   diamonds,
#'   `Cut, n (%)` = cut,
#'   `Weight, mean (sd)` = carat
#' )
#' # save_table_docx(tab, "path/to/file/table.docx")
save_table_docx <- function(
  table,
  path,
  orientation = "portrait",
  fontname = "Times New Roman",
  fontsize = 11
){
  table |>
    flextable::font(
      fontname = fontname,
      part = "all"
    ) |>
    flextable::fontsize(
      size = fontsize
    ) |>
    flextable::save_as_docx(
      path = path,
      pr_section = officer::prop_section(
        page_size = officer::page_size(orient = orientation)
      )
    )
}
