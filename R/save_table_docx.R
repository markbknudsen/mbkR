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
