#' Format a table for PDF documents
#'
#' Creates a well-formatted table for PDF documents using kableExtra
#'
#' @param data Data frame to display as a table
#' @param caption Optional table caption
#' @param digits Number of digits to display for numeric columns
#' @param font_size Font size for the table
#' @param highlight_row Row number(s) to highlight
#' @param highlight_col Column number(s) to highlight
#' @param alignment Character vector of column alignments ("l", "c", "r")
#' @param full_width Logical: whether to make the table full width
#' @param striped Logical: whether to add alternating row colors
#'
#' @return A kableExtra formatted table
#' @export
#'
#' @examples
#' format_table_pdf(head(mtcars), caption = "Motor Trend Car Road Tests")
format_table_pdf <- function(data, caption = NULL, digits = 3, font_size = 10,
                             highlight_row = NULL, highlight_col = NULL,
                             alignment = NULL, full_width = FALSE, striped = TRUE) {

  # Set default alignment if not specified
  if (is.null(alignment)) {
    alignment <- ifelse(sapply(data, is.numeric), "c", "l")
  }

  # Create table with kableExtra
  formatted_table <- kableExtra::kbl(
    data,
    caption = caption,
    format = "latex",
    booktabs = TRUE,
    align = alignment,
    digits = digits,
    linesep = ""
  ) %>%
    kableExtra::kable_styling(
      latex_options = c("HOLD_position", if(striped) "striped", if(full_width) "scale_down"),
      font_size = font_size,
      position = "center",
      full_width = full_width
    )

  # Add row highlighting if specified
  if (!is.null(highlight_row)) {
    formatted_table <- kableExtra::row_spec(
      formatted_table,
      highlight_row,
      bold = TRUE,
      background = "#E8F4F8"
    )
  }

  # Add column highlighting if specified
  if (!is.null(highlight_col)) {
    formatted_table <- kableExtra::column_spec(
      formatted_table,
      highlight_col,
      bold = TRUE
    )
  }

  return(formatted_table)
}

#' Format a table for HTML documents
#'
#' Creates an interactive HTML table using DT package
#'
#' @param data Data frame to display as a table
#' @param caption Optional table caption
#' @param digits Number of digits to display for numeric columns
#' @param highlight_row Row number(s) to highlight
#' @param highlight_col Column names to highlight
#' @param compact Logical: whether to use compact layout
#' @param scrollX Logical: whether to enable horizontal scrolling
#' @param buttons Character vector of buttons to include (e.g. "copy", "csv", "excel")
#'
#' @return A DT datatable object
#' @export
#'
#' @examples
#' format_table_html(head(mtcars), caption = "Motor Trend Car Road Tests")
format_table_html <- function(data, caption = NULL, digits = 3,
                              highlight_row = NULL, highlight_col = NULL,
                              compact = TRUE, scrollX = TRUE,
                              buttons = c("copy", "csv", "excel")) {

  # Create DT options
  dt_options <- list(
    pageLength = 10,
    autoWidth = TRUE,
    dom = "Bfrtip",
    buttons = buttons,
    scrollX = scrollX
  )

  # Apply compact class if requested
  class_option <- ifelse(compact, "compact hover stripe", "hover stripe")

  # Create datatable
  dt <- DT::datatable(
    data,
    caption = caption,
    options = dt_options,
    rownames = FALSE,
    class = class_option,
    filter = "top",
    extensions = c("Buttons", "Responsive")
  )

  # Format numeric columns
  numeric_cols <- which(sapply(data, is.numeric))
  if (length(numeric_cols) > 0) {
    dt <- DT::formatRound(dt, columns = numeric_cols, digits = digits)
  }

  # Highlight rows if specified
  if (!is.null(highlight_row)) {
    dt <- dt %>% DT::formatStyle(
      columns = 0,  # Row names
      target = "row",
      rows = highlight_row,
      backgroundColor = "#E8F4F8",
      fontWeight = "bold"
    )
  }

  # Highlight columns if specified
  if (!is.null(highlight_col)) {
    dt <- dt %>% DT::formatStyle(
      columns = highlight_col,
      fontWeight = "bold"
    )
  }

  return(dt)
}

#' Format a table for RevealJS presentations
#'
#' Creates a simplified table optimized for presentation slides
#'
#' @param data Data frame to display as a table
#' @param caption Optional table caption
#' @param digits Number of digits to display for numeric columns
#' @param max_rows Maximum number of rows to display (NULL for all)
#' @param highlight_row Row number(s) to highlight
#' @param highlight_col Column number(s) to highlight
#'
#' @return A kableExtra formatted table for HTML output
#' @export
#'
#' @examples
#' format_table_reveal(head(mtcars, 5), caption = "Motor Trend Car Road Tests")
format_table_reveal <- function(data, caption = NULL, digits = 2, max_rows = 8,
                                highlight_row = NULL, highlight_col = NULL) {

  # Limit the number of rows for better slide readability if specified
  if (!is.null(max_rows) && nrow(data) > max_rows) {
    data <- data[1:max_rows, ]
  }

  # Create table with kableExtra
  formatted_table <- kableExtra::kbl(
    data,
    caption = caption,
    format = "html",
    digits = digits
  ) %>%
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width = TRUE,
      font_size = 16
    )

  # Add row highlighting if specified
  if (!is.null(highlight_row)) {
    formatted_table <- kableExtra::row_spec(
      formatted_table,
      highlight_row,
      bold = TRUE,
      background = "#E8F4F8"
    )
  }

  # Add column highlighting if specified
  if (!is.null(highlight_col)) {
    formatted_table <- kableExtra::column_spec(
      formatted_table,
      highlight_col,
      bold = TRUE
    )
  }

  return(formatted_table)
}
