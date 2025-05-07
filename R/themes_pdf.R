#' Apply PDF theme settings for statistical reports
#'
#' This function sets up the appropriate theme settings for PDF reports
#' created with Quarto or R Markdown.
#'
#' @param seed Optional random seed for reproducibility, defaults to 9104
#' @param echo Logical: whether to show code chunks, defaults to FALSE
#' @param dpi Resolution for plots, defaults to 300
#'
#' @return Invisibly returns NULL
#' @export
#'
#' @examples
#' \dontrun{
#' # In a Quarto or R Markdown document
#' library(cqThemes)
#' use_pdf_theme()
#' }
use_pdf_theme <- function(seed = 9104, echo = FALSE, dpi = 300) {
  # Set seed for reproducibility
  set.seed(seed)

  # Set knitr options
  setup_knitr_pdf(echo = echo, dpi = dpi)

  # Print message to let user know the theme has been applied
  message("cqThemes PDF theme applied successfully!")
  message("- Use theme_cq_pdf() for ggplot2 visualizations")
  message("- Use format_table_pdf() for formatted tables")
  message("- Use cq_colors() for consistent color palettes")

  return(invisible(NULL))
}

#' Setup knitr options for PDF documents
#'
#' Configures knitr chunk options optimized for PDF output
#'
#' @param echo Logical: whether to show code chunks
#' @param dpi Resolution for plots
#'
#' @return Invisibly returns NULL
#' @export
#'
#' @examples
#' \dontrun{
#' # In a Quarto or R Markdown document
#' library(cqThemes)
#' setup_knitr_pdf()
#' }
setup_knitr_pdf <- function(echo = FALSE, dpi = 300) {
  knitr::opts_chunk$set(
    echo = echo,            # Don't show code in final output
    warning = FALSE,        # Suppress warnings
    message = FALSE,        # Suppress messages
    fig.align = "center",   # Center figures
    fig.pos = "H",          # Position figures exactly here
    fig.width = 8,          # Set wider default figure width
    fig.height = 6,         # Set taller default figure height
    out.width = "95%",      # Set output figure width
    dpi = dpi,              # Higher resolution
    results = "asis"        # Output results as-is
  )

  return(invisible(NULL))
}

#' Custom ggplot2 theme for PDF reports
#'
#' A clean, publication-ready theme for ggplot2 visualizations in PDF documents
#'
#' @param base_size Base font size for the theme
#' @param base_family Base font family for the theme
#'
#' @return A ggplot2 theme object
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = mpg, y = hp)) +
#'   geom_point() +
#'   theme_cq_pdf()
theme_cq_pdf <- function(base_size = 12, base_family = "") {
  # Create theme based on theme_minimal
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = base_size*1.2, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = base_size, hjust = 0.5, color = "gray40"),
      axis.title = ggplot2::element_text(size = base_size),
      axis.text = ggplot2::element_text(size = base_size*0.8),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = base_size*0.8),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(fill = NA, color = "gray90"),
      plot.margin = ggplot2::unit(c(1, 1, 1, 1), "cm") # Using base unit function
    )
}
