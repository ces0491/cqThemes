#' Create a correlation plot with consistent styling
#'
#' @param correlation_matrix A correlation matrix
#' @param title Title for the correlation plot
#' @param colors Colors for the correlation plot
#' @param method Method for visualizing correlations
#' @param order Order of the correlation matrix
#' @param type Type of correlation plot display
#' @param diag Logical: whether to display the diagonal
#'
#' @return The correlation plot object (invisibly)
#' @export
#'
#' @examples
#' cor_matrix <- cor(mtcars[, c("mpg", "disp", "hp", "drat", "wt")])
#' create_correlation_plot(cor_matrix, title = "Correlation Matrix of Car Features")
create_correlation_plot <- function(correlation_matrix,
                                    title = "Correlation Matrix",
                                    colors = NULL,
                                    method = "circle",
                                    order = "hclust",
                                    type = "upper",
                                    diag = FALSE) {

  # Set default colors if not provided
  if (is.null(colors)) {
    colors <- colorRampPalette(cq_colors(2, type = "main"))(200)
  }

  # Create the correlation plot
  corrplot::corrplot(
    correlation_matrix,
    method = method,
    type = type,
    order = order,
    tl.col = "black",
    tl.srt = 45,
    addCoef.col = "black",
    col = colors,
    diag = diag,
    title = title,
    mar = c(0, 0, 2, 0),
    number.cex = 0.9,
    tl.cex = 0.9
  )

  return(invisible(correlation_matrix))
}

#' Save a ggplot with consistent settings
#'
#' @param plot A ggplot object to save
#' @param filename Filename to save the plot
#' @param width Width of the plot in inches
#' @param height Height of the plot in inches
#' @param dpi Resolution in dots per inch
#' @param device Device to use, defaults to "png"
#'
#' @return Invisibly returns the path to the saved file
#' @export
#'
#' @examples
#' \dontrun{
#' p <- ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point() + theme_cq_pdf()
#' save_plot(p, "myplot.png", width = 8, height = 6)
#' }
save_plot <- function(plot, filename, width = 8, height = 6, dpi = 300, device = "png") {
  # Ensure ggplot2 is loaded
  requireNamespace("ggplot2", quietly = TRUE)

  # Save the plot
  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    device = device
  )

  # Print confirmation message
  message(paste("Plot saved to", filename))

  return(invisible(filename))
}

#' Helper function to safely install and load required packages
#'
#' @param packages Character vector of package names
#' @param quiet Logical: whether to suppress messages
#'
#' @return Invisibly returns TRUE if all packages loaded successfully
#' @export
#'
#' @examples
#' \dontrun{
#' ensure_packages(c("ggplot2", "dplyr", "knitr"))
#' }
ensure_packages <- function(packages, quiet = FALSE) {
  # Check which packages need to be installed
  new_packages <- packages[!packages %in% utils::installed.packages()[,"Package"]]

  # Install missing packages
  if (length(new_packages) > 0) {
    if (!quiet) {
      message("Installing missing packages: ", paste(new_packages, collapse = ", "))
    }
    utils::install.packages(new_packages, quiet = quiet)
  }

  # Load all packages
  sapply(packages, function(pkg) {
    if (!quiet) {
      message("Loading package: ", pkg)
    }
    suppressPackageStartupMessages(
      library(pkg, character.only = TRUE)
    )
  })

  return(invisible(TRUE))
}

#' Convert ggplot2 theme to plotly layout
#'
#' Helper function to convert ggplot2 theme settings to plotly layout
#'
#' @param theme A ggplot2 theme object
#'
#' @return A list of plotly layout settings
#' @export
#'
#' @examples
#' \dontrun{
#' theme_settings <- ggplot2_theme_to_plotly(theme_cq_html())
#' }
ggplot2_theme_to_plotly <- function(theme) {
  # Extract relevant theme elements for plotly
  font_size <- theme$text$size %||% 14
  font_family <- theme$text$family %||% "sans-serif"
  title_size <- theme$plot.title$size %||% (font_size * 1.2)

  # Default plotly settings
  plotly_settings <- list(
    font = list(
      family = font_family,
      size = font_size
    ),
    title = list(
      font = list(
        family = font_family,
        size = title_size
      )
    ),
    margin = list(
      l = 50,
      r = 50,
      b = 80,
      t = 100,
      pad = 4
    ),
    paper_bgcolor = "rgba(255,255,255,0)",
    plot_bgcolor = "rgba(240,240,240,0.5)",
    xaxis = list(
      title = list(
        font = list(
          family = font_family,
          size = font_size
        )
      ),
      tickfont = list(
        family = font_family,
        size = font_size * 0.8
      ),
      gridcolor = "#e0e0e0",
      zeroline = TRUE,
      zerolinecolor = "#9e9e9e"
    ),
    yaxis = list(
      title = list(
        font = list(
          family = font_family,
          size = font_size
        )
      ),
      tickfont = list(
        family = font_family,
        size = font_size * 0.8
      ),
      gridcolor = "#e0e0e0",
      zeroline = TRUE,
      zerolinecolor = "#9e9e9e"
    ),
    legend = list(
      font = list(
        family = font_family,
        size = font_size * 0.8
      ),
      orientation = "h",
      xanchor = "center",
      yanchor = "top",
      x = 0.5,
      y = -0.2
    )
  )

  return(plotly_settings)
}

# Helper function for NULL default handling, used internally
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
