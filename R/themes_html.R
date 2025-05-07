#' Apply HTML theme settings for interactive reports
#'
#' This function sets up the appropriate theme settings for HTML reports
#' created with Quarto or R Markdown, including interactive elements.
#'
#' @param seed Optional random seed for reproducibility, defaults to 9104
#' @param echo Logical: whether to show code chunks, defaults to FALSE
#' @param interactive Logical: whether to use interactive plotly visualizations, defaults to TRUE
#'
#' @return Invisibly returns NULL
#' @export
#'
#' @examples
#' \dontrun{
#' # In a Quarto or R Markdown document
#' library(cqThemes)
#' use_html_theme()
#' }
use_html_theme <- function(seed = 9104, echo = FALSE, interactive = TRUE) {
  # Set seed for reproducibility
  set.seed(seed)

  # Set knitr options
  setup_knitr_html(echo = echo, interactive = interactive)

  # Define CSS for HTML reports
  css <- "
  body {
    font-family: 'Helvetica Neue', Arial, sans-serif;
    line-height: 1.6;
    color: #333;
    max-width: 1000px;
    margin: 0 auto;
    padding: 0 15px;
  }
  h1, h2, h3, h4, h5, h6 {
    color: #2c3e50;
    font-weight: 600;
  }
  h1 {
    border-bottom: 2px solid #5ab4ac;
    padding-bottom: 10px;
  }
  table {
    width: 100%;
    border-collapse: collapse;
    margin: 20px 0;
  }
  th {
    background-color: #f7f7f7;
    font-weight: 600;
  }
  th, td {
    padding: 8px 12px;
    border: 1px solid #e0e0e0;
    text-align: left;
  }
  tr:nth-child(even) {
    background-color: #f9f9f9;
  }
  .caption {
    font-style: italic;
    font-size: 0.9em;
    color: #666;
    margin-top: 5px;
    text-align: center;
  }
  .plot-container {
    margin: 20px 0;
    padding: 10px;
    border: 1px solid #eaeaea;
    box-shadow: 0 2px 4px rgba(0,0,0,0.05);
  }
  "

  # Output the CSS
  htmltools::tags$style(css)

  # Store the interactive setting as an option
  options(cq_interactive = interactive)

  # Print message to let user know the theme has been applied
  message("cqThemes HTML theme applied successfully!")
  message("- Use theme_cq_html() for ggplot2 visualizations")
  message("- Use format_table_html() for formatted tables")
  message("- Use cq_colors() for consistent color palettes")
  if (interactive) {
    message("- Interactive mode enabled: static plots will be converted to plotly automatically")
  }

  return(invisible(NULL))
}

#' Setup knitr options for HTML documents
#'
#' Configures knitr chunk options optimized for HTML output
#'
#' @param echo Logical: whether to show code chunks
#' @param interactive Logical: whether to set up for interactive visualizations
#'
#' @return Invisibly returns NULL
#' @export
#'
#' @examples
#' \dontrun{
#' # In a Quarto or R Markdown document
#' library(cqThemes)
#' setup_knitr_html()
#' }
setup_knitr_html <- function(echo = FALSE, interactive = TRUE) {
  knitr::opts_chunk$set(
    echo = echo,            # Control code visibility
    warning = FALSE,        # Suppress warnings
    message = FALSE,        # Suppress messages
    fig.align = "center",   # Center figures
    out.width = "100%",     # Responsive width
    dpi = 144,              # Higher resolution for screens
    fig.width = 10,         # Wider default figure width for responsive design
    fig.height = 7,         # Taller default figure height
    cache = TRUE            # Cache results for faster rendering
  )

  # Set plot hook for converting ggplot2 to plotly if interactive mode is enabled
  if (interactive) {
    # Add a custom knit hook for plotly conversion
    knitr::knit_hooks$set(plot = function(x, options) {
      # Only convert if not explicitly disabled
      if (!isFALSE(options$plotly)) {
        # Get the last plot if available
        p <- get("last_plot", envir = ggplot2:::.globals)
        if (!is.null(p)) {
          # Convert to plotly
          plt <- plotly::ggplotly(p) %>%
            plotly::layout(
              autosize = TRUE,
              margin = list(l = 50, r = 50, b = 100, t = 100, pad = 4),
              font = list(family = "Helvetica Neue, Arial, sans-serif"),
              paper_bgcolor = 'rgba(0,0,0,0)',
              plot_bgcolor = 'rgba(0,0,0,0)'
            ) %>%
            plotly::config(
              displayModeBar = TRUE,
              displaylogo = FALSE,
              modeBarButtonsToRemove = list(
                'sendDataToCloud', 'autoScale2d', 'resetScale2d',
                'hoverClosestCartesian', 'hoverCompareCartesian'
              )
            )

          # Create a div with appropriate styling
          div_id <- paste0("plot_", sample(1000:9999, 1))
          html <- htmltools::div(
            id = div_id,
            class = "plot-container",
            plt
          )

          return(as.character(html))
        }
      }
      # Fall back to default behavior if not a ggplot or plotly is disabled
      knitr::hook_plot_html(x, options)
    })
  }

  return(invisible(NULL))
}

#' Custom ggplot2 theme for HTML reports
#'
#' A clean, web-friendly theme for ggplot2 visualizations
#' with responsive design considerations
#'
#' @param base_size Base font size for the theme
#' @param base_family Base font family for the theme
#' @param grid_col Color for grid lines, defaults to "#ebebeb"
#'
#' @return A ggplot2 theme object
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = mpg, y = hp)) +
#'   geom_point() +
#'   theme_cq_html()
theme_cq_html <- function(base_size = 14, base_family = "sans-serif", grid_col = "#ebebeb") {
  # Create theme based on theme_light for better web readability
  ggplot2::theme_light(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      # Text elements
      plot.title = ggplot2::element_text(
        size = base_size*1.3,
        face = "bold",
        hjust = 0.5,
        margin = ggplot2::margin(b = 10)
      ),
      plot.subtitle = ggplot2::element_text(
        size = base_size*1.1,
        hjust = 0.5,
        color = "#5f6368",
        margin = ggplot2::margin(b = 15)
      ),
      axis.title = ggplot2::element_text(
        size = base_size*0.9,
        color = "#5f6368"
      ),
      axis.text = ggplot2::element_text(
        size = base_size*0.8,
        color = "#5f6368"
      ),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),

      # Legend elements
      legend.position = "bottom",
      legend.background = ggplot2::element_rect(fill = "transparent"),
      legend.key = ggplot2::element_rect(fill = "transparent"),
      legend.title = ggplot2::element_text(size = base_size*0.8),
      legend.text = ggplot2::element_text(size = base_size*0.7),
      legend.box.spacing = ggplot2::unit(0.1, "cm"),

      # Grid and panel elements
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = grid_col),
      panel.border = ggplot2::element_rect(color = "#d6d6d6", fill = NA),
      panel.background = ggplot2::element_rect(fill = "#ffffff"),
      plot.background = ggplot2::element_rect(fill = "#ffffff", color = NA),

      # Plot margins
      plot.margin = ggplot2::margin(15, 15, 15, 15),

      # Facet elements
      strip.background = ggplot2::element_rect(fill = "#f7f7f7", color = "#d6d6d6"),
      strip.text = ggplot2::element_text(
        size = base_size*0.8,
        face = "bold",
        color = "#5f6368"
      )
    )
}

#' Convert a ggplot object to an interactive plotly visualization
#'
#' Applies consistent styling to the plotly output to match the cqThemes aesthetic
#'
#' @param plot A ggplot2 object to convert
#' @param tooltip Variables to show in tooltip, defaults to "all"
#' @param width Width in pixels, defaults to NULL (automatic)
#' @param height Height in pixels, defaults to NULL (automatic)
#'
#' @return A plotly object
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = mpg, y = hp)) +
#'   geom_point() +
#'   theme_cq_html()
#' ggplot_to_plotly(p)
ggplot_to_plotly <- function(plot, tooltip = "all", width = NULL, height = NULL) {
  # Convert ggplot to plotly
  plt <- plotly::ggplotly(plot, tooltip = tooltip, width = width, height = height) %>%
    plotly::layout(
      autosize = TRUE,
      margin = list(l = 50, r = 50, b = 100, t = 100, pad = 4),
      font = list(family = "Helvetica Neue, Arial, sans-serif"),
      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor = 'rgba(245,245,245,0.5)'
    ) %>%
    plotly::config(
      displayModeBar = TRUE,
      displaylogo = FALSE,
      modeBarButtonsToRemove = list(
        'sendDataToCloud', 'autoScale2d', 'resetScale2d',
        'hoverClosestCartesian', 'hoverCompareCartesian'
      )
    )

  return(plt)
}
