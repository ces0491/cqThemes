#' Apply RevealJS theme settings for presentations
#'
#' This function sets up the appropriate theme settings for RevealJS presentations
#' created with Quarto or R Markdown.
#'
#' @param seed Optional random seed for reproducibility, defaults to 9104
#' @param echo Logical: whether to show code chunks, defaults to FALSE
#' @param interactive Logical: whether to use interactive plotly visualizations, defaults to FALSE
#'
#' @return Invisibly returns NULL
#' @export
#'
#' @examples
#' \dontrun{
#' # In a Quarto or R Markdown presentation
#' library(cqThemes)
#' use_revealjs_theme()
#' }
use_revealjs_theme <- function(seed = 9104, echo = FALSE, interactive = FALSE) {
  # Set seed for reproducibility
  set.seed(seed)

  # Set knitr options
  setup_knitr_revealjs(echo = echo, interactive = interactive)

  # Define CSS for RevealJS presentations
  css <- "
  .reveal {
    font-family: 'Source Sans Pro', 'Helvetica Neue', sans-serif;
  }
  .reveal h1, .reveal h2, .reveal h3, .reveal h4, .reveal h5, .reveal h6 {
    font-family: 'Source Sans Pro', 'Helvetica Neue', sans-serif;
    font-weight: 600;
    color: #2c3e50;
    letter-spacing: -0.03em;
  }
  .reveal h1 {
    font-size: 2.5em;
    border-bottom: 2px solid #5ab4ac;
    padding-bottom: 0.2em;
  }
  .reveal h2 {
    font-size: 1.6em;
    color: #5ab4ac;
  }
  .reveal p, .reveal li {
    font-size: 1.2em;
    line-height: 1.5;
  }
  .reveal .slide-number {
    background-color: transparent;
    font-size: 0.5em;
  }
  .reveal table {
    width: 100%;
    border-collapse: collapse;
    margin: 20px 0;
    font-size: 0.8em;
  }
  .reveal th {
    background-color: #f7f7f7;
    font-weight: 600;
    color: #333;
  }
  .reveal th, .reveal td {
    padding: 8px 12px;
    border: 1px solid #e0e0e0;
    text-align: left;
  }
  .reveal tr:nth-child(even) {
    background-color: #f9f9f9;
  }
  .reveal .footer {
    position: fixed;
    bottom: 10px;
    left: 10px;
    font-size: 0.5em;
    color: #777;
  }
  .reveal .caption {
    font-style: italic;
    font-size: 0.7em;
    color: #666;
    margin-top: 5px;
    text-align: center;
  }
  .reveal pre code {
    font-size: 1.2em;
    padding: 15px;
    border-radius: 5px;
  }
  "

  # Output the CSS
  htmltools::tags$style(css)

  # Store the interactive setting as an option
  options(cq_interactive = interactive)

  # Print message to let user know the theme has been applied
  message("cqThemes RevealJS theme applied successfully!")
  message("- Use theme_cq_revealjs() for ggplot2 visualizations")
  message("- Use format_table_reveal() for formatted tables")
  message("- Use cq_colors() for consistent color palettes")
  if (interactive) {
    message("- Interactive mode enabled: static plots can be converted to plotly using ggplot_to_plotly()")
  }

  return(invisible(NULL))
}

#' Setup knitr options for RevealJS presentations
#'
#' Configures knitr chunk options optimized for RevealJS presentations
#'
#' @param echo Logical: whether to show code chunks
#' @param interactive Logical: whether to set up for interactive visualizations
#'
#' @return Invisibly returns NULL
#' @export
#'
#' @examples
#' \dontrun{
#' # In a Quarto or R Markdown presentation
#' library(cqThemes)
#' setup_knitr_revealjs()
#' }
setup_knitr_revealjs <- function(echo = FALSE, interactive = FALSE) {
  knitr::opts_chunk$set(
    echo = echo,            # Control code visibility
    warning = FALSE,        # Suppress warnings
    message = FALSE,        # Suppress messages
    fig.align = "center",   # Center figures
    out.width = "85%",      # Slightly reduced width for slides
    dpi = 144,              # Higher resolution for screens
    fig.width = 10,         # Wider default figure width
    fig.height = 6,         # Default figure height for slides
    fig.retina = 2,         # Retina display support
    fig.showtext = TRUE     # Better font rendering
  )

  # Set plot hook for converting ggplot2 to plotly if interactive mode is enabled
  if (interactive) {
    # Add a custom knit hook for plotly conversion (similar to HTML but with slide adaptations)
    knitr::knit_hooks$set(plot = function(x, options) {
      # Only convert if not explicitly disabled
      if (!isFALSE(options$plotly) && !isFALSE(options$interactive)) {
        # Get the last plot if available
        p <- get("last_plot", envir = ggplot2:::.globals)
        if (!is.null(p)) {
          # Convert to plotly with slide-specific settings
          plt <- plotly::ggplotly(p) %>%
            plotly::layout(
              autosize = TRUE,
              margin = list(l = 40, r = 40, b = 80, t = 80, pad = 4),
              font = list(family = "Source Sans Pro, Helvetica Neue, sans-serif"),
              paper_bgcolor = 'rgba(0,0,0,0)',
              plot_bgcolor = 'rgba(0,0,0,0)'
            ) %>%
            plotly::config(
              displayModeBar = FALSE,
              responsive = TRUE
            )

          # Create a div with appropriate styling
          div_id <- paste0("plot_", sample(1000:9999, 1))
          html <- htmltools::div(
            id = div_id,
            class = "plotly-slide",
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

#' Custom ggplot2 theme for RevealJS presentations
#'
#' A clean, presentation-friendly theme for ggplot2 visualizations
#' with larger text and simplified elements for better visibility
#'
#' @param base_size Base font size for the theme, defaults to 18 for presentations
#' @param base_family Base font family for the theme
#'
#' @return A ggplot2 theme object
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = mpg, y = hp)) +
#'   geom_point() +
#'   theme_cq_revealjs()
theme_cq_revealjs <- function(base_size = 18, base_family = "sans-serif") {
  # Create theme based on theme_minimal for presentation clarity
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      # Text elements - larger for presentation visibility
      plot.title = ggplot2::element_text(
        size = base_size*1.4,
        face = "bold",
        hjust = 0.5,
        margin = ggplot2::margin(b = 15)
      ),
      plot.subtitle = ggplot2::element_text(
        size = base_size*1.1,
        hjust = 0.5,
        color = "#5f6368",
        margin = ggplot2::margin(b = 20)
      ),
      axis.title = ggplot2::element_text(
        size = base_size*1.1,
        color = "#333333",
        face = "bold"
      ),
      axis.text = ggplot2::element_text(
        size = base_size*0.9,
        color = "#333333"
      ),

      # Legend elements
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = base_size*0.8),
      legend.key.size = ggplot2::unit(1.2, "lines"),

      # Grid and panel elements - simplified for better visibility in presentations
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "#dddddd", size = 0.2),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),

      # Increased margins for better framing on slides
      plot.margin = ggplot2::margin(20, 20, 20, 20),

      # Strip elements for facets
      strip.background = ggplot2::element_rect(fill = "#f0f0f0", color = NA),
      strip.text = ggplot2::element_text(
        size = base_size*0.9,
        face = "bold"
      )
    )
}
