#' Default color palette for statistical reports
#'
#' Returns a consistent color palette for visualizations
#'
#' @param n Number of colors to return
#' @param type Type of palette: "main" (default), "categorical", "sequential", or "diverging"
#' @param reverse Logical: whether to reverse the color order
#'
#' @return A character vector of hexadecimal color codes
#' @export
#'
#' @examples
#' # Get default 2-color palette
#' cq_colors()
#'
#' # Get 5 categorical colors
#' cq_colors(5, type = "categorical")
#'
#' # Get a reversed sequential palette with 3 colors
#' cq_colors(3, type = "sequential", reverse = TRUE)
cq_colors <- function(n = 2, type = "main", reverse = FALSE) {
  # Define the palette options
  palettes <- list(
    # Main palette - the two primary colors
    main = c("#5ab4ac", "#d8b365"),

    # Extended categorical palette for multiple categories
    categorical = c("#5ab4ac", "#d8b365", "#8da0cb", "#fc8d62", "#66c2a5",
                    "#e78ac3", "#a6d854", "#ffd92f", "#e5c494", "#b3b3b3"),

    # Sequential palette for ordered data
    sequential = c("#f7fcfd", "#e5f5f9", "#ccece6", "#99d8c9", "#66c2a5",
                   "#41ae76", "#238b45", "#006d2c", "#00441b"),

    # Diverging palette for data with a natural midpoint
    diverging = c("#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", "#f5f5f5",
                  "#c7eae5", "#80cdc1", "#35978f", "#01665e")
  )

  # Select the requested palette
  if (!type %in% names(palettes)) {
    stop("Invalid palette type. Choose from: ", paste(names(palettes), collapse = ", "))
  }

  pal <- palettes[[type]]

  # Ensure we have enough colors
  if (n > length(pal)) {
    warning("Requested more colors than available in palette. Interpolating additional colors.")
    # Use colorRampPalette to interpolate additional colors
    pal <- grDevices::colorRampPalette(pal)(n)
  } else {
    # Subset to the requested number of colors
    pal <- pal[1:n]
  }

  # Reverse if requested
  if (reverse) {
    pal <- rev(pal)
  }

  return(pal)
}

#' Color palette for model comparisons
#'
#' Returns a color palette specifically designed for comparing model performance
#'
#' @param n Number of colors to return
#' @param option Palette option: "plasma" (default), "viridis", "magma", or "inferno"
#'
#' @return A character vector of hexadecimal color codes
#' @export
#'
#' @examples
#' # Get colors for 4 models
#' cq_model_colors(4)
#'
#' # Get colors for 3 models using the magma palette
#' cq_model_colors(3, option = "magma")
cq_model_colors <- function(n = 4, option = "plasma") {
  # Validate option
  valid_options <- c("viridis", "plasma", "magma", "inferno")
  if (!option %in% valid_options) {
    stop("Invalid option. Choose from: ", paste(valid_options, collapse = ", "))
  }

  # Use viridis package for color palettes
  if (option == "viridis") {
    colors <- viridis::viridis(n)
  } else if (option == "plasma") {
    colors <- viridis::plasma(n)
  } else if (option == "magma") {
    colors <- viridis::magma(n)
  } else if (option == "inferno") {
    colors <- viridis::inferno(n)
  }

  return(colors)
}

#' Sequential color palette
#'
#' Returns a sequential color palette for ordered data
#'
#' @param n Number of colors to return
#' @param palette Base color for the palette: "blue" (default), "green", "purple", "orange", or "teal"
#' @param reverse Logical: whether to reverse the color order
#'
#' @return A character vector of hexadecimal color codes
#' @export
#'
#' @examples
#' # Get default sequential blue palette
#' cq_sequential_palette(5)
#'
#' # Get green sequential palette
#' cq_sequential_palette(5, palette = "green")
cq_sequential_palette <- function(n = 5, palette = "blue", reverse = FALSE) {
  # Define the palette options
  palettes <- list(
    blue = c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#4292c6", "#2171b5", "#08519c", "#08306b"),
    green = c("#f7fcf5", "#e5f5e0", "#c7e9c0", "#a1d99b", "#74c476", "#41ab5d", "#238b45", "#006d2c", "#00441b"),
    purple = c("#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3", "#54278f", "#3f007d"),
    orange = c("#fff5eb", "#fee6ce", "#fdd0a2", "#fdae6b", "#fd8d3c", "#f16913", "#d94801", "#a63603", "#7f2704"),
    teal = c("#f7fcfd", "#e5f5f9", "#ccece6", "#99d8c9", "#66c2a5", "#41ae76", "#238b45", "#006d2c", "#00441b")
  )

  # Validate palette selection
  if (!palette %in% names(palettes)) {
    stop("Invalid palette. Choose from: ", paste(names(palettes), collapse = ", "))
  }

  # Select the requested palette
  pal <- palettes[[palette]]

  # Ensure we have enough colors
  if (n > length(pal)) {
    warning("Requested more colors than available in palette. Interpolating additional colors.")
    # Use colorRampPalette to interpolate additional colors
    pal <- grDevices::colorRampPalette(pal)(n)
  } else {
    # Subset to the requested number of colors
    pal <- pal[1:n]
  }

  # Reverse if requested
  if (reverse) {
    pal <- rev(pal)
  }

  return(pal)
}

#' Diverging color palette
#'
#' Returns a diverging color palette for data with a natural midpoint
#'
#' @param n Number of colors to return
#' @param palette Type of diverging palette: "teal-brown" (default), "blue-red", "green-purple", or "blue-orange"
#' @param reverse Logical: whether to reverse the color order
#'
#' @return A character vector of hexadecimal color codes
#' @export
#'
#' @examples
#' # Get default teal-brown diverging palette
#' cq_diverging_palette(7)
#'
#' # Get blue-red diverging palette
#' cq_diverging_palette(5, palette = "blue-red")
cq_diverging_palette <- function(n = 7, palette = "teal-brown", reverse = FALSE) {
  # Define the palette options
  palettes <- list(
    "teal-brown" = c("#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", "#f5f5f5",
                     "#c7eae5", "#80cdc1", "#35978f", "#01665e"),
    "blue-red" = c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#f7f7f7",
                   "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061"),
    "green-purple" = c("#40004b", "#762a83", "#9970ab", "#c2a5cf", "#e7d4e8",
                       "#f7f7f7", "#d9f0d3", "#a6dba0", "#5aae61", "#1b7837", "#00441b"),
    "blue-orange" = c("#7f3b08", "#b35806", "#e08214", "#fdb863", "#fee0b6",
                      "#f7f7f7", "#d8daeb", "#b2abd2", "#8073ac", "#542788", "#2d004b")
  )

  # Validate palette selection
  if (!palette %in% names(palettes)) {
    stop("Invalid palette. Choose from: ", paste(names(palettes), collapse = ", "))
  }

  # Select the requested palette
  pal <- palettes[[palette]]

  # Ensure we have enough colors
  if (n > length(pal)) {
    warning("Requested more colors than available in palette. Interpolating additional colors.")
    # Use colorRampPalette to interpolate additional colors
    pal <- grDevices::colorRampPalette(pal)(n)
  } else {
    # If n is even, ensure the midpoint is represented
    if (n %% 2 == 0) {
      mid_index <- ceiling(length(pal) / 2)
      indices <- seq_len(length(pal))

      # Skip one color from either side of the midpoint
      if (length(pal) %% 2 == 1) {
        # If palette length is odd, skip the exact midpoint
        skip_index <- mid_index
        use_indices <- indices[-skip_index]
      } else {
        # If palette length is even, skip one near the midpoint
        skip_index <- mid_index - 1
        use_indices <- indices[-skip_index]
      }

      # Subset evenly from the remaining colors
      pal <- pal[round(seq(1, length(use_indices), length.out = n))]
    } else {
      # For odd n, include the midpoint and sample evenly
      pal <- pal[round(seq(1, length(pal), length.out = n))]
    }
  }

  # Reverse if requested
  if (reverse) {
    pal <- rev(pal)
  }

  return(pal)
}
