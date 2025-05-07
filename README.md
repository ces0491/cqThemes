# cqThemes: Consistent Styling for Statistical Reports

`cqThemes` is an R package that provides consistent styling for statistical reports and presentations created with Quarto or R Markdown. It includes themes for PDF reports, HTML documents, and RevealJS presentations, all with coordinated color palettes, typography, and spacing.

## Features

- **Three output formats**:
  - PDF Reports - Professional styling for academic and business reports
  - HTML Documents - Interactive web-based documents with responsive design
  - RevealJS Presentations - Slide decks with consistent branding

- **Coordinated styling**:
  - Consistent plot themes for ggplot2
  - Interactive plots with plotly integration for HTML output
  - Well-formatted tables with kableExtra (PDF) and DT (HTML)
  - Carefully selected color palettes for different data types

- **Easy to use**:
  - Simple function calls to apply themes
  - Document templates for quick start
  - Comprehensive documentation and examples

## Installation

You can install the package from GitHub:

```r
# Install devtools if you don't have it
# install.packages("devtools")

# Install cqThemes from GitHub
devtools::install_github("username/cqThemes")
```

## Basic Usage

### PDF Reports

```r
library(cqThemes)
use_pdf_theme()

# Create a plot with the PDF theme
ggplot(mtcars, aes(x = mpg, y = hp)) +
  geom_point() +
  theme_cq_pdf()
```

### HTML Documents

```r
library(cqThemes)
use_html_theme(interactive = TRUE)

# Create a plot with the HTML theme
# (will be automatically converted to interactive plotly)
ggplot(mtcars, aes(x = mpg, y = hp)) +
  geom_point() +
  theme_cq_html()
```

### RevealJS Presentations

```r
library(cqThemes)
use_revealjs_theme()

# Create a plot with the RevealJS theme
ggplot(mtcars, aes(x = mpg, y = hp)) +
  geom_point() +
  theme_cq_revealjs()
```

## Document Templates

The package includes document templates for all three output formats. You can access these templates from RStudio:

1. File → New File → R Markdown... → From Template
2. Select one of the cqThemes templates:
   - cqThemes PDF Report
   - cqThemes HTML Document
   - cqThemes RevealJS Presentation

## Documentation

For more information, see the package vignette:

```r
vignette("cqThemes")
```

## License

This package is licensed under the MIT License.
