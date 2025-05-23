% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colors.R
\name{cq_diverging_palette}
\alias{cq_diverging_palette}
\title{Diverging color palette}
\usage{
cq_diverging_palette(n = 7, palette = "teal-brown", reverse = FALSE)
}
\arguments{
\item{n}{Number of colors to return}

\item{palette}{Type of diverging palette: "teal-brown" (default), "blue-red", "green-purple", or "blue-orange"}

\item{reverse}{Logical: whether to reverse the color order}
}
\value{
A character vector of hexadecimal color codes
}
\description{
Returns a diverging color palette for data with a natural midpoint
}
\examples{
# Get default teal-brown diverging palette
cq_diverging_palette(7)

# Get blue-red diverging palette
cq_diverging_palette(5, palette = "blue-red")
}
