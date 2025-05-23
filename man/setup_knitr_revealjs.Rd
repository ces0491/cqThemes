% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/themes_revealjs.R
\name{setup_knitr_revealjs}
\alias{setup_knitr_revealjs}
\title{Setup knitr options for RevealJS presentations}
\usage{
setup_knitr_revealjs(echo = FALSE, interactive = FALSE)
}
\arguments{
\item{echo}{Logical: whether to show code chunks}

\item{interactive}{Logical: whether to set up for interactive visualizations}
}
\value{
Invisibly returns NULL
}
\description{
Configures knitr chunk options optimized for RevealJS presentations
}
\examples{
\dontrun{
# In a Quarto or R Markdown presentation
library(cqThemes)
setup_knitr_revealjs()
}
}
