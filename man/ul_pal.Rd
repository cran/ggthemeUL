% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ul_pal.R
\name{ul_pal}
\alias{ul_pal}
\title{University of Ljubljana color palettes}
\usage{
ul_pal(palette = "primary", neutralColor = "white", reverse = FALSE, ...)
}
\arguments{
\item{palette}{Character. The name of the palette to use.

\itemize{
  \item Primary: \code{"primary"} (default)
  \item Secondary (cold): \code{"secondaryCold"}
  \item Secondary (warm): \code{"secondaryWarm"}
  \item Individual colors: \code{"red"}, \code{"antracit"}, \code{"medium"}, \code{"lajt"}, \code{"darkblue"}, \code{"navyblue"}, \code{"turquoise"}, \code{"green"}, \code{"yellow"}, \code{"orange"}, \code{"burgundy"}, \code{"pink"}
  \item Divergent: \code{"redBlue"}, \code{"redGreen"}, \code{"blueTurquise"}, \code{"blueYellow"}
}}

\item{neutralColor}{Define the color used as neutral (default \code{"white"}).}

\item{reverse}{Logical. If \code{TRUE}, reverses the order of colors in the palette. Default is \code{FALSE}.}

\item{...}{Additional arguments to be passed to the \code{colorRampPalette} function.}
}
\value{
A function that takes an integer argument n and returns a character vector of n colors.
}
\description{
This function provides color palettes adhering to the official visual identity
of the University of Ljubljana. It includes the primary and secondary palettes,
individual color gradations, and multiple divergent color palettes.
}
\examples{
pal <- ul_pal("redBlue", reverse = TRUE)
pal(7)  # Generates a reversed divergent palette of 7 colors
}
