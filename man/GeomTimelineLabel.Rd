% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\docType{data}
\name{GeomTimelineLabel}
\alias{GeomTimelineLabel}
\title{Geom class to plot earthquakes timeline.}
\format{
An object of class \code{GeomTimelineLabel} (inherits from \code{Geom}, \code{ggproto}, \code{gg}) of length 5.
}
\usage{
GeomTimelineLabel
}
\description{
This object must receive an array of date class, and optionally could
receive parameters to y (some factor variable, it was proposed earthquake
country), colour, size and alpha.
}
\section{Aesthetics (x and label are required) }{

\itemize{
\item{\code{x}} : Array of date class
\item{\code{label}} : Array of char class. Ideally contains the earthquakes
location treated with \code{eq_clean_data} function.
\item{\code{n_max}} : Value to the maximum number of earthquakes location printed in
plot. It must be passed an array to size parameter in GeomTimeline to work.
}
}

\keyword{datasets}
