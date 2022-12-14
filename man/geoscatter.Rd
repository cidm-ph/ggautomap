% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geoscatter.R
\docType{data}
\name{geom_geoscatter}
\alias{geom_geoscatter}
\alias{stat_geoscatter}
\alias{StatGeoscatter}
\title{Place points randomly or in a grid within locations}
\usage{
geom_geoscatter(
  mapping = ggplot2::aes(),
  data = NULL,
  stat = "geoscatter",
  position = "identity",
  ...,
  feature_type = NA,
  sample_type = c("random", "regular", "hexagonal"),
  inset = NULL,
  inset_copy = TRUE,
  inset_clip = TRUE,
  na.rm = TRUE,
  show.legend = "point",
  inherit.aes = TRUE
)

stat_geoscatter(
  mapping = NULL,
  data = NULL,
  geom = "sf",
  position = "identity",
  ...,
  feature_type = NA,
  sample_type = c("random", "regular", "hexagonal"),
  show.legend = NA,
  inherit.aes = TRUE
)
}
\arguments{
\item{mapping, data, stat, geom, position, na.rm, show.legend, inherit.aes, ...}{See \code{\link[ggplot2:ggsf]{ggplot2::geom_sf()}}.}

\item{feature_type}{Type of map feature. See \code{\link[=feature_types]{feature_types()}} for a list of
registered types. If \code{NA}, the type is guessed based on the values in
the data.}

\item{sample_type}{sampling type (see the \code{type} argument of \code{\link[sf:st_sample]{sf::st_sample()}}).
\code{"random"} will place points randomly inside the boundaries, whereas
\code{"regular"} and \code{"hexagonal"} will evenly space points, leaving
a small margin close to the boundaries.}

\item{inset}{Inset configuration; see \code{\link[ggmapinset:configure_inset]{configure_inset()}}.}

\item{inset_copy}{Draw both the base layers and the inset layers using the same
configuration. Only relevant when \code{inset} is specified.}

\item{inset_clip}{When an inset is drawn, place included points only in the inset.}
}
\description{
Each row of data is drawn as a single point inside the geographic area. This
has similar strengths to a standard scatter plot, but has the potential to
be misleading by implying that there is significance to the exact placement
of the points.
}
\section{Aesthetics}{

The \code{location} aesthetic is required.
\code{geom_geoscatter()} understands the same aesthetics as \code{\link[ggplot2:geom_point]{ggplot2::geom_point()}}.
}

\section{Computed variables}{

\describe{
\item{x}{longitude}
\item{y}{latitude}
}
}

\examples{
library(ggplot2)

ggplot(nc_type_example, aes(location = location)) +
geom_boundaries(feature_type = "sf.nc") +
geom_geoscatter(aes(colour = type), size = 0.5)

}
\keyword{datasets}
