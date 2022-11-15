#' Configure transformations underpinning a map inset
#'
#' This specialises [ggmapinset::configure_inset()] to allow the \code{centre}
#' to be specified as a location.
#'
#' @param centre Coordinates of the inset centre. Can instead be the name of a
#'   geographic feature if \code{feature_type} is also provided.
#' @param scale Zoom scale: values larger than one will make the circle bigger.
#' @param translation Translate (shift) the inset. This can be an
#'  \code{st_point} or simply a vector of length 2 containing the x and y
#'  offsets respectively.
#' @param radius Radius of the inset circle.
#' @param units Base length unit (e.g. \code{"km"} or \code{"mi"}).
#'  See [ggmapinset::configure_inset()] for supported values.
#' @inheritParams resolve_feature_type
#'
#' @export
configure_inset <- function(centre = NULL, scale = NULL,
                            translation = NULL, radius = NULL,
                            units = "km",
                            feature_type = NA) {
  if (is.character(centre)) {
    feature_type <- resolve_feature_type(feature_type, centre,
                                         context = "configure_inset")
    geom <- get_geometry_loc(feature_type, centre)
    crs_working <- crs_eqc_midpoint(feature_type)
    geom <- sf::st_transform(geom, crs_working)
    centre <- sf::st_centroid(geom)
  }

  ggmapinset::configure_inset(
    centre = centre,
    scale = scale,
    translation = translation,
    radius = radius,
    units = units
  )
}
