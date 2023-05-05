#' Configure transformations underpinning a map inset
#'
#' This specialises [ggmapinset::configure_inset()] to allow the \code{centre}
#' to be specified as a location. The centroid of that location is used as the
#' inset's centre.
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
#' @inheritParams cartographer::resolve_feature_type
#'
#' @returns An inset configuration object.
#' @seealso ggmapinset::configure_inset
#' @export
#'
#' @examples
#' cfg <- configure_inset(
#'   centre = "Yancey",
#'   feature_type = "sf.nc",
#'   scale = 2,
#'   translation = c(70, -180),
#'   radius = 50,
#'   units = "mi"
#' )
configure_inset <- function(centre = NULL, scale = NULL,
                            translation = NULL, radius = NULL,
                            units = "km",
                            feature_type = NA) {
  if (is.character(centre)) {
    # FIXME: this is still guessing when it should come from the coord where possible
    feature_type <- cartographer::resolve_feature_type(feature_type, centre)
    geom <- cartographer::map_sfc(centre, feature_type)
    crs_orig <- sf::st_crs(geom)

    crs_working <- crs_eqc_midpoint(feature_type)
    geom <- sf::st_transform(geom, crs_working)
    centre <- sf::st_transform(sf::st_centroid(geom), crs_orig)
  }

  ggmapinset::configure_inset(
    centre = centre,
    scale = scale,
    translation = translation,
    radius = radius,
    units = units
  )
}
