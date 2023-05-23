#' Coordinate reference system for spatial computations
#'
#' \code{crs_eqc()} gives a CRS that can be used for e.g. computing centroids or
#' distances.
#' It is an equidistant cylindrical system that by does not distort latitudes
#' near \code{latitude}. The CRS is in units of kilometres by default.
#'
#' @param latitude The latitude of true scale (the \code{proj} parameter
#'   \code{lat_ts}). This is the latitude where the scale is not distorted by
#'   the projection.
#' @param units Base length unit (e.g. \code{"km"} or \code{"mi"}).
#'   See [ggmapinset::configure_inset()] for supported values.
#'
#' @returns CRS object from \code{sf}.
#' @export
#' @examples
#' # Sydney, Australia has a latitude of 33.87 S so this CRS will be suitable
#' # for computations close to there:
#' crs_eqc(latitude = -33.87)
crs_eqc <- function(latitude = 0, units = "km") {
  sf::st_crs(paste0("+proj=eqc", " +lat_ts=", latitude, " +units=", units))
}

crs_eqc_midpoint <- function(feature_type) {
  bbox <- sf::st_bbox(cartographer::map_sf(feature_type))
  crs_eqc(latitude = mean(bbox[[2]], bbox[[4]]))
}
