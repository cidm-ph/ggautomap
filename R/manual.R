#' Convert input data frame into a spatial data frame
#'
#' @param x Data frame with a location column.
#' @param location Name of the location column (tidy evaluation).
#' @param feature_type Feature type of the location column. If NA (the default),
#'   the type is guessed from the values in the location column.
#' @param geom_name Name for the new column to contain the geometry.
#'
#' @returns A spatial data frame containing all of the columns from the input
#'   data frame.
#'
#' @importFrom rlang :=
#' @export
#'
#' @examples
#' data(nc_type_example)
#'
#' into_sf(nc_type_example, location)
into_sf <- function(x, location, feature_type = NA, geom_name = "geometry") {
  location_data <- dplyr::pull(x, {{ location }})
  feature_type <- resolve_feature_type(feature_type, location_data,
                                       context = "{.fn into_sf}")
  location_data <- resolve_feature_names(location_data, feature_type)

  matches <- match(location_data, get_feature_names(feature_type))
  geometry <- sf::st_geometry(get_geometry(feature_type))
  x <- dplyr::mutate(x, {{ geom_name }} := geometry[matches])
  sf::st_sf(x, sf_column_name = geom_name)
}
