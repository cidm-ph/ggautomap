#' Attach coordinates with 'cartographer'
#'
#' Use 'cartographer' to attach a spatial column to the data based
#' on place names in another column. The spatial data is then reduced to
#' coordinates in the same way as [`stat_sf_coordinates()`][ggplot2::stat_sf_coordinates].
#'
#' @section Computed variables:
#' \describe{
#'   \item{geometry}{\code{sf} geometry column representing the points}
#'   \item{x}{X dimension of the simple feature}
#'   \item{y}{Y dimension of the simple feature}
#'   \item{x_inset}{X dimension of the simple feature after inset transformation}
#'   \item{y_inset}{Y dimension of the simple feature after inset transformation}
#'   \item{inside_inset}{logical indicating points inside the inset viewport}
#'   \item{inset_scale}{1 for points outside the inset, otherwise the configured inset scale parameter}
#' }
#'
#' @param mapping,data,geom,position,na.rm,show.legend,inherit.aes,fun.geometry,... See [ggplot2::stat_sf_coordinates()].
#' @inheritParams cartographer::resolve_feature_type
#' @inheritParams ggmapinset::stat_sf_coordinates_inset
#' @seealso [ggmapinset::stat_sf_coordinates_inset()]
#'
#' @export
#' @returns A plot layer
#' @examples
#' library(ggplot2)
#'
#' events <- data.frame(
#'   county = c("Mecklenburg", "Carteret", "Moore", "Caldwell"),
#'   proportion_A = c(0.1, 0.8, 0.0, 0.6)
#' )
#'
#' ggplot(events, aes(location = county)) +
#'   geom_sf(aes(fill = proportion_A), stat = "automap") +
#'   geom_label(aes(label = county), stat = "automap_coords") +
#'   coord_automap(feature_type = "sf.nc")
stat_automap_coords <- function(mapping = NULL, data = NULL,
                                geom = "sf_inset", position = "identity",
                                ...,
                                feature_type = NA,
                                na.rm = TRUE,
                                inset = NA,
                                fun.geometry = NULL,
                                show.legend = NA,
                                inherit.aes = TRUE) {
  ggplot2::layer_sf(
    data = data,
    mapping = mapping,
    stat = StatAutomapCoords,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      feature_type = feature_type,
      inset = inset,
      fun.geometry = fun.geometry,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname stat_automap
#' @usage NULL
#' @format NULL
#'
#' @export
StatAutomapCoords <- ggplot2::ggproto("StatAutomapCoords", ggmapinset::StatSfCoordinatesInset,
  required_aes = c("location"),

  compute_group = function(data, scales, coord, feature_type = NA, inset = NA, fun.geometry = NULL) {
    feature_type <- get_feature_type(feature_type, coord, data$location)
    data$location <- cartographer::resolve_feature_names(data$location, feature_type)
    data$geometry <- cartographer::map_sfc(data$location, feature_type)

    crs_working <- sf::NA_crs_
    if (sf::st_is_longlat(data$geometry)) {
      crs_working <- crs_eqc_midpoint(feature_type)
    }

    data <- ggmapinset::StatSfCoordinatesInset$compute_group(data, scales, coord,
      fun.geometry = fun.geometry,
      inset = inset,
      crs_working = crs_working
    )
    # data$geometry <- coords_to_points(data$x, data$y, crs = sf::st_crs(data$geometry))
    data
  }
)
