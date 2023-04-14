#' Attach spatial data with cartographer and reduce to coordinates
#'
#' Use [`cartographer`][cartographer] to attach a spatial column to the data based
#' on place names in another column. The spatial data is then reduced to
#' coordinates in the same way as [`stat_sf_coordinates()`][ggplot2::stat_sf_coordinates].
#' The result can then be used by [`geom_sf()`][ggplot2::geom_sf] or
#' [`geom_sf_inset()`][ggmapinset::geom_sf_inset] or any geom that needs `x` and
#' `y` aesthetics.
#'
#' @section Computed variables:
#' \describe{
#'   \item{x}{X dimension of the simple feature}
#'   \item{y}{Y dimension of the simple feature}
#' }
#'
#' @param mapping,data,geom,position,na.rm,show.legend,inherit.aes,... See [ggplot2::stat_sf_coordinates()].
#' @inheritParams cartographer::resolve_feature_type
#' @inheritParams ggplot2::stat_sf_coordinates
#'
#' @export
#' @returns A plot layer
#' @examples
#' library(ggplot2)
#'
#' events <- data.frame(
#'   county = c("Mecklenburg", "Carteret", "Moore", "Caldwell"),
#'   proportion_A = c(0.1, 0.8, 0.0, 0.6))
#'
#' ggplot(events, aes(location = county)) +
#'   geom_sf(aes(fill = proportion_A), stat = "automap") +
#'   geom_label(aes(label = county), stat = "automap_coords")
stat_automap_coords <- function(mapping = NULL, data = NULL,
                                geom = "sf", position = "identity",
                                ...,
                                feature_type = NA,
                                na.rm = TRUE,
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
      na.rm = na.rm,
      fun.geometry = fun.geometry,
      ...
    )
  )
}

#' @rdname stat_automap
#' @usage NULL
#' @format NULL
#'
#' @export
StatAutomapCoords <- ggplot2::ggproto("StatAutomapCoords", ggplot2::StatSfCoordinates,
  required_aes = c("location"),

  setup_data = function(data, params) {
    data <- ggplot2::StatSfCoordinates$setup_data(data, params)
    data$location <- cartographer::resolve_feature_names(data$location,
                                                         params$feature_type)
    data
  },

  setup_params = function(data, params) {
    params <- ggplot2::StatSfCoordinates$setup_params(data, params)
    if (is.null(params[["feature_type"]])) params$feature_type <- NA
    params$feature_type <- cartographer::resolve_feature_type(params$feature_type,
                                                              data$location)
    params
  },

  compute_group = function(data, scales, coord, feature_type, ...) {
    geoms <- cartographer::map_sfc(data$location, feature_type)
    crs_data <- sf::st_crs(cartographer::map_sf(feature_type))
    data$geometry <- sf::st_sfc(geoms, crs = crs_data)

    ggplot2::StatSfCoordinates$compute_group(sf::st_as_sf(data), scales, coord, ...)
  }
)
