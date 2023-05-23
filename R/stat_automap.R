#' Attach spatial data with 'cartographer'
#'
#' Use [`cartographer`][cartographer] to attach a spatial column to the data based
#' on place names in another column. The result can then be used by
#' [ggplot2::geom_sf()] or [ggmapinset::geom_sf_inset()].
#'
#' @section Computed variables:
#' \describe{
#'   \item{geometry}{\code{sf} geometry column}
#'   \item{...}{limits as computed by [ggplot2::stat_sf()]}
#' }
#'
#' @param mapping,data,geom,position,na.rm,show.legend,inherit.aes,... See [ggplot2::geom_sf()].
#' @inheritParams cartographer::resolve_feature_type
#'
#' @export
#' @returns A ggplot layer
#' @examples
#' library(ggplot2)
#'
#' events <- data.frame(
#'   county = c("Mecklenburg", "Carteret", "Moore", "Caldwell"),
#'   proportion_A = c(0.1, 0.8, 0.0, 0.6)
#' )
#'
#' ggplot(events, aes(location = county)) +
#'   geom_sf(aes(fill = proportion_A), stat = "automap")
#'
#' ggplot(events, aes(location = county)) +
#'   stat_automap(aes(fill = proportion_A)) +
#'   coord_automap(feature_type = "sf.nc")
stat_automap <- function(mapping = NULL, data = NULL,
                         geom = "sf", position = "identity",
                         ...,
                         feature_type = NA,
                         na.rm = TRUE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  ggplot2::layer_sf(
    data = data,
    mapping = mapping,
    stat = StatAutomap,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      feature_type = feature_type,
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
StatAutomap <- ggplot2::ggproto("StatAutomap", ggmapinset::StatSfInset,
  required_aes = c("location"),

  compute_panel = function(data, scales, coord, feature_type = NA) {
    feature_type <- get_feature_type(feature_type, coord, data$location)
    data$location <- cartographer::resolve_feature_names(data$location, feature_type)

    geoms <- cartographer::map_sfc(data$location, feature_type)
    crs_data <- sf::st_crs(cartographer::map_sf(feature_type))
    data$geometry <- sf::st_sfc(geoms, crs = crs_data)

    ggmapinset::StatSfInset$compute_panel(sf::st_as_sf(data), scales, coord)
  }
)
