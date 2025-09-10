#' Geographic centroid of locations
#'
#' Assigns each point a longitude and latitude corresponding to the geographic
#' centre of its administrative area. This means that all points in the same
#' area will overlap. The default \code{position} uses [position_circle_repel()]
#' to repel the points outwards with an amount controllable with its
#' \code{scale} parameter.
#'
#' @section Aesthetics:
#' The \code{location} aesthetic is required.
#' \code{geom_centroids()} understands the same aesthetics as [ggplot2::geom_point()].
#'
#' @inheritParams ggmapinset::geom_sf_inset
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,fun.geometry,... See [ggplot2::stat_sf_coordinates()].
#' @inheritParams stat_automap_coords
#'
#' @returns A ggplot layer.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' cartographer::nc_type_example_2 |>
#'   head(n = 100) |>
#'   ggplot(aes(location = county)) +
#'   geom_boundaries(feature_type = "sf.nc") +
#'   geom_centroids(aes(colour = type), position = position_circle_repel_sf(scale = 6), size = 0.5) +
#'   coord_automap(feature_type = "sf.nc")
geom_centroids <- function(mapping = ggplot2::aes(), data = NULL,
                           stat = "automap_coords",
                           position = "circle_repel_sf",
                           ...,
                           fun.geometry = NULL,
                           feature_type = NA,
                           inset = NA,
                           map_base = "clip",
                           map_inset = "auto",
                           na.rm = TRUE,
                           show.legend = "point",
                           inherit.aes = TRUE) {
  params <- rlang::list2(
    feature_type = feature_type,
    na.rm = na.rm,
    fun.geometry = fun.geometry,
    ...
  )
  if ("group" %in% names(mapping)) {
    cli::cli_warn(c(
      "Ignoring aethetic mapping for {.field group} in {.fn geom_centroids}",
      "i" = "Mapping for {.field group} was: {as.character(mapping$group)[[2]]}",
      "i" = "{.fn geom_centroids} is always grouped by the geometry column"
    ))
  }
  mapping$group <- "location"

  ggmapinset::build_sf_inset_layers(
    data = data, mapping = mapping,
    stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes, params = params,
    inset = inset, map_base = map_base, map_inset = map_inset
  )
}
