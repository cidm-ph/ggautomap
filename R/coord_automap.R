#' Specify an inset configuration for the whole plot
#'
#' This allows a default inset configuration to be provided to avoid having to
#' repeat it for each layer. Any layer that is inset-aware can use this as the
#' default configuration if none is specifically provided to that layer. This
#' coord also expands the axis limits to include the inset area.
#'
#' @inheritParams cartographer::resolve_feature_type
#' @param inset Inset configuration; see [configure_inset()].
#' @param ... Arguments passed to [ggmapinset::coord_sf_inset()]
#'
#' @returns A ggplot coordinate
#' @export
#' @examples
#' library(ggplot2)
#' library(cartographer)
#'
#' ggplot(nc_type_example_2, aes(location = county)) +
#'   geom_choropleth(aes(colour = type), size = 0.5) +
#'   geom_sf_label_inset(aes(label = county), stat = "automap_coords", size = 3) +
#'   coord_automap(feature_type = "sf.nc")
coord_automap <- function(feature_type = NA, inset = NULL, ...) {
  parent <- ggmapinset::coord_sf_inset(inset, ...)
  ggproto("CoordAutomap", parent,
    feature_type = feature_type
  )
}

get_feature_type <- function(feature_type, coord, feature_names) {
  if (is.null(feature_type)) feature_type <- NA_character_
  if (is.na(feature_type) && inherits(coord, "CoordAutomap")) {
    feature_type <- coord$feature_type
  }
  if (is.na(feature_type)) {
    rlang::warn("Guessing `feature_type`; provide `feature_type` to coord_automap() to suppress")
  }
  cartographer::resolve_feature_type(feature_type, feature_names)
}
