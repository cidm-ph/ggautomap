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
#' @seealso [geom_sf_inset()]
#' @export
coord_automap <- function(feature_type = NA, inset = NULL, ...) {
  parent <- ggmapinset::coord_sf_inset(inset, ...)
  ggproto("CoordAutomap", parent,
    feature_type = feature_type
  )
}
