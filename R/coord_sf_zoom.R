#' Zoom a map to show only certain features
#'
#' This is a wrapper around [ggplot2::coord_sf()] that automatically calculates
#' coordinate limits based on the data and/or any additional locations. The
#' bounding box will be calculated to encompass all of the \code{include}d
#' locations.
#'
#' This should be added to the plot _after_ the call to one of the ggautomap
#' geoms. It will copy the data and \code{feature_type} from the first such
#' layer in the plot. If there is no such layer, it will attempt to guess the
#' feature type and use the data and \code{location} mapping found at the top
#' level \code{ggplot()} call.
#'
#' @param include Vector of feature names that should be shown on the map.
#' @param include_data Scalar logical, if true then all features with data are
#'   also included.
#' @param ... Additional arguments passed to [ggplot2::coord_sf()].
#'
#' @returns A zoom specification that can be added to a ggplot object with [ggplot2::%+%].
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # zoom in on locations that have data:
#' cartographer::nc_type_example[1:49,] |>
#'   ggplot(aes(location = county)) +
#'   geom_boundaries(feature_type = "sf.nc") +
#'   geom_centroids() +
#'   coord_sf_zoom()
#'
#' # or just zoom in on specific locations regardless of the data:
#' cartographer::nc_type_example[1:49,] |>
#'   ggplot(aes(location = county)) +
#'   geom_boundaries(feature_type = "sf.nc") +
#'   coord_sf_zoom(include = c("Rowan", "Polk"), include_data = FALSE)
coord_sf_zoom <- function(include = NULL, include_data = TRUE, ...) {
  structure(
    list(include = include, include_data = include_data,
         coord_sf_args = rlang::list2(...)),
    class = "ggautomap_zoom_spec"
  )
}

#' @export
ggplot_add.ggautomap_zoom_spec <- function(spec, plot, object_name) {
  feature_type <- NA_character_
  data_location <- NA

  # find the layer with the relevant data and params
  for (layer in plot$layers) {
    if (is_ggautomap_stat(layer$stat)) {
      feature_type <- layer$stat_params$feature_type
      mapping <- layer$mapping
      if (!("location" %in% names(mapping))) {
        mapping <- plot$mapping
      }
      if (!("location" %in% names(mapping))) {
        cli::cli_warn("unable to find {.val location} aesthetic in layer")
      }
      data <- layer$layer_data(plot$data)
      data_location <- dplyr::pull(data, !!mapping$location)
      break
    }
  }

  if (any(is.na(data_location))) {
    if (inherits(plot$data, "data.frame") & ("location" %in% names(plot$mapping))) {
      data_location <- dplyr::pull(plot$data, !!plot$mapping$location)
    }
  }

  if (any(is.na(data_location))) {
    cli::cli_abort(c("{.fn coord_sf_zoom} unable to find plot data",
                     "i" = "add {.emph after} a {.pkg ggautomap} layer like {.fn geom_geoscatter} or {.fn geom_centroids}",
                     "i" = "alternatively, define the {.arg data} and the {.field location} aesthetic in the top level {.fn ggplot} call"))
  }

  feature_type <- cartographer::resolve_feature_type(feature_type, data_location)

  include <- cartographer::resolve_feature_names(spec$include, feature_type)
  if (spec$include_data) {
    include <- unique(c(include, data_location))
  }

  geoms <- cartographer::map_sf(feature_type)
  geom_locations <- cartographer::feature_names(feature_type)
  bbox <- sf::st_bbox(geoms[geom_locations %in% include,])

  args <- spec$coord_sf_args
  args$xlim <- c(bbox[[1]], bbox[[3]])
  args$ylim <- c(bbox[[2]], bbox[[4]])

  plot + do.call(ggplot2::coord_sf, args)
}

is_ggautomap_stat <- function(stat) {
  # these need to have a location aesthetic and a feature_type param
  inherits(stat, "StatGeoscatter") #|| inherits(stat, "StatCentroids") || inherits(stat, "StatChoropleth")
}
