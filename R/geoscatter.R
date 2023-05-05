#' Place points randomly or in a grid within locations
#'
#' Each row of data is drawn as a single point inside the geographic area. This
#' has similar strengths to a standard scatter plot, but has the potential to
#' be misleading by implying that there is significance to the exact placement
#' of the points.
#'
#' @rdname geoscatter
#'
#' @section Aesthetics:
#' The \code{location} aesthetic is required.
#' \code{geom_geoscatter()} understands the same aesthetics as [ggplot2::geom_point()].
#'
#' @param sample_type sampling type (see the \code{type} argument of [sf::st_sample()]).
#'   \code{"random"} will place points randomly inside the boundaries, whereas
#'   \code{"regular"} and \code{"hexagonal"} will evenly space points, leaving
#'   a small margin close to the boundaries.
#' @inheritParams ggmapinset::geom_sf_inset
#' @inheritParams stat_geoscatter
#'
#' @returns A ggplot layer.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' cartographer::nc_type_example_2 |>
#'   ggplot(aes(location = county)) +
#'   geom_boundaries(feature_type = "sf.nc") +
#'   geom_geoscatter(aes(colour = type), size = 0.5) +
#'   coord_automap(feature_type = "sf.nc")
geom_geoscatter <- function(mapping = ggplot2::aes(), data = NULL,
                            stat = "geoscatter", position = "identity",
                            ...,
                            feature_type = NA,
                            sample_type = "random",
                            inset = NA,
                            map_base = "clip",
                            map_inset = "auto",
                            na.rm = TRUE,
                            show.legend = "point",
                            inherit.aes = TRUE) {
  sample_type <- rlang::arg_match0(sample_type, c("random", "regular", "hexagonal"))

  params <- rlang::list2(
    feature_type = feature_type,
    sample_type = sample_type,
    na.rm = na.rm,
    ...
  )

  ggmapinset::build_sf_inset_layers(
    data = data, mapping = mapping,
    stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes, params = params,
    inset = inset, map_base = map_base, map_inset = map_inset
  )
}

#' @rdname geoscatter
#'
#' @section Computed variables:
#' \describe{
#'   \item{x}{longitude}
#'   \item{y}{latitude}
#' }
#'
#' @param sample_type sampling type (see the \code{type} argument of [sf::st_sample()]).
#'   \code{"random"} will place points randomly inside the boundaries, whereas
#'   \code{"regular"} and \code{"hexagonal"} will evenly space points, leaving
#'   a small margin close to the boundaries.
#' @param mapping,data,stat,geom,position,na.rm,show.legend,inherit.aes,... See [ggplot2::geom_sf()].
#' @inheritParams cartographer::resolve_feature_type
#'
#' @export
stat_geoscatter <- function(mapping = NULL, data = NULL,
                            geom = "sf_inset", position = "identity",
                            ...,
                            feature_type = NA,
                            sample_type = "random",
                            show.legend = NA,
                            inherit.aes = TRUE) {
  sample_type <- rlang::arg_match0(sample_type, c("random", "regular", "hexagonal"))

  ggplot2::layer_sf(
    data = data,
    mapping = mapping,
    stat = StatGeoscatter,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      feature_type = feature_type,
      sample_type = sample_type,
      ...
    )
  )
}

#' @rdname geoscatter
#' @usage NULL
#' @format NULL
#'
#' @importFrom rlang .data
#' @export
StatGeoscatter <- ggplot2::ggproto("StatGeoscatter", ggmapinset::StatSfInset,
  required_aes = c("location"),

  compute_panel = function(data, scales, coord, feature_type = NA, sample_type = "random") {
    feature_type <- get_feature_type(feature_type, coord, data$location)
    data$location <- cartographer::resolve_feature_names(data$location, feature_type)

    data$ggautomap__row <- seq_len(nrow(data))

    coords <- dplyr::group_modify(dplyr::group_by(data, .data$location), function(dat, grp) {
      geom <- cartographer::map_sfc(grp$location[[1]], feature_type)

      # work in a CRS that isn't distorted near the middle of the map
      crs_orig <- sf::st_crs(geom)
      crs_working <- crs_eqc_midpoint(feature_type)
      geom <- sf::st_transform(geom, crs = crs_working)

      # shrink the geom a little so we don't get points near boundaries
      bbox <- sf::st_bbox(geom)
      geom <- sf::st_buffer(geom, dist = -0.05 * min(bbox[[3]] - bbox[[1]], bbox[[4]] - bbox[[2]]))

      size <- nrow(dat)
      if (sample_type != "random") {
        size <- as.integer(size * 1.3)
      }

      # FIXME ... additional arguments passed to [sf::st_sample()].
      points <- sf::st_sample(geom, size = size, type = sample_type, exact = TRUE)
      points <- sf::st_transform(points, crs = crs_orig)
      # FIXME: it's still possible for non-random modes to generate too few points

      dat$geometry <- points
      dat
    })
    coords <- dplyr::ungroup(coords)
    coords <- dplyr::arrange(coords, .data$ggautomap__row)
    coords <- coords[, setdiff(names(coords), "ggautomap__row")]
    coords
  }
)
