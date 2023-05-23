#' Pack overlapping points into a circle
#'
#' This position looks for any points with identical \code{x} and \code{y}
#' positions and packs them in a circle around the original point. The \code{_sf}
#' version applies the position adjustment in projected coordinates.
#'
#' Note that extreme choices of \code{scale} may cause errors.
#'
#' The \code{scale} parameter can instead be specified as an aesthetic for geoms
#' that support it ([geom_centroids()]). This allows different locations to have
#' different scales, which is especially useful when combined with map insets.
#'
#' @param scale Scale of packing around the central point.
#'   This is in data units, so for the \code{_sf} variant it will depend on the
#'   units specified by the coordinate reference system.
#'
#' @returns A ggplot position object.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' points <- data.frame(
#'   x = c(rep(1, 10), 1:3),
#'   y = c(rep(2, 10), 3:5),
#'   s = 0.05
#' )
#' ggplot(points, aes(x, y)) +
#'   geom_point(size = 3, colour = "red") +
#'   geom_point(position = position_circle_repel(0.05), size = 3, alpha = 0.5)
#'
#' cartographer::nc_type_example_2 |>
#'   dplyr::filter(!county %in% c("HENDERSON", "GASTON", "LINCOLN")) |>
#'   ggplot(aes(location = county)) +
#'   geom_boundaries(feature_type = "sf.nc") +
#'   geom_centroids(aes(colour = type), position = position_circle_repel_sf(scale = 4), size = 0.2) +
#'   coord_automap(feature_type = "sf.nc")
#
# FIXME: points <- data.frame(county = counties, s = ifelse(counties == nc$NAME[[1]], 5, 10))
position_circle_repel <- function(scale = 1 / 4) {
  ggplot2::ggproto(NULL, PositionCircleRepel, scale = scale)
}

#' @rdname position_circle_repel
#'
#' @export
position_circle_repel_sf <- function(scale = 10) {
  ggplot2::ggproto(NULL, PositionCircleRepelSf, scale = scale)
}

#' @rdname position_circle_repel
#' @usage NULL
#' @format NULL
#'
#' @importFrom  packcircles circleRepelLayout
#' @importFrom tidyr unnest_wider
#' @export
PositionCircleRepel <- ggplot2::ggproto("PositionCircleRepel", ggplot2::Position,
  required_aes = c("x", "y"),
  scale = 1 / 4,

  setup_params = function(self, data) {
    list(scale = self$scale)
  },

  compute_panel = function(self, data, params, scales) {
    scale <- if (is.null(data[["scale"]])) params$scale else data$scale
    circle_repel(data.frame(x = data$x, y = data$y, scale = scale))
  }
)

#' @rdname position_circle_repel
#' @usage NULL
#' @format NULL
#'
#' @export
PositionCircleRepelSf <- ggplot2::ggproto("PositionCircleRepelSf", PositionCircleRepel,
  required_aes = c("geometry", "x", "y"),
  scale = 10,

  compute_panel = function(self, data, params, scales) {
    # if the points are in the inset, we rescale them to compensate for the
    # transformation
    scale <- params$scale
    if ("inset_scale" %in% colnames(data)) {
      scale <- scale / data$inset_scale
    }

    crs_data <- sf::st_crs(data$geometry)
    crs_working <- crs_eqc()

    geometry <- coords_to_points(data$x, data$y, crs = crs_data)
    geometry <- sf::st_transform(geometry, crs_working)
    coordinates <- sf::st_coordinates(geometry)

    tmp <- circle_repel(data.frame(
      x = coordinates[, "X"], y = coordinates[, "Y"],
      scale = scale
    ))

    placed_pts <- coords_to_points(tmp$x, tmp$y, crs = crs_working)
    placed_pts <- sf::st_transform(placed_pts, crs_data)
    data$geometry <- placed_pts
    coordinates <- sf::st_coordinates(placed_pts)

    data$x <- coordinates[, "X"]
    data$y <- coordinates[, "Y"]
    data
  }
)

coords_to_points <- function(x, y, crs) {
  points <- sf::st_multipoint(matrix(c(x, y), ncol = 2))
  sf::st_cast(sf::st_sfc(points, crs = crs), "POINT")
}

circle_repel <- function(data) {
  tmp <- dplyr::mutate(data, row = seq_len(nrow(data))) |>
    dplyr::group_by(.data$x, .data$y) |>
    dplyr::mutate(shift = packcircles::circleRepelLayout(
      rep(1, dplyr::n())
    )$layout[, c(1, 2)] * scale) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$row) |>
    tidyr::unnest_wider(col = .data$shift, names_sep = "_")
  data$x <- data$x + tmp$shift_x
  data$y <- data$y + tmp$shift_y
  data
}
