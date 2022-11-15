#' Pack overlapping points into a circle
#'
#' This position looks for any points with identical \code{x} and \code{y}
#' positions and packs them in a circle around the original point. The \code{_sf}
#' version applies the position adjustment in projected coordinates.
#'
#' Note that extreme choices of \code{scale} may cause errors.
#'
#' @param scale Scale of packing around the central point. Note that you can
#'   alternatively set this to a data column by using the \code{scale} aesthetic.
#'   This is in data units, so for the \code{_sf} variant it will depend on the
#'   units specified by the coordinate reference system.
#'
#' @export
position_circle_repel <- function(scale = 1/4) {
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
  scale = 1/4,

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
  required_aes = c("geometry"),
  scale = 10,

  compute_panel = function(self, data, params, scales) {
    tmp <- sf::st_transform(data$geometry, crs_eqc())
    tmp <- matrix(unlist(tmp), ncol = 2, byrow = TRUE)
    scale <- if (is.null(data[["scale"]])) params$scale else data$scale
    tmp <- circle_repel(data.frame(x = tmp[,1], y = tmp[,2], scale = scale))
    points <- mapply(function(x,y) st_point(c(x,y)), tmp$x, tmp$y, SIMPLIFY = FALSE)
    points <- sf::st_sfc(points, crs = crs_eqc())
    data$geometry <- sf::st_transform(points, sf::st_crs(data$geometry))
    tmp <- matrix(unlist(data$geometry), ncol = 2, byrow = TRUE)
    data$x <- tmp[,1]
    data$y <- tmp[,2]
    data
  }
)

circle_repel <- function (data) {
  tmp <- dplyr::mutate(data, row = seq_len(nrow(data)))
  tmp <- dplyr::group_by(tmp, .data$x, .data$y)

  tmp <- dplyr::mutate(tmp, shift = packcircles::circleRepelLayout(
    rep(1, dplyr::n()))$layout[,c(1,2)] * scale)
  tmp <- dplyr::ungroup(tmp)
  tmp <- dplyr::arrange(tmp, .data$row)
  tmp <- tidyr::unnest_wider(tmp, col = .data$shift, names_sep = "_")
  data$x <- data$x + tmp$shift_x
  data$y <- data$y + tmp$shift_y
  data
}
