#' Geographic centroid of administrative areas
#'
#' Assigns each point a longitude and latitude corresponding to the geographic
#' centre of its administrative area. This means that all points in the same
#' area will overlap. The default \code{position} uses [position_circle_repel()]
#' to repel the points outwards with an amount controllable with its
#' \code{density} parameter.
#'
#' @rdname centroids
#'
#' @section Aesthetics:
#' The \code{location} aesthetic is required.
#' \code{geom_centroids()} understands the same aesthetics as [ggplot2::geom_point()].
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(nc_type_example, aes(location = location)) +
#' geom_boundaries(feature_type = "sf.nc") +
#' geom_centroids(aes(colour = type, scale = 6), size = 0.5)
#'
#' @inheritParams ggmapinset::geom_sf_inset
#' @inheritParams stat_centroids
#'
#' @export
geom_centroids <- function(mapping = ggplot2::aes(), data = NULL,
                           stat = "centroids", position = "circle_repel_sf",
                           ...,
                           feature_type = NA,
                           inset = NULL,
                           inset_copy = TRUE,
                           inset_clip = TRUE,
                           na.rm = TRUE,
                           show.legend = "point",
                           inherit.aes = TRUE) {
  params <- rlang::list2(
    feature_type = feature_type,
    na.rm = na.rm,
    ...
  )

  ggmapinset::build_sf_inset_layers(
    data = data, mapping = mapping,
    stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes, params = params,
    inset = inset, inset_copy = inset_copy, inset_clip = inset_clip
  )
}

#' @rdname centroids
#'
#' @section Computed variables:
#' \describe{
#'   \item{cendroid_longitude}{longitude of the administrative region's centroid}
#'   \item{centroid_latitude}{latitude of the administrative region's centroid}
#' }
#'
#' @param mapping,data,stat,geom,position,na.rm,show.legend,inherit.aes,... See [ggplot2::geom_sf()].
#' @inheritParams resolve_feature_type
#'
#' @export
stat_centroids <- function(mapping = NULL, data = NULL,
                           geom = "sf", position = "circle_repel",
                           ...,
                           feature_type = NA,
                           na.rm = TRUE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatCentroids,
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

#' @rdname centroids
#' @usage NULL
#' @format NULL
#'
#' @export
StatCentroids <- ggplot2::ggproto("StatCentroids", ggplot2::Stat,
  required_aes = c("location"),
  default_aes = c(scale = 10),

  setup_data = function(data, params) {
    data <- ggplot2::Stat$setup_data(data, params)
    data$location <- resolve_feature_names(data$location, params$feature_type)
    data
  },

  setup_params = function(data, params) {
    params <- ggplot2::Stat$setup_params(data, params)
    if (is.null(params[["feature_type"]])) params$feature_type <- NA
    params$feature_type <- resolve_feature_type(params$feature_type, data$location,
                                                context = "stat_centroids")
    params
  },

  # protect the location column from is.finite
  compute_layer = function(self, data, params, layout) {
    if ("location" %in% names(data)) {
      data$location <- vctrs::new_vctr(data$location, class = "ggautomap_location")
    }
    ggplot2::ggproto_parent(ggplot2::Stat, self)$compute_layer(data, params, layout)
  },

  compute_group = function(data, scales, coord, feature_type) {
    if (inherits(data$location, "ggautomap_location")) {
      class(data$location) <- "character"
    }

    locations <- unique(data$location)
    geometry <- sf::st_geometry(get_geometry(feature_type))

    all_centroids <- sf::st_transform(geometry, crs_eqc())
    all_centroids <- sf::st_transform(sf::st_centroid(all_centroids), sf::st_crs(geometry))

    res <- all_centroids[match(data$location, get_feature_names(feature_type))]
    centroids <- matrix(unlist(res), ncol = 2, byrow = TRUE)
    sf::st_sf(geometry = res, x = centroids[,1], y = centroids[,2])
  }
)
