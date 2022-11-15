#' Summarise regions with pie charts
#'
#' @section Aesthetics:
#' The \code{location} aesthetic is required.
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(nc_type_example, aes(location = location)) +
#' geom_boundaries(feature_type = "sf.nc") +
#' geom_pie(aes(fill = type), pie_radius = 0.1)
#'
#' @param pie_radius Scale the side of all pies
#' @param proportional If \code{TRUE}, scale the pies by the number of rows in
#'   each region. The radius of each pi is proportional to the count.
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,... See [ggplot2::geom_sf()].
#' @inheritParams resolve_feature_type
#'
#' @export
geom_pie <- function(mapping = ggplot2::aes(), data = NULL,
                     stat = "centroid_pie", position = "identity",
                     ...,
                     feature_type = NA,
                     pie_radius = 1,
                     proportional = FALSE,
                     #inset = NULL,
                     #inset_copy = TRUE,
                     #inset_clip = TRUE,
                     na.rm = TRUE,
                     show.legend = TRUE,
                     inherit.aes = TRUE) {
  params <- rlang::list2(
    feature_type = feature_type,
    pie_radius = pie_radius,
    proportional = proportional,
    na.rm = na.rm,
    ...
  )

  ggplot2::layer(
    data = data, mapping = mapping,
    stat = stat, geom = GeomPie, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes, params = params
  )
}

#' @rdname geom_pie
#' @usage NULL
#' @format NULL
#'
#' @export
GeomPie <- ggplot2::ggproto("GeomPie", ggforce::GeomShape,
  default_aes = modifyList(ggforce::GeomShape$default_aes,
                           list(linewidth = 0.2, colour = "black"))
)

#' @rdname geom_pie
#' @usage NULL
#' @format NULL
#'
#' @importFrom rlang .data
#' @export
StatCentroidPie <- ggplot2::ggproto("StatCentroidPie", ggplot2::Stat,
  required_aes = c("location"),

  setup_data = function(data, params) {
    data <- ggplot2::Stat$setup_data(data, params)
    validate_location(data$location, params$feature_type)

    # override group, as if we had specified aes(group = location)
    data$group <- as.integer(factor(data$location))

    data <- dplyr::add_count(data, .data$group, name = "group_size")
    data$group_size <- data$group_size / nrow(data) * length(unique(data$location))

    data
  },

  setup_params = function(data, params) {
    params <- ggplot2::Stat$setup_params(data, params)
    if (is.null(params[["feature_type"]])) params$feature_type <- NA
    params$feature_type <- resolve_feature_type(params$feature_type, data$location,
                                                context = "{.fn stat_centroid_pie}")
    params
  },

  # protect the location column from is.finite
  compute_layer = function(self, data, params, layout) {
    if ("location" %in% names(data)) {
      data$location <- vctrs::new_vctr(data$location, class = "ggautomap_location")
    }
    ggplot2::ggproto_parent(ggplot2::Stat, self)$compute_layer(data, params, layout)
  },

  compute_group = function(data, scales, coord, feature_type, pie_radius, proportional) {
    if (inherits(data$location, "ggautomap_location")) {
      class(data$location) <- "character"
    }

    entries <- data$group_size[[1]]

    data <- dplyr::count(data, dplyr::across(dplyr::everything()), name = "amount")
    angles <- cumsum(data$amount)
    angles <- angles / max(angles) * 2 * pi
    data$start <- c(0, angles[-length(angles)])
    data$end <- angles
    data <- data[,!names(data) %in% c("amount", "group_size")]

    data$r <- if (proportional) entries * pie_radius else pie_radius

    crs_orig <- sf::st_crs(get_geometry(feature_type))
    crs_working <- crs_eqc_midpoint(feature_type)

    geoms <- sapply(data$location, function (x) { get_geometry_loc(feature_type, x) }, USE.NAMES = FALSE)
    geometry <- sf::st_sfc(geoms, crs = crs_orig)
    centroids <- sf::st_transform(geometry, crs_working)
    centroids <- sf::st_transform(sf::st_centroid(centroids), crs_orig)
    centroids <- matrix(unlist(centroids), ncol = 2, byrow = TRUE)
    data$x0 <- centroids[,1]
    data$y0 <- centroids[,2]

    arcPaths(data)
  }
)

# -----------------------------------------------------------------------------
# The remainder of this file is adapted from the ggforce package.
#
# Original source:     https://github.com/thomasp85/ggforce/tree/9be635c582559f016254b111770a61e4b4aa0958
# Original copyright:  Copyright (c) 2019 Thomas Lin Pedersen
# Original license:    MIT License

data_frame0 <- function(...) vctrs::data_frame(..., .name_repair = "minimal")

make_unique <- function(x, sep = '.') {
  if (!anyDuplicated(x)) return(x)
  groups <- match(x, unique(x))
  suffix <- unsplit(lapply(split(x, groups), seq_along), groups)
  max_chars <- nchar(max(suffix))
  suffix_format <- paste0('%0', max_chars, 'd')
  paste0(x, sep, sprintf(suffix_format, suffix))
}

arcPaths <- function(data) {
  trans <- ggforce::radial_trans(c(0, 1), c(0, 2 * pi), pad = 0)
  data <- data[data$start != data$end, ]
  data$nControl <- ceiling(360 / (2 * pi) * abs(data$end - data$start))
  data$nControl[data$nControl < 3] <- 3
  extraData <- !names(data) %in% c('r0', 'r', 'start', 'end', 'group')
  data$group <- make_unique(as.character(data$group))
  paths <- lapply(seq_len(nrow(data)), function(i) {
    path <- data_frame0(
      a = seq(data$start[i], data$end[i], length.out = data$nControl[i]),
      r = data$r[i]
    )
    path <- vctrs::vec_rbind(path, data_frame0(a = data$start[i], r = 0))
    path$group <- data$group[i]
    path$index <- seq(0, 1, length.out = nrow(path))
    path <- cbind(path, data[rep(i, nrow(path)), extraData, drop = FALSE])
  })
  paths <- vctrs::vec_rbind(!!!paths)
  paths <- cbind(
    paths[, !names(paths) %in% c('r', 'a')],
    trans$transform(paths$r, paths$a)
  )
  paths$x <- paths$x + paths$x0
  paths$y <- paths$y + paths$y0
  paths[, !names(paths) %in% c('x0', 'y0', 'nControl')]
}
