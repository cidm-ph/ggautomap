#' Associate regions with counts
#'
#' Instead of representing each row separately, aggregate them as counts that
#' can be used to draw choropleths. Note that choropleths have a tendency to be
#' misleading by emphasising geographically larger areas.
#'
#' @rdname choropleth
#'
#' @section Aesthetics:
#' The \code{location} aesthetic is required.
#' \code{geom_choropleth()} understands the same aesthetics as [ggplot2::geom_sf()].
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(nc_type_example, aes(location = location)) +
#' geom_boundaries(feature_type = "sf.nc") +
#' geom_choropleth() +
#' scale_fill_steps(low = "#e6f9ff", high = "#00394d", na.value = "white")
#'
#' @inheritParams stat_choropleth
#' @inheritParams ggmapinset::geom_sf_inset
#'
#' @export
geom_choropleth <- function(mapping = ggplot2::aes(), data = NULL,
                            stat = "choropleth", position = "identity",
                            ...,
                            feature_type = NA,
                            inset = NULL,
                            inset_copy = TRUE,
                            inset_clip = FALSE,
                            na.rm = TRUE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  params = rlang::list2(
    feature_type = feature_type,
    na.rm = na.rm,
    ...
  )
  if (is.null(mapping[["colour"]]) & is.null(params[["colour"]])) {
    params$colour <- NA
  }

  ggmapinset::build_sf_inset_layers(
    data = data, mapping = mapping,
    stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes, params = params,
    inset = inset, inset_copy = inset_copy, inset_clip = inset_clip
  )
}

#' @rdname choropleth
#'
#' @section Computed variables:
#' \describe{
#'   \item{count}{rows matching the region}
#'   \item{geometry}{\code{sf} geometry column}
#'   \item{...}{limits as computed by [ggplot2::stat_sf()]}
#' }
#'
#' @param mapping,data,stat,geom,position,na.rm,show.legend,inherit.aes,... See [ggplot2::geom_sf()].
#' @inheritParams resolve_feature_type
#'
#' @export
stat_choropleth <- function(mapping = NULL, data = NULL,
                            geom = "sf", position = "identity",
                            ...,
                            feature_type = NA,
                            na.rm = TRUE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  ggplot2::layer_sf(
    data = data,
    mapping = mapping,
    stat = StatChoropleth,
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

#' @rdname choropleth
#' @usage NULL
#' @format NULL
#'
#' @importFrom rlang .data
#' @export
StatChoropleth <- ggplot2::ggproto("StatChoropleth", ggplot2::StatSf,
  required_aes = c("location"),
  default_aes = ggplot2::aes(fill = ggplot2::after_stat(count)),

  setup_data = function(data, params) {
    data <- ggplot2::StatSf$setup_data(data, params)
    data$location <- resolve_feature_names(data$location, params$feature_type)
    data
  },

  setup_params = function(data, params) {
    params <- ggplot2::StatSf$setup_params(data, params)
    if (is.null(params[["feature_type"]])) params$feature_type <- NA
    params$feature_type <- resolve_feature_type(params$feature_type, data$location,
                                                context = "{.fn stat_choropleth}")
    params
  },

  # protect the location column from is.finite
  compute_layer = function(self, data, params, layout) {
    if (!is.null(data[["location"]])) {
      data$location <- vctrs::new_vctr(data$location, class = "ggautomap_location")
    }

    ggplot2::ggproto_parent(ggplot2::StatSf, self)$compute_layer(data, params, layout)
  },

  compute_group = function(data, scales, coord, feature_type) {
    if (inherits(data$location, "ggautomap_location")) {
      class(data$location) <- "character"
    }

    counts <- dplyr::count(data, location = .data$location, name = "count")
    geoms <- sapply(counts$location, function (x) { get_geometry_loc(feature_type, x) }, USE.NAMES = FALSE)
    counts$geometry <- sf::st_sfc(geoms, crs = sf::st_crs(get_geometry(feature_type)))

    ggplot2::StatSf$compute_group(sf::st_as_sf(counts), scales, coord)
  }
)
