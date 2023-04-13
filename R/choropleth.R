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
#' @inheritParams stat_choropleth
#' @inheritParams ggmapinset::geom_sf_inset
#'
#' @returns A ggplot layer.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' cartographer::nc_type_example[1:49,] |>
#'   ggplot(aes(location = county)) +
#'   geom_boundaries(feature_type = "sf.nc") +
#'   geom_choropleth() +
#'   scale_fill_steps(low = "#e6f9ff", high = "#00394d", na.value = "white")
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
#' @inheritParams cartographer::resolve_feature_type
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
    data$location <- cartographer::resolve_feature_names(data$location,
                                                         params$feature_type)
    data
  },

  setup_params = function(data, params) {
    params <- ggplot2::StatSf$setup_params(data, params)
    if (is.null(params[["feature_type"]])) params$feature_type <- NA
    params$feature_type <- cartographer::resolve_feature_type(params$feature_type,
                                                              data$location)
    params
  },

  compute_panel = function(data, scales, coord, feature_type) {
    counts <- dplyr::count(data, location = .data$location, name = "count")
    geoms <- cartographer::map_sfc(counts$location, feature_type)
    crs_data <- sf::st_crs(cartographer::map_sf(feature_type))
    counts$geometry <- sf::st_sfc(geoms, crs = crs_data)

    ggplot2::StatSf$compute_panel(sf::st_as_sf(counts), scales, coord)
  }
)
