#' Associate regions with counts
#'
#' Counts the number of occurrences of each location, then by default maps the
#' count to the fill aesthetic. If your data has only one row per location and
#' some other field that you'd like to map to aesthetics, use
#' [`geom_sf()`][ggplot2::geom_sf] or [`geom_sf_inset()`][ggmapinset::geom_sf_inset]
#' with `stat = "automap"` instead.
#'
#' Note that choropleths have a tendency to be misleading by emphasising
#' geographically larger areas.
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
#' cartographer::nc_type_example_2 |>
#'   ggplot(aes(location = county)) +
#'   geom_choropleth() +
#'   geom_boundaries(feature_type = "sf.nc") +
#'   scale_fill_steps(low = "#e6f9ff", high = "#00394d") +
#'   coord_automap(feature_type = "sf.nc")
geom_choropleth <- function(mapping = ggplot2::aes(), data = NULL,
                            stat = "choropleth", position = "identity",
                            ...,
                            feature_type = NA,
                            inset = NA,
                            map_base = "normal",
                            map_inset = "auto",
                            na.rm = TRUE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  params <- rlang::list2(
    feature_type = feature_type,
    na.rm = na.rm,
    ...
  )
  if (is.null(mapping[["colour"]]) && is.null(params[["colour"]])) {
    params$colour <- NA
  }

  ggmapinset::build_sf_inset_layers(
    data = data, mapping = mapping,
    stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes, params = params,
    inset = inset, map_base = map_base, map_inset = map_inset
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
StatChoropleth <- ggplot2::ggproto("StatChoropleth", StatAutomap,
  default_aes = ggplot2::aes(fill = ggplot2::after_stat(count)),

  compute_panel = function(data, scales, coord, feature_type = NA) {
    counts <- dplyr::count(data, location = .data$location, name = "count")
    StatAutomap$compute_panel(counts, scales, coord, feature_type)
  }
)
