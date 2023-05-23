#' Map feature boundaries
#'
#' Retrieves the full map data from `{cartographer}` and plots the boundaries.
#' As well as the chosen feature boundaries, the outline of the map is
#' drawn separately if one has been registered with the map data, with the
#' possibility to override its aesthetics.
#'
#' @param outline.aes A list to override the aesthetics for the outline of the map.
#'   This has no effect if the map wasn't registered with a separate outline.
#' @param data Ignored (this geometry always uses the registered geographic data).
#' @param mapping,stat,position,na.rm,show.legend,inherit.aes,... See [ggplot2::geom_sf()].
#' @inheritParams cartographer::resolve_feature_type
#' @inheritParams ggmapinset::geom_sf_inset
#'
#' @returns A ggplot layer.
#' @importFrom utils modifyList
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot() +
#'   geom_boundaries(feature_type = "sf.nc")
geom_boundaries <- function(mapping = ggplot2::aes(),
                            data = NULL,
                            stat = "sf_inset", position = "identity",
                            ...,
                            feature_type = NULL,
                            inset = NA,
                            map_base = "normal",
                            map_inset = "auto",
                            na.rm = FALSE,
                            outline.aes = list(colour = "#666666"),
                            show.legend = NA,
                            inherit.aes = FALSE) {
  if (!is.null(data)) {
    cli::cli_abort("{.fn geom_boundaries} provides its own {.arg data}")
  }
  if (is.null(feature_type) || is.na(feature_type)) {
    cli::cli_abort(c("{.arg feature_type} must be specified for {.fn geom_boundaries}",
      "i" = "Registered types: {cartographer::feature_types()}"
    ))
  }

  boundaries(
    mapping = mapping,
    data_inner = cartographer::map_sf(feature_type),
    data_outline = cartographer::map_outline(feature_type),
    stat = stat, position = position, ...,
    inset = inset, map_base = map_base, map_inset = map_inset,
    na.rm = na.rm,
    outline.aes = outline.aes,
    show.legend = show.legend, inherit.aes = inherit.aes
  )
}

boundaries <- function(mapping,
                       data_inner,
                       data_outline,
                       stat,
                       position,
                       ...,
                       inset = NA,
                       map_base = "normal",
                       map_inset = "auto",
                       na.rm,
                       outline.aes,
                       show.legend,
                       inherit.aes) {
  params <- rlang::list2(na.rm = na.rm, ...)
  if (is.null(mapping[["colour"]]) && is.null(params[["colour"]])) {
    params$colour <- "#bbbbbb"
  }
  if (is.null(mapping[["fill"]]) && is.null(params[["fill"]])) {
    params$fill <- NA
  }

  layers <- ggmapinset::build_sf_inset_layers(
    data = data_inner, mapping = mapping,
    stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes, params = params,
    inset = inset, map_base = map_base, map_inset = map_inset
  )

  if (!is.null(data_outline)) {
    params_outline <- modifyList(params, outline.aes, keep.null = TRUE)
    layers <- c(
      layers,
      ggmapinset::build_sf_inset_layers(
        data = data_outline, mapping = ggplot2::aes(),
        stat = stat, position = position,
        show.legend = show.legend, inherit.aes = inherit.aes, params = params_outline,
        inset = inset, map_base = map_base, map_inset = map_inset
      )
    )
  }

  c(layers, ggplot2::coord_sf(default = TRUE))
}
