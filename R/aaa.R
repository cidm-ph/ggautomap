#' @importFrom ggmapinset geom_inset_frame
#' @export
ggmapinset::geom_inset_frame

#' @importFrom ggmapinset geom_sf_inset
#' @export
ggmapinset::geom_sf_inset

#' @importFrom ggmapinset GeomSfInset
#' @export
ggmapinset::GeomSfInset

#' @importFrom ggmapinset stat_sf_inset
#' @export
ggmapinset::stat_sf_inset

#' @importFrom ggmapinset StatSfInset
#' @export
ggmapinset::StatSfInset

#' @importFrom ggmapinset StatSfCoordinatesInset
#' @export
ggmapinset::StatSfCoordinatesInset

#' @importFrom ggmapinset geom_sf_text_inset
#' @export
ggmapinset::geom_sf_text_inset

#' @importFrom ggmapinset geom_sf_label_inset
#' @export
ggmapinset::geom_sf_label_inset

#' @importFrom ggmapinset configure_inset
#' @export
ggmapinset::configure_inset

#' @importFrom ggmapinset shape_circle
#' @export
ggmapinset::shape_circle

#' @importFrom ggmapinset shape_rectangle
#' @export
ggmapinset::shape_rectangle

#' @importFrom ggmapinset shape_sf
#' @export
ggmapinset::shape_sf

#' @importFrom ggmapinset coerce_centre
#' @export
coerce_centre.character <- function(centre, ...) {
  args <- rlang::list2(...)
  extra_args <- setdiff(rlang::names2(args), c("feature_type"))
  extra_args[extra_args == ""] <- "(unnamed)"
  if (!rlang::is_empty(extra_args)) {
    cli::cli_abort(
      "unused arguments {.arg {extra_args}} for {.fn coerce_character}"
    )
  }
  feature_type <- get0("feature_type", as.environment(args), ifnotfound = NA)

  feature_type <- cartographer::resolve_feature_type(feature_type, centre)
  centre <- cartographer::resolve_feature_names(centre, feature_type)

  geom <- cartographer::map_sfc(centre, feature_type)
  crs_orig <- sf::st_crs(geom)

  crs_working <- crs_eqc_midpoint(feature_type)
  geom <- sf::st_transform(geom, crs_working)
  centre <- sf::st_transform(sf::st_centroid(geom), crs_orig)
  centre
}
