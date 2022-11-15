#' @include ggautomap-global.R
NULL

#' @export
vec_math.ggautomap_location <- function(.fn, .x, ...) {
  switch(.fn, is.finite = !is.na(.x), stop("n/a"))
} # NB: part of workaround used in implementing geoms

#' @import sf
#' @import vctrs
#' @import ggplot2
NULL

#' @importFrom ggmapinset geom_inset_frame
#' @export
ggmapinset::geom_inset_frame
