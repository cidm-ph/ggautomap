# Geographic centroid of locations

Assigns each point a longitude and latitude corresponding to the
geographic centre of its administrative area. This means that all points
in the same area will overlap. The default `position` uses
[`position_circle_repel()`](https://cidm-ph.github.io/ggautomap/reference/position_circle_repel.md)
to repel the points outwards with an amount controllable with its
`scale` parameter.

## Usage

``` r
geom_centroids(
  mapping = ggplot2::aes(),
  data = NULL,
  stat = "automap_coords",
  position = "circle_repel_sf",
  ...,
  fun.geometry = NULL,
  feature_type = NA,
  inset = NA,
  map_base = "clip",
  map_inset = "auto",
  na.rm = TRUE,
  show.legend = "point",
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, position, na.rm, show.legend, inherit.aes,
  fun.geometry, ...:

  See
  [`ggplot2::stat_sf_coordinates()`](https://ggplot2.tidyverse.org/reference/stat_sf_coordinates.html).

- feature_type:

  Type of map feature. See
  [`feature_types()`](https://cidm-ph.github.io/cartographer/reference/feature_types.html)
  for a list of registered types. If `NA`, the type is guessed based on
  the values in `feature_names`.

- inset:

  Inset configuration; see
  [`configure_inset()`](https://cidm-ph.github.io/ggmapinset/reference/configure_inset.html).
  If `NA` (the default), this is inherited from the coord (see
  [`coord_sf_inset()`](https://cidm-ph.github.io/ggmapinset/reference/coord_sf_inset.html)).

- map_base:

  Controls the layer with the base map. Possible values are `"normal"`
  to create a layer as though the inset were not specified, `"clip"` to
  create a layer with the inset viewport cut out, and `"none"` to
  prevent the insertion of a layer for the base map.

- map_inset:

  Controls the layer with the inset map. Possible values are `"auto"` to
  choose the behaviour based on whether `inset` is specified, `"normal"`
  to create a layer with the viewport cut out and transformed, and
  `"none"` to prevent the insertion of a layer for the viewport map.

## Value

A ggplot layer.

## Aesthetics

The `location` aesthetic is required. `geom_centroids()` understands the
same aesthetics as
[`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html).

## Examples

``` r
library(ggplot2)

cartographer::nc_type_example_2 |>
  head(n = 100) |>
  ggplot(aes(location = county)) +
  geom_boundaries(feature_type = "sf.nc") +
  geom_centroids(aes(colour = type), position = position_circle_repel_sf(scale = 6), size = 0.5) +
  coord_automap(feature_type = "sf.nc")
```
