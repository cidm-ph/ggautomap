# Map feature boundaries

Retrieves the full map data from `{cartographer}` and plots the
boundaries. As well as the chosen feature boundaries, the outline of the
map is drawn separately if one has been registered with the map data,
with the possibility to override its aesthetics.

## Usage

``` r
geom_boundaries(
  mapping = ggplot2::aes(),
  data = NULL,
  stat = "sf_inset",
  position = "identity",
  ...,
  feature_type = NULL,
  inset = NA,
  map_base = "normal",
  map_inset = "auto",
  na.rm = FALSE,
  outline.aes = list(colour = "#666666"),
  show.legend = NA,
  inherit.aes = FALSE
)
```

## Arguments

- mapping, stat, position, na.rm, show.legend, inherit.aes, ...:

  See
  [`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html).

- data:

  Ignored (this geometry always uses the registered geographic data).

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

- outline.aes:

  A list to override the aesthetics for the outline of the map. This has
  no effect if the map wasn't registered with a separate outline.

## Value

A ggplot layer.

## Examples

``` r
library(ggplot2)

ggplot() +
  geom_boundaries(feature_type = "sf.nc")
```
