# Associate regions with counts

Counts the number of occurrences of each location, then by default maps
the count to the fill aesthetic. If your data has only one row per
location and some other field that you'd like to map to aesthetics, use
[`geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html) or
[`geom_sf_inset()`](https://cidm-ph.github.io/ggmapinset/reference/geom_sf_inset.html)
with `stat = "automap"` instead.

## Usage

``` r
geom_choropleth(
  mapping = ggplot2::aes(),
  data = NULL,
  stat = "choropleth",
  position = "identity",
  ...,
  feature_type = NA,
  inset = NA,
  map_base = "normal",
  map_inset = "auto",
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE
)

stat_choropleth(
  mapping = NULL,
  data = NULL,
  geom = "sf",
  position = "identity",
  ...,
  feature_type = NA,
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, stat, geom, position, na.rm, show.legend, inherit.aes,
  ...:

  See
  [`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html).

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

## Details

Note that choropleths have a tendency to be misleading by emphasising
geographically larger areas.

## Aesthetics

The `location` aesthetic is required. `geom_choropleth()` understands
the same aesthetics as
[`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html).

## Computed variables

- count:

  rows matching the region

- geometry:

  `sf` geometry column

- ...:

  limits as computed by
  [`ggplot2::stat_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)

## Examples

``` r
library(ggplot2)

cartographer::nc_type_example_2 |>
  ggplot(aes(location = county)) +
  geom_choropleth() +
  geom_boundaries(feature_type = "sf.nc") +
  scale_fill_steps(low = "#e6f9ff", high = "#00394d") +
  coord_automap(feature_type = "sf.nc")
```
