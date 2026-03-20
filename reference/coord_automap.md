# Specify an inset configuration for the whole plot

This allows a default inset configuration to be provided to avoid having
to repeat it for each layer. Any layer that is inset-aware can use this
as the default configuration if none is specifically provided to that
layer. This coord also expands the axis limits to include the inset
area.

## Usage

``` r
coord_automap(feature_type = NA, inset = NULL, ...)
```

## Arguments

- feature_type:

  Type of map feature. See
  [`feature_types()`](https://cidm-ph.github.io/cartographer/reference/feature_types.html)
  for a list of registered types. If `NA`, the type is guessed based on
  the values in `feature_names`.

- inset:

  Inset configuration; see
  [`configure_inset()`](https://cidm-ph.github.io/ggautomap/reference/configure_inset.md).

- ...:

  Arguments passed to
  [`ggmapinset::coord_sf_inset()`](https://cidm-ph.github.io/ggmapinset/reference/coord_sf_inset.html)

## Value

A ggplot coordinate

## Examples

``` r
library(ggplot2)
library(cartographer)

ggplot(nc_type_example_2, aes(location = county)) +
  geom_choropleth(aes(colour = type), size = 0.5) +
  geom_sf_label_inset(aes(label = county), stat = "automap_coords", size = 3) +
  coord_automap(feature_type = "sf.nc")
```
