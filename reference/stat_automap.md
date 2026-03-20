# Attach spatial data with 'cartographer'

Use 'cartographer' to attach a spatial column to the data based on place
names in another column. The result can then be used by
[`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)
or
[`ggmapinset::geom_sf_inset()`](https://cidm-ph.github.io/ggmapinset/reference/geom_sf_inset.html).

## Usage

``` r
stat_automap(
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

- mapping, data, geom, position, na.rm, show.legend, inherit.aes, ...:

  See
  [`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html).

- feature_type:

  Type of map feature. See
  [`feature_types()`](https://cidm-ph.github.io/cartographer/reference/feature_types.html)
  for a list of registered types. If `NA`, the type is guessed based on
  the values in `feature_names`.

## Value

A ggplot layer

## Computed variables

- geometry:

  `sf` geometry column

- ...:

  limits as computed by
  [`ggplot2::stat_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)

## Examples

``` r
library(ggplot2)

events <- data.frame(
  county = c("Mecklenburg", "Carteret", "Moore", "Caldwell"),
  proportion_A = c(0.1, 0.8, 0.0, 0.6)
)

ggplot(events, aes(location = county)) +
  geom_sf(aes(fill = proportion_A), stat = "automap")
#> Warning: Guessing `feature_type`; provide `feature_type` to coord_automap() to suppress


ggplot(events, aes(location = county)) +
  stat_automap(aes(fill = proportion_A)) +
  coord_automap(feature_type = "sf.nc")
```
