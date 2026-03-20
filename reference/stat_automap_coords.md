# Attach coordinates with 'cartographer'

Use 'cartographer' to attach a spatial column to the data based on place
names in another column. The spatial data is then reduced to coordinates
in the same way as
[`stat_sf_coordinates()`](https://ggplot2.tidyverse.org/reference/stat_sf_coordinates.html).

## Usage

``` r
stat_automap_coords(
  mapping = NULL,
  data = NULL,
  geom = "sf_inset",
  position = "identity",
  ...,
  feature_type = NA,
  na.rm = TRUE,
  inset = NA,
  fun.geometry = NULL,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping, data, geom, position, na.rm, show.legend, inherit.aes,
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

## Value

A plot layer

## Computed variables

- geometry:

  `sf` geometry column representing the points

- x:

  X dimension of the simple feature

- y:

  Y dimension of the simple feature

- x_inset:

  X dimension of the simple feature after inset transformation

- y_inset:

  Y dimension of the simple feature after inset transformation

- inside_inset:

  logical indicating points inside the inset viewport

- inset_scale:

  1 for points outside the inset, otherwise the configured inset scale
  parameter

## See also

[`ggmapinset::stat_sf_coordinates_inset()`](https://cidm-ph.github.io/ggmapinset/reference/stat_sf_coordinates_inset.html)

## Examples

``` r
library(ggplot2)

events <- data.frame(
  county = c("Mecklenburg", "Carteret", "Moore", "Caldwell"),
  proportion_A = c(0.1, 0.8, 0.0, 0.6)
)

ggplot(events, aes(location = county)) +
  geom_sf(aes(fill = proportion_A), stat = "automap") +
  geom_label(aes(label = county), stat = "automap_coords") +
  coord_automap(feature_type = "sf.nc")
```
