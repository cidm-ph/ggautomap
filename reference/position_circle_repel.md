# Pack overlapping points into a circle

This position looks for any points with identical `x` and `y` positions
and packs them in a circle around the original point. The `_sf` version
applies the position adjustment in projected coordinates.

## Usage

``` r
position_circle_repel(scale = 1/4)

position_circle_repel_sf(scale = 10)
```

## Arguments

- scale:

  Scale of packing around the central point. This is in data units, so
  for the `_sf` variant it will depend on the units specified by the
  coordinate reference system.

## Value

A ggplot position object.

## Details

Note that extreme choices of `scale` may cause errors.

The `scale` parameter can instead be specified as an aesthetic for geoms
that support it
([`geom_centroids()`](https://cidm-ph.github.io/ggautomap/reference/geom_centroids.md)).
This allows different locations to have different scales, which is
especially useful when combined with map insets.

## Examples

``` r
library(ggplot2)

points <- data.frame(
  x = c(rep(1, 10), 1:3),
  y = c(rep(2, 10), 3:5),
  s = 0.05
)
ggplot(points, aes(x, y)) +
  geom_point(size = 3, colour = "red") +
  geom_point(position = position_circle_repel(0.05), size = 3, alpha = 0.5)


cartographer::nc_type_example_2 |>
  dplyr::filter(!county %in% c("HENDERSON", "GASTON", "LINCOLN")) |>
  ggplot(aes(location = county)) +
  geom_boundaries(feature_type = "sf.nc") +
  geom_centroids(aes(colour = type), position = position_circle_repel_sf(scale = 4), size = 0.2) +
  coord_automap(feature_type = "sf.nc")
```
