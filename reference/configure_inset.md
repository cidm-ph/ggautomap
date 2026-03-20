# Configure transformations underpinning a map inset

This specialises
[`ggmapinset::configure_inset()`](https://cidm-ph.github.io/ggmapinset/reference/configure_inset.html)
to allow the `centre` to be specified as a location. The centroid of
that location is used as the inset's centre.

## Usage

``` r
configure_inset(
  centre = NULL,
  scale = NULL,
  translation = NULL,
  radius = NULL,
  units = "km",
  feature_type = NA
)
```

## Arguments

- centre:

  Coordinates of the inset centre. Can instead be the name of a
  geographic feature if `feature_type` is also provided.

- scale:

  Zoom scale: values larger than one will make the circle bigger.

- translation:

  Translate (shift) the inset. This can be an `st_point` or simply a
  vector of length 2 containing the x and y offsets respectively.

- radius:

  Radius of the inset circle.

- units:

  Base length unit (e.g. `"km"` or `"mi"`). See
  [`ggmapinset::configure_inset()`](https://cidm-ph.github.io/ggmapinset/reference/configure_inset.html)
  for supported values.

- feature_type:

  Type of map feature. See
  [`feature_types()`](https://cidm-ph.github.io/cartographer/reference/feature_types.html)
  for a list of registered types. If `NA`, the type is guessed based on
  the values in `feature_names`.

## Value

An inset configuration object.

## See also

ggmapinset::configure_inset

## Examples

``` r
cfg <- configure_inset(
  centre = "Yancey",
  feature_type = "sf.nc",
  scale = 2,
  translation = c(70, -180),
  radius = 50,
  units = "mi"
)
#> Warning: The `radius` argument of `configure_inset()` is deprecated as of ggmapinset
#> 0.4.0.
#> ℹ Use `shape = shape_circle(centre, radius)` instead.
#> ℹ The deprecated feature was likely used in the ggautomap package.
#>   Please report the issue at <https://github.com/cidm-ph/ggautomap/issues>.
```
