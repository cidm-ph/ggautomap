# Coordinate reference system for spatial computations

`crs_eqc()` gives a CRS that can be used for e.g. computing centroids or
distances. It is an equidistant cylindrical system that by does not
distort latitudes near `latitude`. The CRS is in units of kilometres by
default.

## Usage

``` r
crs_eqc(latitude = 0, units = "km")
```

## Arguments

- latitude:

  The latitude of true scale (the `proj` parameter `lat_ts`). This is
  the latitude where the scale is not distorted by the projection.

- units:

  Base length unit (e.g. `"km"` or `"mi"`). See
  [`ggmapinset::configure_inset()`](https://cidm-ph.github.io/ggmapinset/reference/configure_inset.html)
  for supported values.

## Value

CRS object from `sf`.

## Examples

``` r
# Sydney, Australia has a latitude of 33.87 S so this CRS will be suitable
# for computations close to there:
crs_eqc(latitude = -33.87)
#> Coordinate Reference System:
#>   User input: +proj=eqc +lat_ts=-33.87 +units=km 
#>   wkt:
#> PROJCRS["unknown",
#>     BASEGEOGCRS["unknown",
#>         DATUM["World Geodetic System 1984",
#>             ELLIPSOID["WGS 84",6378137,298.257223563,
#>                 LENGTHUNIT["metre",1]],
#>             ID["EPSG",6326]],
#>         PRIMEM["Greenwich",0,
#>             ANGLEUNIT["degree",0.0174532925199433],
#>             ID["EPSG",8901]]],
#>     CONVERSION["unknown",
#>         METHOD["Equidistant Cylindrical",
#>             ID["EPSG",1028]],
#>         PARAMETER["Latitude of 1st standard parallel",-33.87,
#>             ANGLEUNIT["degree",0.0174532925199433],
#>             ID["EPSG",8823]],
#>         PARAMETER["Longitude of natural origin",0,
#>             ANGLEUNIT["degree",0.0174532925199433],
#>             ID["EPSG",8802]],
#>         PARAMETER["False easting",0,
#>             LENGTHUNIT["kilometre",1000],
#>             ID["EPSG",8806]],
#>         PARAMETER["False northing",0,
#>             LENGTHUNIT["kilometre",1000],
#>             ID["EPSG",8807]]],
#>     CS[Cartesian,2],
#>         AXIS["(E)",east,
#>             ORDER[1],
#>             LENGTHUNIT["kilometre",1000,
#>                 ID["EPSG",9036]]],
#>         AXIS["(N)",north,
#>             ORDER[2],
#>             LENGTHUNIT["kilometre",1000,
#>                 ID["EPSG",9036]]]]
```
