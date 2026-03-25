# ggautomap (development version)

* Updated to use new inset shape definition style from {ggmapinset}.
  There is no longer a need to override the `configure_inset()` function
  which was brittle and depended on order of package attachment to work.
* Require a newer version of {ggmapinset} to fix a confusing behaviour
  where a geom with no inset configured could sometimes draw two copies
  of itself (#5).
* `stat_geoscatter()` and `geom_geoscatter()` now use a fixed seed by
  default so that the position of scattered points is reproducible.
  The old behaviour can be restored by setting `seed = NA`.

# ggautomap 0.3.3

* Doc tweaks and housekeeping.

# ggautomap 0.3.2

* Initial release on CRAN.
