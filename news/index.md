# Changelog

## ggautomap (development version)

- Updated to use new inset shape definition style from {ggmapinset}.
  There is no longer a need to override the
  [`configure_inset()`](https://cidm-ph.github.io/ggmapinset/reference/configure_inset.html)
  function which was brittle and depended on order of package attachment
  to work.
- Require a newer version of {ggmapinset} to fix a confusing behaviour
  where a geom with no inset configured could sometimes draw two copies
  of itself ([\#5](https://github.com/cidm-ph/ggautomap/issues/5)).
- [`stat_geoscatter()`](https://cidm-ph.github.io/ggautomap/reference/geoscatter.md)
  and
  [`geom_geoscatter()`](https://cidm-ph.github.io/ggautomap/reference/geoscatter.md)
  now use a fixed seed by default so that the position of scattered
  points is reproducible. The old behaviour can be restored by setting
  `seed = NA`.

## ggautomap 0.3.3

CRAN release: 2025-09-10

- Doc tweaks and housekeeping.

## ggautomap 0.3.2

CRAN release: 2023-05-24

- Initial release on CRAN.
