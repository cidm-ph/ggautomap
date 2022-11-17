.onLoad <- function(libname, pkgname) {
  register_map("sf.nc",
    function() sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE),
    feature_column = "NAME")
}
