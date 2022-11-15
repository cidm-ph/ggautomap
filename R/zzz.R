.onLoad <- function(libname, pkgname) {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  register_map("sf.nc", nc, "NAME")
}
