ggautomap_global <- new.env(parent = emptyenv())

#' Register a new feature type
#'
#' This adds a new feature type that can then be used by all the geoms in this
#' package. If registering from another package, this should occur in the
#' \code{.onLoad()} hook in the package.
#'
#' Registration supports delayed evaluation (lazy loading). This is particularly
#' useful for larger datasets, so that they are not loaded into memory until
#' they are accessed.
#'
#' @examples
#' register_map(
#'   "sf.nc",
#'   function() sf::st_read(system.file("shape/nc.shp", package = "sf")),
#'   feature_column = "NAME")
#'
#' @param feature_type Name of the type. Should include the package name to
#'   avoid clashes if registered in a package.
#' @param data A simple feature data frame with the map data, or a function
#'   that returns a data frame. When \code{lazy} is \code{TRUE}, the function
#'   will not be called until the data is first accessed.
#' @param feature_column Name of the column of \code{data} that contains the
#'   feature names.
#' @param aliases Optional named character vector or list that maps aliases to
#'   values that appear in the feature column. This allows abbreviations or
#'   alternative names to be supported.
#' @param outline Optional sf geometry containing just the outline of the map.
#' @param lazy When \code{TRUE}, defer evaulation of \code{data} until it it
#'   used.
#'
#' @export
register_map <- function(feature_type, data, feature_column,
                         aliases = NULL, outline = NULL, lazy = TRUE) {
  if (is.null(aliases)) aliases <- character(0)

  if (lazy) {
    delayedAssign(feature_type,
      list(
        data = validate_map_data(if (is.function(data)) data() else data,
                                 feature_column),
        feature_column = feature_column,
        aliases = aliases,
        outline = outline
      ),
      assign.env = ggautomap_global
    )
  } else {
    ggautomap_global[[feature_type]] <- list(
      data = validate_map_data(if (is.function(data)) data() else data,
                               feature_column),
      feature_column = feature_column,
      aliases = aliases,
      outline = outline
    )
  }
}

validate_map_data <- function(data, feature_column) {
  if (!inherits(data, "sf")) {
    cli::cli_abort("{.arg data} must be an sf object, not {class(data)}")
  }
  if (!feature_column %in% names(data)) {
    cli::cli_abort("{.field feature_column} {feature_column} not found")
  }
  invisible(data)
}

#' List known feature types
#'
#' @export
feature_types <- function() {
  names(ggautomap_global)
}

#' List known feature names
#'
#' This gives the list of feature names that are part of the specified map data.
#' When matching your data column to the map data, the names are resolved by
#' checking for the first match using:
#'   1. case sensitive match, then
#'   2. case sensitive match using aliases, then
#'   3. case insensitive match, then
#'   4. case insensitive match using aliases.
#'
#' @param feature_type Type of map feature. See [feature_types] for a list of
#'   registered types.
#'
#' @return Character vector of names.
#'
#' @export
feature_names <- function(feature_type) {
  if (is.na(feature_type)) cli::cli_abort("Must specify a {.arg feature_type}")
  names <- get_feature_names(feature_type)
  aliases <- get_aliases(feature_type)
  c(names, unname(aliases))
}

get_feature_names <- function(feature_type) {
  cfg <- ggautomap_global[[feature_type]]
  if (is.null(cfg)) cli::cli_abort("Unknown feature type {feature_type}")
  cfg$data[[cfg$feature_column]]
}

get_geom_feature_column <- function(feature_type) {
  cfg <- ggautomap_global[[feature_type]]
  if (is.null(cfg)) cli::cli_abort("Unknown feature type {feature_type}")
  cfg$feature_column
}

get_geometry <- function(feature_type) {
  cfg <- ggautomap_global[[feature_type]]
  if (is.null(cfg)) cli::cli_abort("Unknown feature type {feature_type}")
  cfg$data
}

get_geometry_loc <- function(feature_type, location) {
  geoms <- get_geometry(feature_type)
  geom_locations <- get_geom_feature_column(feature_type)
  geom_locations <- unlist(unclass(geoms)[geom_locations])

  if (!(location %in% geom_locations)) {
    cli::cli_abort("Location {location} is not a known {feature_type} feature")
  }
  sf::st_geometry(geoms[geom_locations == location,][1,])
}

get_aliases <- function(feature_type) {
  cfg <- ggautomap_global[[feature_type]]
  if (is.null(cfg)) cli::cli_abort("Unknown feature type {feature_type}")
  cfg$aliases
}

get_outline <- function(feature_type) {
  cfg <- ggautomap_global[[feature_type]]
  if (is.null(cfg)) cli::cli_abort("Unknown feature type {feature_type}")
  cfg$outline
}

#' Guess the feature type if it was missing
#'
#' @param feature_type Type of map feature. See [feature_types()] for a list of
#'   registered types. If \code{NA}, the type is guessed based on the values in
#'   the data.
#' @param locations Character vector of location names in the data.
#' @param context Name of the calling function, for inclusion in error message
#'   if it's not possible to unambiguously guess the feature type.
#'
#' @usage NULL
resolve_feature_type <- function (feature_type, locations, context) {
  if (is.null(feature_type)) return(NULL)
  if (is.na(feature_type)) feature_type <- guess_feature_type(locations)

  if (is.null(feature_type) || is.na(feature_type)) {
    cli::cli_abort("{.arg feature_type} must be provided for {.fn {context}}")
  }
  types <- feature_types()
  if (!(feature_type %in% types)) {
    cli::cli_abort(c(
      paste0("Unknown {.arg feature_type} {.val ", feature_type, "}"),
      i = "Expected one of {types}"
    ))
  }

  feature_type
}

resolve_feature_names <- function(locations, feature_type) {
  feature_names <- get_feature_names(feature_type)
  aliases <- get_aliases(feature_type)
  matches <- match_feature_names(locations, feature_names, aliases)

  unknown_features <- locations[is.na(matches)]
  if (length(unknown_features) > 0) {
    cli::cli_abort(c(
      paste0("{.field location} contains unexpected values"),
      "x" = "The unknown values are {unknown_features}.",
      "i" = "Expected {feature_type} names like {head(feature_names, n = 3)}.",
      "i" = "See feature_names('{feature_type}') for the full list."
    ))
  }

  feature_names[matches]
}

match_feature_names <- function(locations, feature_names, aliases) {
  if (length(locations) == 0) return(integer(0))

  mapply(
    function(...) {
      m <- c(...)
      m <- as.integer(m[!is.na(m)])
      if (length(m) > 0) m[[1]] else NA_integer_
    },
    match(locations, feature_names),
    match(aliases[locations], feature_names),
    match(tolower(locations), tolower(feature_names)),
    match(stats::setNames(aliases, tolower(names(aliases)))[tolower(locations)],
          feature_names),
    match(stats::setNames(tolower(aliases), tolower(names(aliases)))[tolower(locations)],
          tolower(feature_names))
  )
}

# FIXME this approach is forcing _all_ of the lazy loaded datasets
guess_feature_type <- function (locations) {
  locations <- unique(locations)
  types <- feature_types()

  found <- sapply(types, function (ty) {
    feature_names <- get_feature_names(ty)
    aliases <- get_aliases(ty)
    matches <- match_feature_names(locations, feature_names, aliases)
    sum(!is.na(matches))
  })

  if (length(locations) == 0) {
    cli::cli_abort(c("Unable to guess {.arg feature_type} from locations",
                     "x" = "{.field location} is empty",
                     "i" = "Specify {.arg feature_type} explicitly"))
  } else if (all(found == 0)) {
    cli::cli_abort(c("Unable to guess {.arg feature_type} from locations",
                     "x" = "These locations are not in any list of features: {head(locations, n = 3)}"))
  } else if (sum(found == max(found)) > 1) {
    cli::cli_abort(c("Unable to guess {.arg feature_type} from locations",
                     "x" = "The locations match multiple feature types: {names(found[found == max(found)])}",
                     "i" = "Specify {.arg feature_type} explicitly"))
  }
  names(found[which.max(found)])
}
