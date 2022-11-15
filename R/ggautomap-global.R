ggautomap_global <- new.env(parent = emptyenv())

ggautomap_global$maps <- list()

#' Register a new feature type
#'
#' This adds a new feature type that can then be used by all the geoms in this
#' package. If registering from another package, this should occur in the
#' \code{.onLoad()} hook in the package.
#'
#' @param feature_type Name of the type. Should include the package name to
#'   avoid clashes if registered in a package.
#' @param data A simple feature data frame with the map data.
#' @param feature_column Name of the column of \code{data} that contains the
#'   feature names.
#' @param aliases Optional named character vector or list that maps aliases to
#'   values that appear in the feature column. This allows abbreviations or
#'   alternative names to be supported.
#' @param outline Optional sf geometry containing just the outline of the map.
#'
#' @export
register_map <- function(feature_type, data, feature_column,
                         aliases = NULL, outline = NULL) {
  if (!inherits(data, "sf")) {
    cli::cli_abort("{.arg data} must be an sf object, not {class(data)}")
  }
  if (!feature_column %in% names(data)) {
    cli::cli_abort("{.field feature_column} {feature_column} not found")
  }
  if (is.null(aliases)) aliases <- c()

  ggautomap_global$maps[[feature_type]] <- list(
    data = data,
    feature_column = feature_column,
    aliases = aliases,
    outline = outline
  )
}

#' List known feature types
#'
#' @export
feature_types <- function() {
  names(ggautomap_global$maps)
}

#' List known feature names
#'
#' @param feature_type Type of map feature. See [feature_types] for a list of
#'   registered types.
#'
#' @return Character vector of names.
#'
#' @export
feature_names <- function(feature_type) {
  if (is.na(feature_type)) cli::cli_abort("Must specify a {.arg feature_type}")
  get_feature_names(feature_type)
}

get_feature_names <- function(feature_type) {
  cfg <- ggautomap_global$maps[[feature_type]]
  if (is.null(cfg)) cli::cli_abort("Unknown feature type {feature_type}")
  cfg$data[[cfg$feature_column]]
}

get_geom_feature_column <- function(feature_type) {
  cfg <- ggautomap_global$maps[[feature_type]]
  if (is.null(cfg)) cli::cli_abort("Unknown feature type {feature_type}")
  cfg$feature_column
}

get_geometry <- function(feature_type) {
  cfg <- ggautomap_global$maps[[feature_type]]
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
  cfg <- ggautomap_global$maps[[feature_type]]
  if (is.null(cfg)) cli::cli_abort("Unknown feature type {feature_type}")
  cfg$aliases
}

get_outline <- function(feature_type) {
  cfg <- ggautomap_global$maps[[feature_type]]
  if (is.null(cfg)) cli::cli_abort("Unknown feature type {feature_type}")
  cfg$outline
}

#' Guess the feature type if it was missing
#'
#' @param feature_type Type of map feature. See [feature_types] for a list of
#'   registered types. If \code{NA}, the type is guessed based on the values in
#'   the data.
#' @param locations Character vector of location names in the data.
#' @param context Name of the calling function, for inclusion in error message
#'   if it's not possible to unambiguously guess the feature type.
resolve_feature_type <- function (feature_type, locations, context) {
  if (is.null(feature_type)) return(NULL)
  if (is.na(feature_type)) feature_type <- guess_feature_type(locations)
  validate_feature_type(context, feature_type)
  feature_type
}

guess_feature_type <- function (locations) {
  locations <- unique(locations)
  types <- feature_types()
  found <- sapply(types, function (ty) sum(locations %in% get_feature_names(ty)))
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

validate_feature_type <- function (context, feature_type = NULL) {
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
}

validate_location <- function(location, feature_type, source = "{.arg location}") {
  feature_names <- get_feature_names(feature_type)

  unknown_features <- setdiff(location, feature_names)
  if (length(unknown_features) > 0) {
    cli::cli_abort(c(
      paste0(source, " contains unexpected values"),
      "x" = "The unknown values are {unknown_features}",
      "i" = "Expected {feature_type} names like {head(feature_names, n = 3)}"
    ))
  }
  location
}
