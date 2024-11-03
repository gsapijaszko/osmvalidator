#' find_unconnected_ways() Searches for unconnected solitary ways
#'
#' @importFrom sf st_dimension st_intersects
#' @importFrom tidyr pivot_longer
#'
#' @param highways simple feature collection (class sf) with highways as LINESTRING
#' @param exclude list of types of highways to exclude, ex. c("proposed", "rest_area", "steps"), default NULL
#' @param return_all_columns if all columns from input data set has to be returned, default FALSE. In this case only osm_id, highway, name, and geometry are returned.
#'
#' @returns set of unconected highways, otherwise NULL
#'
#' @usage find_unconnected_ways(highways = NULL, exclude = NULL, return_all_columns = FALSE)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ways <- system.file("extdata/highways.gpkg", package = "osmvalidator") |>
#'   sf::st_read()
#'
#' find_unconnected_ways(ways)
#'
#' }
#'

find_unconnected_ways <- function(highways = NULL, exclude = NULL, return_all_columns = FALSE) {
  # to satisfy R CMD check
  #
  highway <- area <- row.id <- col.id <- n <- value <- geometry_col <- NULL

  if(is.null(highways)) {
    stop(message("Please provide a set of highways."))
  }

  if(!inherits(highways, "sf")) {
    stop(message("Highways has to be a simple feature collection of class sf."))
  }

  # TODO - check if LINESTRING, ev. cast to
  if(any(sf::st_dimension(highways) != 1)) {
    stop(message("Highways has to be LINESTRING."))
  }

  if("highway" %in% colnames(highways)) {
    highways <- highways |>
      subset(!is.na("highway"))
  }

  if(!is.null(exclude)) {
    if(inherits(exclude, "character")) {
      highways <- highways |>
        subset(!highway %in% exclude)
    }
  }

  # filter out the ways in form of areas
  if("area" %in% colnames(highways)) {
    highways <- highways |>
      subset(is.na(area))
  }

  # find all which intersects
  i <- sf::st_intersects(highways, sparse = TRUE) |>
    as.data.frame() |>
    subset(row.id != col.id) |>
    tidyr::pivot_longer(cols = c(row.id, col.id)) |>
    subset(select = value) |>
    unique()

  highways <- highways |>
    subset(!rownames(highways) %in% i$value)

  if(nrow(highways) == 0L) {
    message("No disconnected highways found.")
    highways <- NULL
  } else {
    message(paste(nrow(highways), "disconnected ways found."))
    if(isFALSE(return_all_columns)) {
      geometry_col <- attr(highways, "sf_column")[[1]]
      highways <- highways |>
        subset(select = c("osm_id", "highway", "name", geometry_col))
    }
  }
  return(highways)
}
