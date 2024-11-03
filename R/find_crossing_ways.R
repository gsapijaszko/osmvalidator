#' find_crossing_ways() Searches for crossing and not connected (high)ways
#'
#' @importFrom sf st_crosses st_filter st_cast st_dimension
#' @importFrom dplyr rowwise mutate
#' @importFrom tidyr pivot_longer
#'
#' @param highways simple feature collection (class sf) with highways as LINESTRING
#' @param exclude list of types of highways to exclude, ex. c("proposed", "rest_area", "steps"), default NULL
#' @param exclude_bridges if bridges (and highways on any `layer`) has to be excluded, default TRUE
#' @param return_all_columns if all columns from input data set has to be returned, default FALSE. In this case only osm_id, group, highway, name, and geometry are returned.
#'
#' @returns set of highways which crosses but are not connected if any found, otherwise NULL
#' @usage find_crossing_ways(highways = NULL,
#'                           exclude = NULL,
#'                           exclude_bridges = TRUE,
#'                           return_all_columns = FALSE)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ways <- system.file("extdata/highways.gpkg", package = "osmvalidator") |>
#'   sf::st_read()
#'
#' find_crossing_ways(highways = ways,
#'                    exclude = c("proposed", "rest_area", "bus_stop", "steps"))
#' }
#'

find_crossing_ways <- function(highways = NULL, exclude = NULL, exclude_bridges = TRUE, return_all_columns = FALSE) {
  # to satisfy R CMD check
  #
  highway <- bridge <- layer <- area <- row.id <- col.id <- n <- value <- geometry_col <- NULL

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
      subset(!is.na("highway") & highway != "no") # https://wiki.openstreetmap.org/wiki/Tag:highway=no
  }

  if(!is.null(exclude)) {
    if(inherits(exclude, "character")) {
      highways <- highways |>
        subset(!highway %in% exclude)
    }
  }

  if(isTRUE(exclude_bridges)) {
    if("bridge" %in% colnames(highways)) {
      highways <- highways |>
        subset(is.na(bridge))
      if("layer" %in% colnames(highways)) {
        highways <- highways |>
          subset(layer != "-1" | is.na(layer))
      }
    }
  }

  # filter out the ways in form of areas
  if("area" %in% colnames(highways)) {
    highways <- highways |>
      subset(is.na(area))
  }

  # find crossing ways and not sharing the common nodes
  #
  message("Finding crossing ways..")
  topo <- suppressMessages(sf::st_crosses(highways, sparse = TRUE)) |>
    as.data.frame()  |>
    subset(row.id != col.id)

  if(nrow(topo) > 0 ) {

    # removing duplicates across rows and columns
    # https://stackoverflow.com/questions/77709120/remove-duplicate-rows-based-on-combinations-in-two-columns
    #
    #  topo <- topo |>
    #    dplyr::distinct(pmin(row.id, col.id), pmax(row.id, col.id), n, .keep_all = TRUE) |>
    #    subset(select = c(1:3))

    topo <- topo[!with(topo, mapply(\(...) c(min(...), max(...)), row.id, col.id)) |> t() |> duplicated(), ]
    message(paste(nrow(topo), "ways found."))

    message("Checking for common nodes, it might take while...")
    topo <- topo |>
      dplyr::rowwise() |>
      dplyr::mutate(n =
                      suppressWarnings(sf::st_cast(highways[row.id, ], to = "POINT")) |>
                      sf::st_filter(suppressWarnings(sf::st_cast(highways[col.id, ], to = "POINT"))) |>
                      nrow()
      )  |>
      subset(n == 0)
topo
    if(nrow(topo) > 0) {
      message(paste(nrow(topo), "crossing ways without connection found."))
      topo <- topo[!with(topo, mapply(\(...) c(min(...), max(...)), row.id, col.id)) |> t() |> duplicated(), ]
      topo <- topo |>
        as.data.frame() |>
        dplyr::mutate(group = as.integer(factor(row.id))) |>
        subset(select = -n) |>
        tidyr::pivot_longer(cols = c(row.id, col.id))

      groups <- topo$group
      topo <- highways[topo[, "value"][[1]], ]
      topo$group <- groups

      if(isFALSE(return_all_columns)) {
        geometry_col <- attr(topo, "sf_column")[[1]]
        topo <- topo |>
          subset(select = c("osm_id", "group", "highway", "name", geometry_col))
      }

    } else {
      message("No crossing ways without connection found.")
      topo <- NULL
    }
  } else {
    message("No crossing ways found.")
    topo <- NULL
  }

  return(topo)
}
