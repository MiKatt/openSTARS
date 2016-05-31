#' Import data into GRASS.
#'
#' @import rgrass7
#'
#' @param dem character; path to DEM.
#' @param sites character; path to sites.
#' @param stream character, (optional); path to network shape.
#'  If available it can be to burn into DEM
#' @param ... other paths to raster data to import as predictors
#'  (currently not implemented)
#'
#' @return Nothing, the data is loaded into the GRASS session.
#'
#' @note A GRASS session must be initiated before, see  \code{\link[rgrass7]{initGRASS}}.
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#'
import_data <- function(dem, sites, streams = NULL, ...) {
  if (nchar(get.GIS_LOCK()) == 0)
    stop('GRASS not initialised. Please run initGRASS().')
  if (is.null(dem) | is.null(sites))
    stop('DEM and sites are needed.')
  message('Setting up GRASS Environment...\n')

  # Set Projection to input file -------------------------
  currmapset <- execGRASS("g.gisenv",
                          parameters = list(
                            get = "MAPSET"),
                          intern = TRUE)
  execGRASS("g.mapset",
            flags = c('quiet'),
            parameters = list(
              mapset = "PERMANENT"))
  execGRASS("g.proj",
            flags = c("c", "quiet"),
            parameters = list(
              georef = sites
            ))

  # set Region -----------------
  # read raster to set region
  execGRASS("r.in.gdal",
            flags = c('o', "overwrite", "quiet"),
            parameters = list(
              input = dem,
              output = "dem"))
  execGRASS("g.region",
            flags = c('quiet'),
            parameters = list(
              raster = "dem"))

  # Import data -------------------
  message('Loading data into GRASS...\n')

  # reread raster with correct projection
  execGRASS("r.in.gdal",
            flags = c("overwrite", "quiet"),
            parameters = list(
              input = dem,
              output = "dem"))

  # sites data
  execGRASS("v.in.ogr",
            flags = c("overwrite", "quiet"),
            parameters = list(
              input = sites,
              output = "sites_o"))

  # streams data
  if (!is.null(streams)) {
    execGRASS("v.in.ogr",
              flags = c("overwrite", "quiet"),
              parameters = list(
                input = streams,
                output = "streams_o"))
  } else {
    message('No streams available, skipping.\n')
  }
}