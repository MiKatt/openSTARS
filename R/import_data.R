#' Import data into GRASS.
#'
#' This function load dem and sites data (both required) into the GRASS session.
#'
#' @import rgrass7
#'
#' @param dem character; path to DEM.
#' @param sites character; path to sites.
#' @param streams character, (optional); path to network shape.
#'  If available it can be burnt into DEM
#' @param ... other paths to raster data to import as predictors
#'  (currently not implemented)
#'
#' @return Nothing, the data is loaded into the GRASS session.
#' The DEM is stored as raster 'dem' and sites as vector 'sites_o' within GRASS.
#'
#' @note A GRASS session must be initiated before, see  \code{\link[rgrass7]{initGRASS}}.
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#'
#' @examples
#' \donttest{
#' library(rgrass7)
#' initGRASS(gisBase = "/usr/lib/grass70/",
#'   home = tempdir(),
#'   override = TRUE)
#' gmeta()
#' dem_path <- system.file("extdata", "nc", "elev_ned_30m.tif", package = "openSTARS")
#' sites_path <- system.file("extdata", "nc", "sites_nc.shp", package = "openSTARS")
#' import_data(dem = dem_path, sites = sites_path)
#'
#' dem <- readRAST('dem')
#' plot(dem)
#' }
#' 

import_data <- function(dem, sites, streams = NULL, ...) {
  if (nchar(get.GIS_LOCK()) == 0)
    stop('GRASS not initialised. Please run initGRASS().')
  if (is.null(dem) | is.null(sites))
    stop('DEM and sites are needed.')
  message('Setting up GRASS Environment...\n')

  # Set Projection to input file -------------------------
  # MiKatt: Make mapset user defined?
  # MiKatt: Not possible, g.proj gives error: "ERROR: You must select the PERMANENT mapset before updating the current location's projection (current mapset is <01_Test>)"
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
            flags = c("overwrite", "quiet"),
            parameters = list(
              input = dem,
              output = "dem"))
  execGRASS("g.region",
            flags = c('quiet'),
            parameters = list(
              raster = "dem"))

  # Import data -------------------
  message('Loading data into GRASS...\n')

  # reread raster with correct projection and extent
  execGRASS("r.in.gdal",
            flags = c("overwrite", "quiet"),
            parameters = list(
              input = dem,
              output = "dem"))
  # execGRASS('r.info',
  #           parameters = list(
  #             map = 'dem'
  #           ))

  # sites data
  execGRASS("v.in.ogr",
            flags = c("o", "overwrite", "quiet"),
            parameters = list(
              input = sites,
              output = "sites_o"))
  # execGRASS('v.info',
  #           parameters = list(
  #             map = 'sites_o'
  #           ))

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