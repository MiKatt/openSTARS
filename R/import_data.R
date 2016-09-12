#' Import data into GRASS.
#'
#' This function loads dem and sites data (both required) into the GRASS session. 
#' Optionally, streams data is loaded and the streams may be corrected by 
#' snapping to prevent lose ends.
#'
#' @import rgrass7
#'
#' @param dem character; path to DEM raster file.
#' @param sites character; path to sites vector file.
#' @param streams character, (optional); path to network vector file.
#'  If available it can be burnt into DEM
#' @param snap_streams boolean, (optional); snap line ends.
#'  If TRUE line ends of the streams are snapped to the next feature if they are
#'   unconncted with threshold of 10 m using GRASS function v.clean.
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
#' setup_grass_environment(dem = dem_path, sites = sites_path)
#' import_data(dem = dem_path, sites = sites_path)
#'
#' dem <- readRAST('dem')
#' plot(dem)
#' }
#' 

import_data <- function(dem, sites, streams = NULL, snap_streams = FALSE, ...) {
  if (nchar(get.GIS_LOCK()) == 0)
    stop('GRASS not initialised. Please run initGRASS().')
  if (is.null(dem) | is.null(sites))
    stop('DEM and sites are needed.')
 
  # Import data -------------------
  message('Loading data into GRASS...\n')

  # reread raster with correct extent 
  if(.Platform$OS.type == "windows"){
    execGRASS("r.in.gdal",
              flags = c("overwrite","quiet","o"), 
              parameters = list(
                input = dem,
                output = "dem"),ignore.stderr=T)
  } else{
    execGRASS("r.in.gdal",
              flags = c("overwrite", "quiet"),
              parameters = list(
                input = dem,
                output = "dem"),ignore.stderr=T)
  }
  # sites data
  execGRASS("v.in.ogr",
            flags = c("o", "overwrite", "quiet"),
            parameters = list(
              input = sites,
              output = "sites_o"),ignore.stderr=T)

  # streams data
  if (!is.null(streams)) {
    execGRASS("v.in.ogr",
              flags = c("overwrite", "quiet"),
              parameters = list(
                input = streams,
                output = "streams_o"),ignore.stderr=T)
    # MiKatt: snapp line ends to next vertext to prevent loose ends/ unconnected streams and to build topography
    if(snap_streams){
      execGRASS("v.clean",
                flags=c("c","overwrite","quiet"),
                parameters = list(
                  input = "streams_o",
                  type = "line",
                  output = "streams_oc",
                  tool = "snap",
                  threshold = 10),ignore.stderr=T)
      execGRASS("g.copy",
                flags = c('overwrite', 'quiet'),
                parameters = list(
                  vector = 'streams_oc,streams_o'))
      execGRASS("g.remove",
                flags = c('quiet', 'f'),
                parameters = list(
                  type = 'vector',
                  name = 'streams_oc'
                ))
    }
  } else {
    message('No streams available, skipping.\n')
  }
}