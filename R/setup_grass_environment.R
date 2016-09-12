#' Setup GRASS environment.
#'
#' This function sets the GRASS mapset to PERMANENT, its projection and extension.
#'
#' @import rgrass7
#'
#' @param dem character; path to DEM.
#' @param sites character; path to sites.
#'
#' @return Nothing, the GRASS mapset is set to PERMANENT, 
#' projection is set to the one of the sites shape, the extent of the region is set to the one of the dem
#'
#' @note A GRASS session must be initiated before, see  \code{\link[rgrass7]{initGRASS}}.
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}, Mira Kattwinkel, \email{mira.kattwinkel@@gmx.net}
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
#' }
#' 

setup_grass_environment <- function(dem, sites) {
  if (nchar(get.GIS_LOCK()) == 0)
    stop('GRASS not initialised. Please run initGRASS().')
  if (is.null(dem) | is.null(sites))
    stop('DEM and sites are needed.')
  message('Setting up GRASS Environment...\n')

  # Set Projection to input file -------------------------
   execGRASS("g.mapset",
            flags = c('quiet'),
            parameters = list(
            mapset = "PERMANENT"))
  execGRASS("g.proj",
            flags = c("c"),#, "quiet"),
            parameters = list(
            georef = sites
            ))
 
  # set Region -----------------
  # read raster to set region
  # MiKatt: flag "o": Override projection check. Necessary for Windows, otherwise DEM is not imported
  if(.Platform$OS.type == "windows"){
    execGRASS("r.in.gdal",
              flags = c("overwrite","quiet","o"), 
              parameters = list(
                input = dem,
                output = "dem_temp"))
    message(paste("Windows does not recognize the projecion of the dem raster. It will be overwritten with the one of \n",sites,".\n
                   Please verify that they are identical.",sep=""))
  } else{
    execGRASS("r.in.gdal",
              flags = c("overwrite","quiet"), 
              parameters = list(
                input = dem,
                output = "dem_temp"))
  }
  execGRASS("g.region",
            flags = c('quiet'),
            parameters = list(
            raster = "dem_temp"))

  # remove temporary dem file
  execGRASS("g.remove",
            flags = c('quiet', 'f'),
            parameters = list(
              type = 'raster',
              name = 'dem_temp'
            ))
}