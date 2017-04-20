#' Setup GRASS environment.
#'
#' This function sets the GRASS mapset to PERMANENT and sets its projection and extension.
#'
#' @param dem character; path to DEM.
#' @param sites character; path to sites.
#' @param epsg number; EPSG code for the spatial reference to be used
#'
#' @return Nothing, the GRASS mapset is set to PERMANENT,
#' projection is set to the one of the sites shape or to the epsg code provided, the extent of the region is set to the one of the dem.
#'
#' @note Either \code{sites} or \code{epsg} must be provided. A GRASS session must be initiated before, see  \code{\link[rgrass7]{initGRASS}}.
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}, Mira Kattwinkel, \email{mira.kattwinkel@@gmx.net}
#' @export
#'
#' @examples
#' \donttest{
#' initGRASS(gisBase = "/usr/lib/grass72/",
#'   home = tempdir(),
#'   override = TRUE)
#' dem_path <- system.file("extdata", "nc", "elev_ned_30m.tif", package = "openSTARS")
#' sites_path <- system.file("extdata", "nc", "sites_nc.shp", package = "openSTARS")
#' setup_grass_environment(dem = dem_path, epsg = 3358)
#' gmeta()
#' }
#'

setup_grass_environment <- function(dem, sites = NULL, epsg = NULL) {
  if (nchar(get.GIS_LOCK()) == 0)
    stop("GRASS not initialised. Please run initGRASS().")
  if (is.null(dem) | (is.null(sites) & is.null(epsg)))
    stop("DEM and either sites or epsg code are needed.")
  message("Setting up GRASS Environment...\n")

  # Set Projection to input file -------------------------
  execGRASS("g.mapset",
            flags = c("quiet"),
            parameters = list(
              mapset = "PERMANENT"))
  if(!is.null(sites)){
    execGRASS("g.proj",
              flags = c("c"),#, "quiet"),
              parameters = list(
                georef = sites
              ))
  } else {
    execGRASS("g.proj",
              flags = c("c"),#, "quiet"),
              parameters = list(
                epsg = epsg
              ))
  }

  # set Region -----------------
  # read raster to set region
  # MiKatt: flag "o": Override projection check. Necessary for Windows, otherwise DEM is not imported
  # MiKatt: it is necassary to set the region with g.region; using flag "e" when importing the dem does not work
  #         (r.hydrodem produces very huge raster)
  if(.Platform$OS.type == "windows"){
    execGRASS("r.in.gdal",
              flags = c("overwrite","quiet","o"),
              parameters = list(
                input = dem,
                output = "dem_temp"))
    message(paste("Windows does not recognize the projection of the DEM raster. ",
                  "It will be overwritten with the one of the observation sites (",
                  sites, "). Please verify that the projections match.", sep= ""))
  } else{
    execGRASS("r.in.gdal",
              flags = c("overwrite","quiet"),
              parameters = list(
                input = dem,
                output = "dem_temp"))
  }
  execGRASS("g.region",
            flags = c("quiet"),
            parameters = list(
              raster = "dem_temp"))

  # remove temporary dem file
  execGRASS("g.remove",
            flags = c("quiet", "f"),
            parameters = list(
              type = "raster",
              name = "dem_temp"
            ))
}
