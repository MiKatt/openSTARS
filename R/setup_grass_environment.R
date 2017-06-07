#' Setup GRASS environment.
#'
#' This function sets the GRASS mapset to PERMANENT and sets its projection and extension.
#'
#' @param dem character; path to DEM.
#' @param sites character; path to sites or sp data object.
#' @param epsg number; EPSG code for the spatial reference to be used
#' @param proj4 (optional) proj4 string; character string of projection arguments
#'  
#'
#' @return Nothing, the GRASS mapset is set to PERMANENT,
#' projection is set to the one of the sites shape, to the proj4 string or to 
#' the epsg code provided, the extent of the region is set to the one of bounding
#' box of the dem.
#'
#' @note Either \code{sites}, \code{epsg} or \code{proj4} must be provided. Make
#' sure that all raster and vector files are in the same projection; it will be
#' overwritten without warning. A GRASS session must be initiated before, see
#' \code{\link[rgrass7]{initGRASS}}.
#' 
#' @author Mira Kattwinkel, \email{mira.kattwinkel@@gmx.net}
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

setup_grass_environment <- function(dem, sites = NULL, epsg = NULL, proj4 = NULL) {
  if (nchar(get.GIS_LOCK()) == 0)
    stop("GRASS not initialised. Please run initGRASS().")
  if (is.null(dem) | (is.null(sites) & is.null(epsg) & is.null(proj4)))
    stop("DEM and either sites, epsg code or porj4 string are needed.")
  message("Setting up GRASS Environment...\n")

  # Set Projection to input file -------------------------
  ## AS: Habe optionen hinzugefügt, dass Punkte auch SP*-Objekte aus R sein können
  execGRASS("g.mapset", flags = c("quiet"),
            parameters = list(
              mapset = "PERMANENT"))
  # MiKatt: shortend
  if(is.null(proj4)){
    if(inherits(sites, 'Spatial')) {
      proj4 <- sp::proj4string(sites)
    } ## MiKatt: exclude as long as under development
    # else if(inherits(sites, 'sf')) {
    #     ## AS: syntax might change in future
    #     proj4 <- sf::st_crs(sites)$proj4string
    # }
  }
  if(!is.null(proj4)){
    execGRASS("g.proj", flags = c("c"),
              parameters = list(
                proj4 = proj4
              ))
  } else if(!is.null(sites)){
    execGRASS("g.proj", flags = c("c"),
              parameters = list(
                georef = sites
              ))
  } else {
    execGRASS("g.proj",
              flags = c("c"),#, "quiet"),
              parameters = list(
                epsg = epsg
              ))
    # set proj4 from epsg for dem projection
    proj4 <- paste0("+init=epsg:", epsg)
  }

  # set Region -----------------
  
  # read raster to set region
  # MiKatt: flag "o": Override projection check. Necessary for Windows, otherwise DEM is not imported
  # MiKatt: it is necassary to set the region with g.region; using flag "e" when importing the dem does not work
  #         (r.hydrodem produces very huge raster)
  # if(.Platform$OS.type == "windows"){
  #   execGRASS("r.in.gdal",
  #             flags = c("overwrite","quiet","o"),
  #             parameters = list(
  #               input = dem,
  #               output = "dem_temp"))
  #   message(paste("Windows does not recognize the projection of the DEM raster. ",
  #                 "It will be overwritten with the one of the observation sites (",
  #                 sites, "). Please verify that the projections match.", sep= ""))
  # } else {
  #   execGRASS("r.in.gdal",
  #             flags = c("overwrite","quiet"),
  #             parameters = list(
  #               input = dem,
  #               output = "dem_temp"))
  # }
  
  ##AS: Anmerkungen: Windows-unix query wäre hier wahrscheinlich nicht mehr nötig?
  ##AS: Wenn doch, müsste man sie über writeVECT wrapen
  ##AS: Zusätzlich werden jetzt das raster u sp package verwendet, aber das sind eh standards
  # MiKatt changed to more readable names
  dem_raster <- raster::raster(dem)
  dem_extent <- raster::extent(dem_raster)
  dem_extent <- as(dem_extent, 'SpatialPolygons')
  dem_extent <- SpatialPolygonsDataFrame(dem_extent, data.frame(ID = 1))
  dem_res_x <- raster::xres(dem_raster)
  dem_res_y <- raster::yres(dem_raster)
  if(dem_res_x != dem_res_y)
    warning("North-south and east-west resolution of dem differ. Please check!")
  if (is.null(proj4)) {
    raster::projection(dem_extent) <- raster::projection(raster::raster(dem))
  } else {
    raster::projection(dem_extent) <- sp::CRS(proj4)
  }
  
  # write bounding box of dem
  writeVECT(SDF = dem_extent, vname = 'bbox_dem', driver = 'SQLite',
            v.in.ogr_flags = c("overwrite", "quiet"))
  execGRASS("g.region", flags = c("quiet"),
            parameters = list(
              vector = "bbox_dem",
              nsres = dem_res_y,
              ewres = dem_res_x
              ))
  # remove temporary dem file
  execGRASS("g.remove", flags = c("quiet", "f"),
            parameters = list(
              type = "vector",
              name = "bbox_dem"
            ))
  
  # execGRASS("g.region",
  #           flags = c("quiet"),
  #           parameters = list(
  #             raster = "dem_temp"))
  # 
  # # remove temporary dem file
  # execGRASS("g.remove",
  #           flags = c("quiet", "f"),
  #           parameters = list(
  #             type = "raster",
  #             name = "dem_temp"
  #           ))
}
