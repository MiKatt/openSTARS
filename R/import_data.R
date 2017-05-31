#' Import data into GRASS.
#'
#' This function loads dem and sites data (both required) into the GRASS session.
#' Optionally, prediction sites and streams data can be loaded and the streams 
#' may be corrected by snapping to prevent lose ends.
#'
#' @param dem character; path to DEM raster file.
#' @param band integer (optional); defines which band is used
#' @param sites character or object; path to sites vector file or sp of sf object.
#' @param streams character (optional); path to network vector file.
#'  If available it can be burnt into DEM.
#' @param snap_streams boolean (optional); snap line ends.
#'  If TRUE line ends of the streams are snapped to the next feature if they are
#'   unconncted with threshold of 10 m using GRASS function v.clean.
#' @param pred_sites character vector (optional); paths to prediction sites 
#' vector files
#' @param predictor_maps character vector (optional); paths to raster data to 
#' import as predictors.
#' @param predictor_names character vector (optional); names for potential predictor
#' variables.
#'
#' @return Nothing, the data is loaded into the GRASS session (mapset PERMANENT).
#' The DEM is stored as raster 'dem', sites as vector 'sites_o', prediction sites
#' vector(s) using the original file names with an appended '_o' (without extension),
#' streams as vector 'streams_o' in the GRASS location. Additinally, predictor 
#' map raster(s) can be read in and are stroed in GRASS using either the 
#' original file names (without extension) or using the names provides in 
#' predictor_names. The latter option may be usefull if ArcGIS grid data 
#' (typically stored as 'grid_name/w001001.adf') are used.
#'.
#'
#' @note A GRASS session must be initiated before, see \code{\link[rgrass7]{initGRASS}}.
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com},  Mira Kattwinkel
#'  \email{mira.kattwinkel@@gmx.net}
#' @export
#'
#' @examples
#' \donttest{
#' initGRASS(gisBase = "/usr/lib/grass72/",
#'   home = tempdir(),
#'   override = TRUE)
#' gmeta()
#' dem_path <- system.file("extdata", "nc", "elev_ned_30m.tif", package = "openSTARS")
#' sites_path <- system.file("extdata", "nc", "sites_nc.shp", package = "openSTARS")
#' setup_grass_environment(dem = dem_path, sites = sites_path)
#' import_data(dem = dem_path, sites = sites_path)
#' dem <- readRAST('dem')
#' plot(dem)
#' }
#'

import_data <- function(dem, band = 1, sites, streams = NULL, snap_streams = FALSE, 
                        pred_sites = NULL, predictor_maps = NULL, predictor_names = NULL) {
  if (nchar(get.GIS_LOCK()) == 0)
    stop("GRASS not initialised. Please run initGRASS().")
  if (is.null(dem) | is.null(sites))
    stop("DEM and sites are needed.")

  # Import data -------------------
  message("Loading DEM into GRASS...\n")

  # import raster with correct extent
  # MiKatt: it is necassary to set the region with g.region in setup_grass_environment;
  #         using flag "e" when importing the dem does not work
  #         (r.hydrodem produces very huge raster)
  if(.Platform$OS.type == "windows"){
    execGRASS("r.in.gdal",
              flags = c("overwrite","quiet","o"),
              parameters = list(
                input = dem,
                band = band,
                output = "dem"),ignore.stderr=T)
  } else{
    execGRASS("r.in.gdal",
              flags = c("overwrite", "quiet"),
              parameters = list(
                input = dem,
                band = band,
                output = "dem"),ignore.stderr=T)
  }
  
  message("Loading sites into GRASS as sites_o ...\n")
  # sites data
  if(inherits(sites, 'Spatial')) {
    writeVECT(sites, "sites_o",  v.in.ogr_flags = c("o", "overwrite", "quiet"),
              ignore.stderr=T)
  } else if(inherits(sites, 'sf')) { 
    sites_sp <- as(sites, 'Spatial') # AS:  no method for sf yet imho
    writeVECT(sites_sp, "sites_o", v.in.ogr_flags = c("o", "overwrite", "quiet"),
              ignore.stderr=T)
  } else {
  execGRASS("v.in.ogr", flags = c("o", "overwrite", "quiet"),
            parameters = list(
              input = sites,
              output = "sites_o"),ignore.stderr=T)
  }
  
  # prediction sites data
  if (!is.null(pred_sites)) {
    pred_sites_names <- do.call(rbind,base::strsplit(sapply(pred_sites,basename,USE.NAMES=F), split="[.]"))[,1]
    if(any(pred_sites_names == "sites")){
      message("Prediction sites cannot be named 'sites'. Please rename.")
    } else{
      pred_sites_names <- paste0(pred_sites_names, "_o")
      message(paste0("Loading preditions sites into GRASS as ",
                     paste(pred_sites_names, collapse=", ", sep=""), " ...\n"))
      for(i in 1:length(pred_sites)){
        execGRASS("v.in.ogr",
                  flags = c("o", "overwrite", "quiet"),
                  parameters = list(
                    input = pred_sites[i],
                    output = pred_sites_names[i]),ignore.stderr=T)
      }
    }
  }
  

  # predictor raster maps
  if (!is.null(predictor_maps)) {
    if(is.null(predictor_names))
      predictor_names <- do.call(rbind,base::strsplit(sapply(predictor_maps,basename,USE.NAMES=F), split="[.]"))[,1]
    message(paste0("Loading predictior varibales into GRASS as ",paste(predictor_names, collapse = ", ", sep=""), " ...\n"))
    for(i in 1:length(predictor_names)){
      if(.Platform$OS.type == "windows"){
        execGRASS("r.in.gdal",
                  flags = c("overwrite","quiet","o"),
                  parameters = list(
                    input = predictor_maps[i],
                    output = predictor_names[i]),ignore.stderr=T)
      } else{
        execGRASS("r.in.gdal",
                  flags = c("overwrite", "quiet"),
                  parameters = list(
                    input = predictor_maps[i],
                    output = predictor_names[i]),ignore.stderr=T)
      }
    }
  }
  
  # streams data
  if (!is.null(streams)) {
    message("Loading streams into GRASS as streams_o  ...\n")
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
                flags = c("overwrite", "quiet"),
                parameters = list(
                  vector = "streams_oc,streams_o"))
      execGRASS("g.remove",
                flags = c("quiet", "f"),
                parameters = list(
                  type = "vector",
                  name = "streams_oc"
                ))
    }
  } else {
    message("No streams available, skipping.\n")
  }
}
