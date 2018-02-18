#' Import data into 'GRASS.'
#'
#' This function loads dem and sites data (both required) into the 'GRASS' session.
#' Optionally, prediction sites and streams data can be loaded and the streams 
#' may be corrected by snapping to prevent lose ends.
#'
#' @param dem character; path to DEM (digital elevation model) raster file.
#' @param band integer (optional); defines which band is used
#' @param sites character string or object; path to sites vector file (shape) 
#' or sp data object.
#' @param streams character string or object (optional); path to network vector 
#' file (shape) or sp data object. If available this can be burnt into the DEM 
#' in \code{\link{derive_streams}}
#' @param snap_streams boolean (optional); snap line ends.
#'  If TRUE line ends of the streams are snapped to the next feature if they are
#'   unconnected with threshold of 10 m using 'GRASS' function v.clean.
#' @param pred_sites character vector (optional); paths to prediction sites 
#' vector files
#' @param predictor_raster character vector (optional); paths to raster data to 
#' import as predictors.
#' @param predictor_r_names character vector (optional); names for potential predictor
#' variables.
#'
#' @return Nothing, the data is loaded into the 'GRASS' session (mapset PERMANENT).
#' The DEM is stored as raster 'dem', sites as vector 'sites_o', prediction sites
#' vector(s) using the original file names with an appended '_o' (without extension),
#' streams as vector 'streams_o' in the 'GRASS' location. Additionally, predictor 
#' raster map(s) can be read in and are stored in 'GRASS' using either the 
#' original file names (without extension) or using the names provides in 
#' predictor_r_names. The latter option may be useful if ArcGIS grid data 
#' (typically stored as 'grid_name/w001001.adf') are used.
#' 
#' @details All vector data (sites and streams) is imported into the current
#' location using \href{https://grass.osgeo.org/grass74/manuals/v.import.html}{v.import}.
#' Hence, if the projections does not match to the one of the dem (which was used
#' to specify the location in \code{\link{setup_grass_environment}}) the maps 
#' are imported on the fly.
#' 
#' @note A GRASS session must be initiated before, see \code{\link[rgrass7]{initGRASS}}.
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com},  Mira Kattwinkel
#'  \email{mira.kattwinkel@@gmx.net}
#' @export
#' 
#' @examples
#' \donttest{
#' # Initiate GRASS session
#' if(.Platform$OS.type == "windows"){
#'   gisbase = "c:/Program Files/GRASS GIS 7.2.0"
#'   } else {
#'   gisbase = "/usr/lib/grass72/"
#'   }
#' initGRASS(gisBase = gisbase,
#'     home = tempdir(),
#'     override = TRUE)
#'
# Load files into GRASS
#' dem_path <- system.file("extdata", "nc", "elev_ned_30m.tif", package = "openSTARS")
#' sites_path <- system.file("extdata", "nc", "sites_nc.shp", package = "openSTARS")
#' setup_grass_environment(dem = dem_path)
#' import_data(dem = dem_path, sites = sites_path)
#' gmeta()
#' dem <- readRAST('dem')
#' plot(dem)
#' }

import_data <- function(dem, band = 1, sites, streams = NULL, snap_streams = FALSE, 
                        pred_sites = NULL, predictor_raster = NULL, predictor_r_names = NULL) {
  if (nchar(get.GIS_LOCK()) == 0)
    stop("GRASS not initialised. Please run initGRASS().")
  if (is.null(dem) | is.null(sites))
    stop("DEM and sites are needed.")
  
  # Import data -------------------
  message("Loading DEM into GRASS...\n")
  
  # import raster
  # MiKatt: it is necassary to set the region with g.region in setup_grass_environment;
  #         using flag "e" when importing the dem does not work
  #         (r.hydrodem produces very huge raster)
  # 20180215: not necessary?
  # if(.Platform$OS.type == "windows"){
  #   execGRASS("r.in.gdal",
  #             flags = c("overwrite","quiet","o"),
  #             parameters = list(
  #               input = dem,
  #               band = band,
  #               output = "dem"),ignore.stderr=T)
  # } else{
  execGRASS("r.in.gdal",
            flags = c("overwrite", "quiet"),
            parameters = list(
              input = dem,
              band = band,
              output = "dem"),ignore.stderr=T)
  #}
  
  message("Loading sites into GRASS as sites_o ...\n")
  # sites data
  # flag "-r": only current region
  # 20180216: not flag "o", because if projection is wrong I want to see an error
  if(inherits(sites, 'Spatial')) {
    writeVECT(sites, "sites_o",  v.in.ogr_flags = c("overwrite", "quiet", "r"),
              ignore.stderr=T)
    #} # MiKatt: exclude as long as under development
    # else if(inherits(sites, 'sf')) { 
    #   sites_sp <- as(sites, 'Spatial') # AS:  no method for sf yet imho
    #   writeVECT(sites_sp, "sites_o", v.in.ogr_flags = c("o", "overwrite", "quiet"),
    #             ignore.stderr=T)
  } else {
    # a <- execGRASS("v.in.ogr", flags = c("j"),
    #           parameters = list(
    #             input = sites,
    #             output = "sites_o"), intern = T)
    # 
    # execGRASS("v.in.ogr", flags = c("overwrite", "quiet", "r"), #"o", 
    #           parameters = list(
    #             input = sites,
    #             output = "sites_o"),ignore.stderr=T)
    execGRASS("v.import", flags = c("overwrite", "quiet"),
              parameters = list(
                input = sites,
                output = "sites_o",
                extent = "region"))
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
        # execGRASS("v.in.ogr",
        #           flags = c("o", "overwrite", "quiet"),
        #           parameters = list(
        #             input = pred_sites[i],
        #             output = pred_sites_names[i]),ignore.stderr=T)
        execGRASS("v.import",
                  flags = c("overwrite", "quiet"),
                  parameters = list(
                    input = pred_sites[i],
                    output = pred_sites_names[i],
                    extent = "region"))
      }
    }
  }
  
  
  # predictor raster maps
  if (!is.null(predictor_raster)) {
    if(is.null(predictor_r_names))
      predictor_r_names <- do.call(rbind,base::strsplit(sapply(predictor_raster,basename,USE.NAMES=F), split="[.]"))[,1]
    message(paste0("Loading predictior varibales into GRASS as ",paste(predictor_r_names, collapse = ", ", sep=""), " ...\n"))
    for(i in 1:length(predictor_r_names)){
      if(.Platform$OS.type == "windows"){
        execGRASS("r.in.gdal",
                  flags = c("overwrite","quiet","o"),
                  parameters = list(
                    input = predictor_raster[i],
                    output = predictor_r_names[i]),ignore.stderr=T)
      } else{
        execGRASS("r.in.gdal",
                  flags = c("overwrite", "quiet"),
                  parameters = list(
                    input = predictor_raster[i],
                    output = predictor_r_names[i]),ignore.stderr=T)
      }
    }
  }
  
  # streams data
  if (!is.null(streams)) {
    message("Loading streams into GRASS as streams_o  ...\n")
    # flag "-r": only current region
    
    if(inherits(streams, 'Spatial')) {
      writeVECT(streams, "streams_o",  v.in.ogr_flags = c("o", "overwrite", "quiet", "r"),
                ignore.stderr=T)
    } else {
      # execGRASS("v.in.ogr",
      #           flags = c("overwrite", "quiet", "r"),
      #           parameters = list(
      #             input = streams,
      #             output = "streams_o"),ignore.stderr=T)
      execGRASS("v.import",
                flags = c("overwrite", "quiet"),
                parameters = list(
                  input = streams,
                  output = "streams_o",
                  extent = "region"))
    }
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
