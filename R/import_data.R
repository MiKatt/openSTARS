#' Import data into 'GRASS.'
#'
#' This function loads a DEM (digital elevation model) and sites data (both required) into the 'GRASS' session.
#' Optionally, prediction sites and streams data can be loaded and the streams 
#' may be corrected by snapping to prevent lose ends. Likewise, potential predictor 
#' maps (raster or vector format) can be loaded.
#'
#' @param dem character; path to DEM (digital elevation model) raster file.
#' @param band integer (optional); defines which band of the dem to use
#' @param sites character string or object; path to sites vector file (ESRI shape) 
#'  or sp or sf data object.
#' @param streams character string or object (optional); path to network vector 
#'  file (ESRI shape) or sp or sf data object. If available this can be burnt into the DEM 
#'  in \code{\link{derive_streams}}
#' @param snap_streams boolean (optional); snap line ends.
#'  If TRUE line ends of the streams are snapped to the next feature if they are
#'   unconnected with threshold of 10 m using 'GRASS' function v.clean.
#' @param pred_sites character string vector or object(s) (optional); path(s) to prediction sites 
#'  vector files (ESRI shape) or sp or sf data object. 
#'  Different formats (i.e. path and objects) must not be mixed; more than one sf or sp
#'  object must be provided as a list, not concatenated with \code{c}.
#' @param predictor_raster character vector (optional); paths to raster data to 
#' import as predictors. 
#' @param predictor_r_names character string vector (optional); names for potential predictor
#'  variables in raster format; if not provided \code{perdictor_raster} is used.
#' @param predictor_vector character string vector of object(s) (optional); path(s)
#'  to vector data (ESRI shape) or sp or sf object names to import as predictors.
#'  Different formats (i.e. path and objects) must not be mixed; more than one sf or sp
#'  object must be provided as a list, not concatenated with \code{c}.
#' @param predictor_v_names character vector (optional); names for potential predictor
#'  variables in vector format ; if not provided \code{perdictor_vector} is used.
#'
#' @return Nothing, the data is loaded into the 'GRASS' session (mapset PERMANENT).
#' The DEM is stored as raster 'dem', sites as vector 'sites_o', prediction sites
#' as vector using the original file names with an appended '_o' (without extension),
#' streams as vector 'streams_o' in the 'GRASS' location. Additionally, predictor 
#' raster map(s) can be read in and are stored in 'GRASS' using either the 
#' original file names (without extension) or using the names provides in 
#' \code{predictor_r_names}. The latter option may be useful if ArcGIS grid data 
#' (typically stored as 'grid_name/w001001.adf') are used. Likewise, predictor
#' vector maps can be read in from Esri Shape file (given as the full file path)
#' or as sf or sp objects. Potential predictor data can also be read in later, e.g.
#' using GRASS commands \href{https://grass.osgeo.org/grass74/manuals/v.import.html}{v.import} 
#' or \href{https://grass.osgeo.org/grass74/manuals/r.in.gdal.html}{r.in.gdal}
#' (see examples below).
#' 
#' @details All vector data (sites, streams and potential predictors) is imported 
#' into the current location using \href{https://grass.osgeo.org/grass74/manuals/v.import.html}{v.import}.
#' Hence, if the projections does not match to the one of the DEM (which was used
#' to specify the location in \code{\link{setup_grass_environment}}) the maps 
#' are projected and imported on the fly.
#' All raster data are not transformed but it is assumed that they have the same 
#' projection as the current location. Hence, it is important to make sure that 
#' they all have indeed the same projection (and same cell size) and that the correct
#' one is set in \code{\link{setup_grass_environment}}. If this condition is not met,
#' the raster data should be propocessed before importing.
#' Use \code{\link{check_projection}} to compare the projection of a raster data set and
#' the one of the current location.
#' 
#' @note A GRASS session must be initiated before, see \code{\link[rgrass7]{initGRASS}}.
#' 
#' If sites, pred_sites and / or streams are sp objects it is important that they 
#' have a datum defined otherwise the import will not work. Hence, it is e.g. 
#' better to use proj4string = CRS("+proj=tmerc +lat_0=0 +lon_0=9 +k=1 +x_0=3500000 +y_0=0 +datum=potsdam +units=m +no_defs")
#' instead of proj4string = CRS("+proj=tmerc +lat_0=0 +lon_0=9 +k=1 +x_0=3500000 +y_0=0 +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7 +units=m +no_defs"))
#' when defining sp objects.
#' 
#' @author Eduard Szoecs, \email{eduardszoecs@@gmial.com},  Mira Kattwinkel
#'  \email{mira.kattwinkel@@gmx.net}
#' @export
#' 
#' @examples
#' \donttest{
#' if(.Platform$OS.type == "windows"){
#'   gisbase = "c:/Program Files/GRASS GIS 7.6.0"
#'   } else {
#'   gisbase = "/usr/lib/grass74/"
#'   }
#' initGRASS(gisBase = gisbase,
#'     home = tempdir(),
#'     override = TRUE)
#' 
#' # Load files into GRASS
#' dem_path <- system.file("extdata", "nc", "elev_ned_30m.tif", package = "openSTARS")
#' sites_path <- system.file("extdata", "nc", "sites_nc.shp", package = "openSTARS")
#' streams_path <- system.file("extdata", "nc", "streams.shp", package = "openSTARS")
#' preds_v_path <- system.file("extdata", "nc", "pointsources.shp", package = "openSTARS")
#' preds_r_path <- system.file("extdata", "nc", "landuse_r.tif", package = "openSTARS")
#'                  
#' setup_grass_environment(dem = dem_path)
#' import_data(dem = dem_path, sites = sites_path, streams = streams_path,
#'             predictor_vector = preds_v_path, predictor_raster = preds_r_path)
#' gmeta()
#' 
#' # Plot data
#' dem <- readRAST("dem", ignore.stderr = TRUE)
#' sites_orig <-  readVECT("sites_o", ignore.stderr = TRUE)
#' lu <- readRAST("landuse_r", ignore.stderr = TRUE)
#' # import additional vector data
#' fp <-  system.file("extdata", "nc", "pointsources.shp", package = "openSTARS")
#' execGRASS("v.import", flags = c("overwrite", "quiet"),
#' parameters = list(
#'   input = fp,
#'   output =  "psources",
#'   extent = "region"),  # to import into current regien
#'   intern = TRUE, ignore.stderr = TRUE)
#'   
#' #plot(dem, col = terrain.colors(20))
#' plot(dem, col = grey.colors(20))
#' points(sites_orig, pch = 4)
#' ps <- readVECT("psources")
#' points(ps, bg = "red", pch = 21, col = "grey", cex = 1.5)
#' 
#' # plot landuse data
#' library(raster)
#' op <- par()
#' par(xpd = FALSE)
#' plot(raster(lu), legend = FALSE, xaxt = "n", yaxt = "n", bty = "n",
#' col = c("red", "goldenrod", "green", "forestgreen","darkgreen", "blue", "lightblue"))
#' par(xpd = TRUE)
#' legend("bottom", cex = 0.75,
#'   legend = c("developed", "agriculture", "herbaceous", "shrubland", "forest", "water", "sediment"),
#'   fill = c("red", "goldenrod", "green", "forestgreen","darkgreen", "blue", "lightblue"),
#'   horiz = TRUE, inset = -0.175)
#' par <- op
#' }

import_data <- function(dem, band = 1, sites, streams = NULL, snap_streams = FALSE, 
                        pred_sites = NULL, predictor_raster = NULL, predictor_r_names = NULL,
                        predictor_vector = NULL, predictor_v_names = NULL) {
  if (nchar(get.GIS_LOCK()) == 0)
    stop("GRASS not initialised. Please run initGRASS().")
  if (is.null(dem) | is.null(sites))
    stop("DEM and sites are needed.")

  # Import data -------------------
  message("Loading DEM into GRASS as 'dem' ...")
  
  # import raster
  # MiKatt: it is necassary to set the region with g.region in setup_grass_environment;
  #         using flag "e" when importing the dem does not work
  #         (r.hydrodem produces very huge raster)
  execGRASS("r.in.gdal",
            flags = c("overwrite", "quiet", "o"), # overwrite projection check
            parameters = list(
              input = dem,
              band = band,
              output = "dem"),ignore.stderr=T)

  message("Loading sites into GRASS as 'sites_o' ...")
  # sites data
  # flag "-r": only current region
  # 20180216: not flag "o", because if projection is wrong I want to see an error
  import_vector_data(data = sites, name = "sites_o", proj_ref_obj = dem)
  
  # prediction sites data
  if (!is.null(pred_sites)) {
    if(is.character(pred_sites)){
     pred_sites_names <- do.call(rbind,base::strsplit(sapply(pred_sites,basename,USE.NAMES=F), split="[.]"))[,1]
    } else {
      if(length(class(pred_sites)) == 1 && class(pred_sites) == "list"){
        pred_sites_names <- paste("pred_sites", 1:length(pred_sites))
      } else{
        pred_sites_names <- "pred_sites"
      }
    }
    if(any(pred_sites_names == "sites")){
      message("Prediction sites cannot be named 'sites'. Please rename.")
    } else{
      pred_sites_names <- paste0(pred_sites_names, "_o")
      message(paste0("Loading preditions sites into GRASS as ",
                     paste("'", pred_sites_names, "'", collapse=", ", sep=""), " ..."))
      for(i in 1:length(pred_sites_names)){
        import_vector_data(data = pred_sites[i], name = pred_sites_names[i], proj_ref_obj = dem)
      }
    }
  }
  
  # predictor raster maps
  if (!is.null(predictor_raster)) {
    if(is.null(predictor_r_names)){}
      predictor_r_names <- do.call(rbind,base::strsplit(sapply(predictor_raster,basename,USE.NAMES=F), split="[.]"))[,1]
    #message(writeLines(strwrap(paste0("Loading raster predictor variables into GRASS as ",paste("'",predictor_r_names, "'", collapse = ", ", sep=""), " ..."),
    #         width = 80)))
    message(paste0("Loading raster predictor variables into GRASS as ",paste("'",predictor_r_names, "'", collapse = ", ", sep=""), " ..."))
    for(i in 1:length(predictor_r_names)){
        execGRASS("r.in.gdal",
                  flags = c("overwrite","quiet","o"),
                  parameters = list(
                    input = predictor_raster[i],
                    output = predictor_r_names[i]),ignore.stderr=T)
     }
  }
  
  # predictor vector maps
  if (!is.null(predictor_vector)) {
    if(is.null(predictor_v_names)){
      if(is.character(predictor_vector)){
        predictor_v_names <- do.call(rbind,base::strsplit(sapply(predictor_vector,basename,USE.NAMES=F), split="[.]"))[,1]
      } else {
        if(length(class(predictor_vector)) == 1 && class(predictor_vector) == "list"){
          predictor_v_names <- paste("predictor_v", 1:length(predictor_vector))
        } else{
          predictor_v_names <- "predictor_v"
        }
      }
    }
    #message(writeLines(strwrap(paste0("Loading vector predictor variables into GRASS as ",paste("'", predictor_v_names, "'", collapse = ", ", sep=""), " ..."),
    #        width = 80), con = stderr()), appendLF = TRUE)
    message(paste0("Loading vector predictor variables into GRASS as ",paste("'", predictor_v_names, "'", collapse = ", ", sep=""), " ..."))
    for(i in 1:length(predictor_v_names)){
      import_vector_data(data = predictor_vector[i], name = predictor_v_names[i], proj_ref_obj = dem)
    }
  }
  
  # streams data
  if (!is.null(streams)) {
    message("Loading streams into GRASS as 'streams_o'  ...")
    # flag "-r": only current region
    import_vector_data(data = streams, name = "streams_o", proj_ref_obj = dem)
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
      #sink("temp.txt")
      execGRASS("g.copy",
                flags = c("overwrite", "quiet"),
                parameters = list(
                  vector = "streams_oc,streams_o"), intern = TRUE, ignore.stderr = TRUE)
      #sink()
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
  try(unlink("temp.txt"), silent = TRUE)
}

#' Import vector data into GRASS.
#' 
#' Generic function to import vector data of various formats into the GRASS environment.
#' 
#' @param data character string or object; path to data vector file (shape), postgis 
#' data source name (dsn; see details), or sp or sf data object.
#' @param name string giving the base name of the vector data within the GRASS envrionment (i.e. output)
#' @param layer character string; default 1, particularly needed if data is a dsn for 
#' importing postgis data (see details)
#' @param proj_ref_obj character; path to a georeferenced data file to be used as reference; only
#'   used if \code{data} is an sf of sp object, then, the two projections are compared to find the
#'   correct way for importing; typically the dem raster file used in this project.
#' @param snap float; snapping threshold in map units. If != -1 (default) vertices are snapped to other vertices
#'   in this snapping distance during import. If used, the features are automatically cleaned afterwards 
#'   (see GRASS tools \href{https://grass.osgeo.org/grass74/manuals/v.import.html}{v.import}  
#'   and \href{https://grass.osgeo.org/grass74/manuals/v.clean.html}{v.clean} )
#' 
#' @return Nothing.
#' 
#' @details For importing data from Postgis, all data base credentials must be supplied 
#'  in \code{data} and the correct \code{layer} and, if the table containing the polygons
#'  are in a specific schema also that one (see example)
#'  
#'@examples 
#'# import data from Postgis
#'\dontrun{
#' import_vector_data(data = "PG: 'pgname=postgit_DB', 'host=123.45.67.890', 
#' 'port='1234', 'user=username', 'password=password'",
#'  name = "forest", layer = "landuse_schema.forest")
#'}
#' @export
#' 
#' @author Mira Kattwinkel, \email{mira.kattwinkel@@gmx.net}

import_vector_data <- function(data, name, layer = NULL, proj_ref_obj = NULL, snap = -1){
  # flag "-r": only current region
  import_flag <- TRUE
  if(inherits(data, 'sf')){
    data <- as(data, 'Spatial')
  }
  if(inherits(data, 'Spatial')) {
    if(!is.null(proj_ref_obj)){
      proj_data <- execGRASS("g.proj", flags = c("j", "f"),
                             parameters = list(
                               proj4 = proj4string(data)
                             ), intern = TRUE)[1] # sites_in@proj4string@projargs does not work!
      proj_ref <- execGRASS("g.proj", flags = c("j", "f"),
                            parameters = list(
                              georef = proj_ref_obj
                            ), intern = TRUE, ignore.stderr = TRUE)
      if(identical(proj_ref, proj_data)) {
        writeVECT(data, name,  v.in.ogr_flags = c("overwrite", "quiet", "r", "o"), # "o": overwrite projection check
                  ignore.stderr=TRUE)
        import_flag <- FALSE
      }
    }
    if(import_flag) {
        rgdal::writeOGR(obj = data, dsn = tempdir(), layer = name, driver="ESRI Shapefile", overwrite_layer = TRUE)
        data <- file.path(tempdir(), paste0(name, ".shp"))
      }
  } 
  if(import_flag) {
    # gives error on Linux if projection is not identical but it works!
    # 'intern' and 'ignore.stderr' to suppress error messages
    if(is.null(layer)){
      execGRASS("v.import", flags = c("overwrite", "quiet"),
                parameters = list(
                  input = data,
                  output =  name,
                  snap = snap,
                  extent = "region"),  # to import into current region (= flags("r") in v.in.ogr)
                intern = TRUE, ignore.stderr = TRUE)      
    } else {
      execGRASS("v.import", flags = c("overwrite", "quiet"),
                parameters = list(
                  input = data,
                  layer = layer,
                  output =  name,
                  snap = snap,
                  extent = "region"),  # to import into current region (= flags("r") in v.in.ogr)
                intern = TRUE, ignore.stderr = TRUE)
    }
    if(snap != -1){
      execGRASS("v.clean", flags = c("c", "quiet"),
                parameters = list(
                  input = name,
                  output = paste0(name, "_c"),
                  tool = "break,rmdupl,rmsa"
                ))
      execGRASS("v.copy", flags = c("overwrite", "quiet"),
                parameters = list(
                  vector(paste0(name, "_c"), name)
                ))
      execGRASS("g.remove", flags = c("f", "quiet"),
                parameters = list(
                  type = "vector",
                  name =  paste0(name, "_c")
                ))
    }
    if(file.exists(file.path(tempdir(), paste0(name, ".shp")))){
      invisible(file.remove(file.path(tempdir(), list.files(path = tempdir(), pattern = paste0(name, ".")))))
    }
  }
}


#' Compare projection raster data to the one of the current GRASS location.
#' 
#' @param path character string vector; path raster data file(s)
#' 
#' @return Nothing.
#' 
#' @details Prints out a table of the PROJ.4 elements of the projection
#'   information of the current GRASS location and of the raster file(s) as
#'   well as one columns for each comparison indicating the differences. 
#'   Based on this information it can be decided if the data can be read
#'   into GRASS (\code{\link{import_data}}) without prior processing, i.e.
#'   if all raster data are of the same projection.
#'  
#' @export
#' 
#' @author Mira Kattwinkel, \email{mira.kattwinkel@@gmx.net}

check_projection <- function(path){
    loc_proj <- execGRASS("g.proj", flags = c("j"),
                        intern = TRUE, ignore.stderr = TRUE)
  comp <- loc_proj
  n <- length(path)
  
  for(i in 1:n){
    rast_proj <- execGRASS("g.proj", flags = c("j"),
                          parameters = list(
                            georef = path[i]
                          ), intern = TRUE)
    comp <- cbind(comp, rast_proj)
    comp <- cbind(comp, comp[,1] == comp[,2*i])
  }
  
  fnames <- sapply(1:n, function(x) basename(path[x]))
  cnames <- paste0("comp", 1:n)
  colnames(comp) <- c("GRASS project", c(sapply(1:n, function(x) c(fnames[x], cnames[x]))))
  print(comp)
}