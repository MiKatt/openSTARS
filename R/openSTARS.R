#' openSTARS: An Open Source Implementation of the 'ArcGIS' Toolbox 'STARS'.
#'
#' openSTARS provides functions to prepare data so that it can be imported by
#' the \code{\link[SSN]{SSN}} package for spatial modelling on stream networks.
#' 'GRASS GIS 7.0' (or greater) with installed addons r.stream.basins,
#' r.stream.distance, r.stream.order, and r.hydrodem is needed.
#'
#' 
#' @import rgrass7
#' @import data.table
#' 
#' @docType package
#' @name openSTARS
#'
#' @examples
#' \donttest{
#' if(.Platform$OS.type == "windows"){
#'   gisbase = "c:/Program Files/GRASS GIS 7.4.0"
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
#' 
#' setup_grass_environment(dem = dem_path)
#' import_data(dem = dem_path, sites = sites_path, streams = streams_path,
#'             predictor_vector = preds_v_path, predictor_raster = preds_r_path)
#' gmeta()
#' 
#' # Derive streams from DEM
#' # burn in 'streams' 10 meters
#' derive_streams(burn = 10, accum_threshold = 700, condition = TRUE, clean = TRUE)
#' 
#' # Check and correct complex junctions (there are no complex juctions in this
#' # example date set; set accum_threshold in derive_streams to a smaller value
#' # to create complex juctions)
#' cj <- check_compl_junctions()
#' if(cj){
#'   correct_compl_junctions()
#' }
#' 
#' # calculate slope as potential predictor
#' execGRASS("r.slope.aspect", flags = c("overwrite","quiet"),
#' parameters = list(
#'   elevation = "dem",
#'     slope = "slope"
#'     ))
#' 
#' # Prepare edges
#' calc_edges()
#' calc_attributes_edges(input_raster = c("slope", "landuse_r"), 
#'                       stat_rast = c("max", "percent"), 
#'                       attr_name_rast = c("maxSlo", "luse"),
#'                       input_vector = "pointsources", stat_vect = "count",
#'                       attr_name_vect = "psource")
#' 
#' # Prepare site
#' calc_sites()
#' 
#' # Usually, only one of the following methods is needed. The exact one takes
#' # much longer to run
#' # approximate potential predictor variables for each site based on edge values
#' calc_attributes_sites_approx(input_attr_name = c("maxSlo", "luse_1", "luse_2", 
#'                                                  "luse_3", "luse_4", "luse_5", 
#'                                                  "luse_6", "luse_7"),
#'                              output_attr_name = c("maxSloA","luse1A", "luse2A", 
#'                                                   "luse_3", "luse4A", "luse5A", 
#'                                                   "luse6A", "luse7A"),
#'                              stat = c("max", rep("percent", 7)))
#' 
#' # exact potential predictor variables for each site based on catchments
#' calc_attributes_sites_exact(input_raster = c("slope", "landuse_r"),
#'                             attr_name_rast = c("maxSloEx", "luseE"), 
#'                             stat_rast = c("max", "percent"))
#' 
#' # Plot data
#' dem <- readRAST("dem", ignore.stderr = TRUE)
#' sites <- readVECT("sites", ignore.stderr = TRUE)
#' sites_orig <-  readVECT("sites_o", ignore.stderr = TRUE)
#' edges <- readVECT("edges", ignore.stderr = TRUE)
#' plot(dem, col = terrain.colors(20))
#' lines(edges, col = "blue")
#' points(sites_orig, pch = 4)
#' cols <- colorRampPalette(c("blue", "red"))(length(sites$H2OArea))[rank(sites$H2OArea)]
#' points(sites, pch = 16, col = cols)
#' 
#' # Write data to SSN Folder
#' ssn_dir <- file.path(tempdir(), "nc.ssn")
#' export_ssn(ssn_dir, delete_directory = TRUE)
#' 
#' # Check if all files are ok
#' library(SSN)
#' check_ssn(ssn_dir)
#' 
#' # Load into SSN-package
#' ssn_obj <- importSSN(ssn_dir, o.write = TRUE)
#' print(ssn_obj)
#' }
#' 
#' #Datasets shipped with openSTARS
#'
#' @name openSTARS_data
#' All data has been taken from the GRASS GIS North Carolina data set.
#' Source \url{https://grass.osgeo.org/download/sample-data/} or artificially
#' created.
#' 
#' @section elev_ned30m.tif:
#' South-West Wake county National Elevation Data 30m.
#' @section sites_nc.shp:
#' Arbitrary sites along rivers in North Carolina. 
#' @section streams.shp:
#' Rivers in North Carolina.
#' @section geology.shp:
#' Geological data.
#' @section landuse_r.tif:
#' Land use date in North Carolina.
#' @section  lakes.shp:
#' Artificial lakes (not at topologically correct locations)
#' @section pointsources.shp:
#' Artificial point sources.
NULL