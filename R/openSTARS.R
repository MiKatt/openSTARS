#' openSTARS: An Open Source Implementation of the 'ArcGIS' Toolbox 'STARS'.
#'
#' openSTARS provides functions to prepare data so that it can be imported by
#' the \code{\link[SSN]{SSN}} package for spatial modelling on stream networks.
#' 'GRASS GIS 7.0' (or greater) with installed addons r.stream.basins,
#' r.stream.distance, r.stream.order, and r.hydrodem is needed.
#'
#' @import rgrass7
#' @import data.table
#' 
#' @docType package
#' @name openSTARS
#'
#' @examples
#' \donttest{
#' # Initiate GRASS session
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
#' preds_path <- c(system.file("extdata", "nc", "landuse.shp", package = "openSTARS"),
#'                 system.file("extdata", "nc", "pointsources.shp", package = "openSTARS"))
#' setup_grass_environment(dem = dem_path)
#' import_data(dem = dem_path, sites = sites_path, predictor_vector = preds_path)
#' gmeta()
#'
#' # Derive streams from DEM
#' derive_streams(burn = 0, accum_threshold = 700, condition = TRUE, clean = TRUE)
#'
#' # Check and correct complex junctions (there are no complex juctions in this 
#' # example date set)
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
#' calc_attributes_edges(input_raster = "slope", stat_rast = "max", attr_name_rast = "maxSlo",
#'   input_vector = c("landuse", "pointsources"), stat_vect = c("percent", "count"), 
#'   attr_name_vect = c("landuse", "psource"))
#'
#' # Prepare site
#' calc_sites()
#' 
#' # Usually, only one of the following methods is needed. The exact one takes
#' # much longer to run
#' # approximate potential predictor variables for each site based on edge values
#' calc_attributes_sites_approx(input_attr_name = c("maxSlo", "agri", "forest", "grass", "urban"), 
#'   output_attr_name = c("maxSloA", "agriA", "forestA", "grassA", "urbanA"),
#'   stat = c("max", rep("percent", 4)))
#' 
#' # exact potential predictor variables for each site based on catchments
#' calc_attributes_sites_exact(input_raster = "slope", attr_name_rast = "maxSloEx", stat_rast = "max",
#'   input_vector = "landuse", attr_name_vect = "landuse", stat_vect = "percent")
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
#' }

# define column names of data.tables as global variables to prevent a NOTE in
# R CMD check 'no visible binding for global variable'.
if(getRversion() >= "2.15.1")
  utils::globalVariables(c(".", "area", "binaryID", "cat_", "cat_large",
                           "cat_small", "cumsum_cells", "cut_stream", "cut_stream_prev", 
                           "cut_x", "cut_y", "dif",
                           "edge_cat", "end_x", "end_y", "H2OArea", "len", 
                           "Length", "move_stream", "netID", "newlen","next_str", 
                           "non_null_cells", "OBJECTID", "offset", "pcat", 
                           "prev_str01", "prev_str02", "prev_str03", "prev_str04", "rcaArea","rid", 
                           "stream", "total_area", "variable", "X1", "X2"))

  

#' Datasets shipped with openSTARS
#'
#' @name openSTARS_data
#'
#' @section elev_ned30m.tif:
#' South-West Wake county National Elevation Data 30m.
#' The data has been taken from the GRASS GIS North Carolina data set.
#' Source \url{https://grass.osgeo.org/download/sample-data/}.
#' @section  lakes.shp:
#' Artificial lakes (not at topologically correct locations).
#' @section sites_nc.shp:
#' Arbitrary sites along rivers in North Carolina.
#' @section landuse.shp
#' Artificial landuse data.
#' @section pointsources.shp
#' #' Artificial point sources.
NULL
