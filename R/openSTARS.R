#' openSTARS: An Open Source Implementation of the ArcGIS Toolbox STARS.
#'
#' openSTARS provides functions to prepare data so that it can be imported by
#' the \code{\link[SSN]{SSN}} package for spatial modelling on stream networks.
#' GRASS GIS 7.0 (or greater) with installed addons r.stream.basins,
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
#' initGRASS(gisBase = "/usr/lib/grass72/",
#'     home = tempdir(),
#'     override = TRUE)
#'
# Load files into GRASS
#' dem_path <- system.file("extdata", "nc", "elev_ned_30m.tif", package = "openSTARS")
#' sites_path <- system.file("extdata", "nc", "sites_nc.shp", package = "openSTARS")
#' setup_grass_environment(dem = dem_path, sites = sites_path)
#' import_data(dem = dem_path, sites = sites_path)
#' gmeta()
#'
#' # Derive streams from DEM
#' derive_streams(burn = 0, accum_threshold = 700, condition = TRUE, clean = TRUE)
#'
#' # Prepare edges
#' calc_edges()
#'
#' # Prepare site
#' calc_sites()
#' # Calculate H2OArea
#' calc_attributes_sites_exact()
#'
#' # Plot data
#' dem <- readRAST('dem', ignore.stderr = TRUE)
#' sites <- readVECT('sites', ignore.stderr = TRUE)
#' edges <- readVECT('edges', ignore.stderr = TRUE)
#' plot(dem, col = terrain.colors(20))
#' lines(edges, col = 'blue')
#' cols <- colorRampPalette(c("blue", 'red'))(length(sites$H2OArea))[rank(sites$H2OArea)]
#' points(sites, pch = 16, col = cols)
#'
#' # Write data to SSN Folder
#' ssn_dir <- file.path(tempdir(), 'nc.ssn')
#' export_ssn(ssn_dir)
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
                           "cat_small", "cumsum_cells", "cut_x", "cut_y", "dif",
                           "end_x", "end_y", "len", "Length", "move_stream",
                           "netID", "newlen","next_str", 
                           "non_null_cells", "OBJECTID", "offset", "pcat", 
                           "prev_str01", "prev_str02", "prev_str03", "rid", 
                           "stream", "total_area", "variable", "X1", "X2"))


#' Datasets shipped with openSTARS
#'
#' @name openSTARS_data
#'
#' @section elev_ned30m.tif:
#' South-West Wake county National Elevation Data 30m.
#' The data has been taken from the GRASS GIS North Carolina data set.
#' Source \url{https://grass.osgeo.org/download/sample-data/}.
#' @section sites_nc.shp:
#' Arbitrary sites along rivers in North Carolina.
NULL
