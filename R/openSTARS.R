#' openSTARS: An open source implementation of the STARS toolbox.
#'
#' GRASS GIS 7.0 (or greater) with installed addons r.stream.basins, r.stream.distance and r.stream.order
#' is needed.
#'
#' @docType package
#' @name webchem
#' @examples
#' library(openSTARS)
#' # Initiate GRASS Sesion
#' initGRASS(gisBase = "/usr/lib/grass70/",
#'     home = tempdir(),
#'     override = TRUE)
#'
#' # Load files into GRASS
#' dem_path <- system.file("extdata", "nc", "elev_ned_30m.tif", package = "openSTARS")
#' sites_path <- system.file("extdata", "nc", "sites_nc.shp", package = "openSTARS")
#' import_data(dem = dem_path, sites = sites_path)
#' gmeta()
#'
#' # Derive streams from DEM
#' derive_streams()
#'
#' # Prepare edges
#' calc_edges()
#'
#' # Prepare site
#' calc_sites()
#'
#' # Calculate binary IDs
#' binaries <- calc_binary()
#'
#' # Plot data
#' dem <- readRAST('dem', ignore.stderr = TRUE)
#' sites <- readVECT('sites', ignore.stderr = TRUE)
#' edges <- readVECT('edges', ignore.stderr = TRUE)
#' plot(dem, col = terrain.colors(20))
#' lines(edges, col = 'blue')
#' cols <- colorRampPalette(c("blue", 'red'))(length(sites$value))[rank(sites$value)]
#' points(sites, pch = 16, col = cols)
#'
#' # Write SSN Folder
#' ssn_dir <- file.path(tempdir(), 'nc.ssn')
#' export_ssn(ssn_dir, binary = binaries)
#'
#' # Load into SSN-package
#' library(SSN)
#' ssn_obj <- importSSN(ssn_dir, o.write = TRUE)
#' plot(ssn_obj, 'value')
NULL


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