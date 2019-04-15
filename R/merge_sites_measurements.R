#'Merge a table with measurements to the sites.
#'
#'@description After all processing steps are done and before exporting as an SSN object
#'  measurements can be added to the site map. They can contain multiple parameters and
#'  repeated measurements at the same site.
#'
#'@param measurements character string, data.table or data.frame object; 
#'  path to table data containing the data or a data.table or data.frame object
#'@param site_id character string; columns name that gives the unique name of the
#'  site. Must be identical in both the sites vector object and the table of 
#'  measurements
#'@param all_sites locical; should sites without measurments be preserved (default FALSE)
#'@param ... additional agruments to read.table in case \code{measuremtes} is a file path
#' to table data
#'  
#'@details Measurements are merged to the sites objects based on \code{site_id}. If
#'  there are repeated measurements, point features are dublicated and the 'pid' of the 
#'  sites is updated accoringly to be unique. 
#'
#'@author Mira Kattwinkel \email{mira.kattwinkel@@gmx.net}
#'
#'@export
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
#' setup_grass_environment(dem = dem_path)
#' import_data(dem = dem_path, sites = sites_path)
#' gmeta()
#'
#' # Derive streams from DEM
#' derive_streams(burn = 0, accum_threshold = 700, condition = TRUE, clean = TRUE)
#' 
#' # Check and correct complex junctions (there are no complex confluences in this
#' # example date set)
#' cj <- check_compl_confluences()
#' if(cj){
#'   correct_compl_confluences()
#' }
#'
#' # Prepare edges
#' calc_edges()
#'
#' # Prepare site
#' calc_sites()
#' # Plot data
#' dem <- readRAST('dem', ignore.stderr = TRUE)
#' edges <- readVECT('edges', ignore.stderr = TRUE)
#' sites <- readVECT('sites', ignore.stderr = TRUE)
#' sites_o <- readVECT('sites_o', ignore.stderr = TRUE)
#' plot(dem, col = terrain.colors(20),axes = TRUE)
#' lines(edges, col = 'blue')
#' points(sites, pch = 4)
#' points(sites_o, pch = 1)
#' legend("topright", pch = c(1, 4), legend = c("original", "corrected"))
#'}


merge_sites_measurements <- function(dat, site_id, all_sites = FALSE, ...) {
  
  if(!is.data.frame(dat) & !is.data.table()){
    d <- try(dat <- read.table(dat, header = T, stringAsFactor = FALSE, ...))
    if(class(d) == "try-error")
      stop("'dat' must contain a valid path name to table data.")
  } 
  
  if(!site_id %in% colnames(dat)){
    stop(writeLines(strwrap(paste0("'site_id' (", site_id, ") must contain a valid colum name in 'dat'. Options are: ", 
                                   paste0(colnames(dat), collapse = ", ")), width = 80)))
  }
  
  sites <- readVECT("sites", ignore.stderr = TRUE)
  if(!site_id %in% colnames(sites@data)){
    stop(writeLines(strwrap(paste0("'site_id' (", site_id, ") must contain a valid colum name in 'sites'. Options are: ", 
                                   paste0(colnames(sites@data), collapse = ", ")), width = 80)))
  }
  
  sites <- sp::merge(sites, dat, by = site_id, duplicateGeoms = TRUE, all.x = all_sites)
  d <- sites@data
  d$pid <- 1:nrow(d)
  row.names(d) <- 1:nrow(d)
  sites@data <- d
  writeVECT(sites, "sites", v.in.ogr_flags = "overwrite", ignore.stderr = TRUE)
}
