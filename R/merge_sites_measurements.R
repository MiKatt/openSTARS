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
#'@param all_sites logical; should sites without measurements be preserved (default FALSE)
#'@param ... additional arguments to read.table in case \code{measuremtes} is a file path
#' to table data; see \code{\link{read.table}} for details.
#'  
#'@details Measurements are merged to the sites objects based on \code{site_id}. If
#'  there are repeated measurements, point features are duplicated and the 'pid' of the 
#'  sites is updated accordingly to be unique. 
#'
#'@author Mira Kattwinkel \email{mira.kattwinkel@@gmx.net}
#'
#'@export
#' 
#' @examples 
#' \donttest{
#' dem_path <- system.file("extdata", "nc", "elev_ned_30m.tif", package = "openSTARS")
#' if(.Platform$OS.type == "windows"){
#'   grass_program_path = "c:/Program Files/GRASS GIS 7.6"
#'   } else {
#'   grass_program_path = "/usr/lib/grass78/"
#'   }
#' 
#' setup_grass_environment(dem = dem_path, 
#'                         gisBase = grass_program_path,      
#'                         remove_GISRC = TRUE,
#'                         override = TRUE
#'                         )
#' gmeta()
#'                         
#' # Load files into GRASS
#' dem_path <- system.file("extdata", "nc", "elev_ned_30m.tif", package = "openSTARS")
#' sites_path <- system.file("extdata", "nc", "sites_nc.shp", package = "openSTARS")
#' import_data(dem = dem_path, sites = sites_path)
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
#' 
#' merge_sites_measurements(measurements = system.file(
#'   "extdata", "nc", "obs_data.csv", package = "openSTARS"),
#'    site_id = "site_id", sep = ",", dec = ".")
#' # note the dublicated rows, and the new columns at the end
#' sites <- readVECT("sites", ignore.stderr = TRUE)
#' head(sites@data, n = 6)
#'}


merge_sites_measurements <- function(measurements, site_id, all_sites = FALSE, ...) {
  
  if(!is.data.frame(measurements) & !is.data.table(measurements)){
    d <- try(measurements <- utils::read.table(measurements, header = T, stringsAsFactors = FALSE, ...))
    if(class(d) == "try-error")
      stop("'measurements' must contain a valid path name to table data.")
  } 
  
  if(!site_id %in% colnames(measurements)){
    #stop(writeLines(strwrap(paste0("'site_id' (", site_id, ") must contain a valid colum name in 'measurements'. Options are: ", 
    #                               paste0(colnames(measurements), collapse = ", ")), width = 80)))
    stop(paste0("'site_id' (", site_id, ") must contain a valid colum name in 'measurements'. Options are: ", 
                                   paste0(colnames(measurements), collapse = ", ")))
  }
  
  sites <- readVECT("sites", ignore.stderr = TRUE)
  if(!site_id %in% colnames(sites@data)){
    #stop(writeLines(strwrap(paste0("'site_id' (", site_id, ") must contain a valid colum name in 'sites'. Options are: ", 
    #                               paste0(colnames(sites@data), collapse = ", ")), width = 80)))
    stop(paste0("'site_id' (", site_id, ") must contain a valid colum name in 'sites'. Options are: ", 
                                   paste0(colnames(sites@data), collapse = ", ")))
  }
  
  sites <- sp::merge(sites, measurements, by = site_id, duplicateGeoms = TRUE, all.x = all_sites)
  d <- sites@data
  d$pid <- 1:nrow(d)
  row.names(d) <- 1:nrow(d)
  i <- which(colnames(d) %in% c("cat", "cat_"))
  d <- d[,-i]
  sites@data <- d
  sink("temp.txt")
  writeVECT(sites, "sites", v.in.ogr_flags = c("overwrite", "quiet", "o"), ignore.stderr = TRUE)
  sink() 
}
