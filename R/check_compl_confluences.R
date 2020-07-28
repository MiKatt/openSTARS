#' Check if there are more than two inflows to an outflow.
#'
#' It is checked, if more than two line segments flow into a node, i.e.
#' if there are more than two inflows to an outflow.
#'
#' @return TRUE if there are complex confluences.
#' 
#' @details It is checked, if there are columns named 'prev_str03', 'prev_str04' and
#' 'prev_str05' in the attribute table of streams_v derived with \code{derive_streams}
#' (i.e. based on the GRASS function 
#' \href{https://grass.osgeo.org/grass78/manuals/addons/r.stream.order.html}{r.stream.order}).
#'
#' @note \code{\link{setup_grass_environment}}, \code{\link{import_data}} and
#'   \code{\link{derive_streams}} must be run before.
#'
#' @author Mira Kattwinkel \email{mira.kattwinkel@@gmx.net}
#' @export
#'
#' @examples
#' \donttest{
#' # Initiate and setup GRASS
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
#' check_compl_confluences()
#' 
#' # Deriving streams with finer resolution will result in complext confluences
#'  derive_streams(burn = 0, accum_threshold = 150, condition = TRUE, clean = TRUE)
#'  check_compl_confluences()
#' }

check_compl_confluences <- function(){
  ret <- FALSE
  cnames<-execGRASS("db.columns",
                    parameters = list(
                      table = "streams_v"
                    ), intern = TRUE)
  if("prev_str03" %in% cnames){
    #message(writeLines(strwrap("There are complex confluences in the stream network. Please run correct_compl_confluences for correction.", width = 80)))
    message("There are complex confluences in the stream network. Please run correct_compl_confluences() for correction.")
    ret <- TRUE
  }
  return(ret)
}
