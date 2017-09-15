#' Check if there are more than two inflows to an outflow.
#'
#' It is checked, if there are columns named 'prev_str03', 'prev_str04' and
#' 'prev_str05' in the attribute table of streams_v derived with
#' \href{https://grass.osgeo.org/grass70/manuals/addons/r.stream.order.html}{r.stream.order},
#' hence, if there are more than two inflows to a junction.
#'
#' @return TRUE if there are complex junctions.
#'
#' @note \code{\link{setup_grass_environment}}, \code{\link{import_data}} and
#'   \code{\link{derive_streams}} must be run before.
#'
#' @author Mira Kattwinkel \email{mira.kattwinkel@@gmx.net}
#' @export
#' @importFrom rgrass7 execGRASS
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
#' # Load files into GRASS
#' dem_path <- system.file("extdata", "nc", "elev_ned_30m.tif", package = "openSTARS")
#' sites_path <- system.file("extdata", "nc", "sites_nc.shp", package = "openSTARS")
#' setup_grass_environment(dem = dem_path, sites = sites_path)
#' import_data(dem = dem_path, sites = sites_path)
#' gmeta()
#'
#' # Derive streams from DEM
#' derive_streams(burn = 0, accum_threshold = 700, condition = TRUE, clean = TRUE)
#' 
#' check_compl_junctions()
#' }

check_compl_junctions <- function(){
  ret <- FALSE
  cnames<-rgrass7::execGRASS("db.columns",
                    parameters = list(
                      table = "streams_v"
                    ), intern=T)
  if(any(c("prev_str03","prev_str04","prev_str05") %in% cnames)){
    message("There are complex confluences in the stream network. Please run correct_compl_junctions for correction. \n")
    if(length(grep("prev_str",cnames)) > 3) {
      message("There are junctions with more than three inflows. Currently, correct_compl_junctions only works for three inflows.")
    }
    ret <- TRUE
  }
  return(ret)
}
