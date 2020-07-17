#' Setup 'GRASS' environment.
#'
#' This function sets the 'GRASS' mapset to PERMANENT and sets its projection and extension.
#'
#' @param dem character; path to DEM.  
#' @param gisBase character; the directory path to GRASS binaries and libraries, containing 
#'   bin and lib subdirectories among others (see details).
#' @param ... Optional arguments to be passed to \code{\link[rgrass7]{initGRASS}} (see details).
#'
#' @return Nothing. A GRASS session is initiated and the 'GRASS' mapset is set to PERMANENT. 
#'  The geographical projection, geographical extension, number of columns and number of 
#'  rows for the data and the resolution are defined by the dem. They are stored the DEFAULT_WIND file.
#'
#' @details A GRASS session is initiated using \code{\link[rgrass7]{initGRASS}}. The path to 
#'   the GRASS program must be provided as \code{gisBase}. For Linux, this might look like 
#'   "/usr/lib/grass78/" and for Windows "c:/Program Files/GRASS GIS 7.8".
#'   Optional arguments are for example 
#'   * \code{gisDbase}: the GRASS GISBASE directory for this session; defaults to tempdir()
#'   * \code{location}: name of the location for this session; defaults to tempfile()
#'   * \code{override}: TRUE for allowing to override an existing location.
#' 
#' @author Mira Kattwinkel, \email{mira.kattwinkel@@gmx.net}
#' @export
#'
#' @examples
#' \donttest{
#' # path to GRASS
#' if(.Platform$OS.type == "windows"){
#'   gisbase = "c:/Program Files/GRASS GIS 7.6"
#'   } else {
#'   gisbase = "/usr/lib/grass78/"
#'   }
#' # path to the dem   
#' dem_path <- system.file("extdata", "nc", "elev_ned_30m.tif", package = "openSTARS")
#' setup_grass_environment(dem = dem_path, gisBase = gisbase, location = "nc_example_location")
#' gmeta()
#' }

setup_grass_environment <- function(dem, gisBase, ...){
  dem_grid <- readGDAL(dem, silent = TRUE)
  initGRASS(gisBase = grass_program_path,
            SG = dem_grid,
            mapset = "PERMANENT",
            ...)
  execGRASS("g.proj", flags = c("c", "quiet"),
            parameters = list(
              georef = dem
            ))
}
