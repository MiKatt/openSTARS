#' Setup 'GRASS' environment.
#'
#' This function sets the 'GRASS' mapset to PERMANENT and sets its projection and extension.
#'
#' @param dem character; path to DEM.

#' @return Nothing, the 'GRASS' mapset is set to PERMANENT,
#' its projection and extent are set to that one of the dem.
#'
#' @note A 'GRASS' session must be initiated before, seem \code{\link[rgrass7]{initGRASS}}.
#' 
#' @author Mira Kattwinkel, \email{mira.kattwinkel@@gmx.net}
#' @export
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
#' gmeta()
#' }

setup_grass_environment <- function(dem) {
  if (nchar(get.GIS_LOCK()) == 0)
    stop("GRASS not initialised. Please run initGRASS().")
  message("Setting up GRASS Environment...\n")

  # Set Projection to input file -------------------------
  ## AS: Habe optionen hinzugefügt, dass Punkte auch SP*-Objekte aus R sein können
  execGRASS("g.mapset", flags = c("quiet"),
            parameters = list(
              mapset = "PERMANENT"))

 
  execGRASS("g.region", flags = c("quiet"),
            parameters = list(
              raster = dem
              ))
  
  execGRASS("g.project", flags = c("quiet"),
            parameters = list(
              georef = dem
            ))
  }
