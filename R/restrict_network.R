#' Restict edges to certain stream networks
#' 
#' When the stream network is derived from a DEM, the network will cover the whole
#' extent of the DEM input. However, the obervation sites might be restricted
#' to a certain area, i.e. to certain networks. This functin deletes edges
#' that belong to networks (based on their netID) without sites (observation or prediction).
#' This can help to save computation time before calculating edge attributes.
#' 
#' @param sites name(s) of sites.
#' @param keep boolean; should the original 'edges' be saved as 'edges_o'? Default is TRUE.
#' 
#' @return Nothing. The function updates 'edges' and (if keep = TRUE) saves 
#' the original file to 'edges_o'.
#' 
#'
#' @author Mira Kattwinkel  \email{mira.kattwinkel@@gmx.net}
#' @export
#' 
#' #' @examples
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
#' lakes_path <- system.file("extdata", "nc", "lakes.shp", package = "openSTARS")
#' setup_grass_environment(dem = dem_path)
#' import_data(dem = dem_path, sites = sites_path)
#' gmeta()
#'
#' # Derive streams from DEM
#' derive_streams(burn = 0, accum_threshold = 100, condition = TRUE, clean = TRUE)
#' 
#' # Check and correct complex junctions (there are complex juctions in the 
#' # example date set if the accumulation threshold is low)
#' cj <- check_compl_junctions()
#' if(cj){
#'   correct_compl_junctions()
#' }
#' 
#' delete_lakes(lakes = lakes_path)
#' 
#' # plot
#' dem <- readRAST('dem', ignore.stderr = TRUE)
#' streams <- readVECT('streams_v', ignore.stderr = TRUE)
#' streams_with_lakes <- readVECT('streams_v_prev_lakes', ignore.stderr = TRUE)
#' lakes <- readVECT('lakes', ignore.stderr = TRUE)
#' # zoom to a relevant part of the dem
#' plot(dem, col = terrain.colors(20), axes = TRUE)
#' lines(streams_with_lakes, col = 'red', lty = 2, lwd = 2)
#' lines(streams, col = 'blue', lty = 4, lwd = 2)

#' legend("topright", col = c("red", "blue"), lty = c(1,4), lwd = c(2,2), 
#'   legend = c("though lakes", "lakes cut out"))
#' }

restict_network <- function(sites, keep = TRUE){
  
  vect <- execGRASS("g.list",
                    parameters = list(
                      type = "vector"
                    ), intern = T)

  if(!any(sites) %in% vect){
    stop(ifelse(length(sites) == 1, paste(sites, "does not exist."), paste(paste(sites, collapse = ", "), "do not exist.")))
  }
  
  netIDs <- NULL
  for(i in 1:length(sites)){
    netIDs <- c(netIDs,unique(as.numeric(execGRASS("v.db.select", flags = "quiet", 
                                                   parameters = list(
                                                     map = sites[i], 
                                                     columns = "netID",
                                                     separator = ","
                                                   ), intern = T)[-1])))
  }
  netIDs <- unique(netIDs)
  
  if(keep == TRUE){
    execGRASS("g.copy", flags = c("quiet", "overwrite"),
              parameters = list(
                vector = "edges,edges_o"
              ))
  }
  
  message(paste("Deleting edges with netIDs", paste(netIDs, collapse = ", "), "..."))
  execGRASS("v.edit", flags = c("overwrite", "quiet"),
            parameters = list(
              map = "edges", 
              type = "line",
              tool = "delete", 
              where = paste("netID IN (", paste(netIDs, collapse = "," ), ")")
            ))
}

