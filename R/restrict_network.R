#' Restrict edges to certain stream networks
#' 
#' When the stream network is derived from a DEM, the network will cover the whole
#' extent of the DEM input. However, the obervation sites might be restricted
#' to a certain area, i.e. to certain networks. This functin deletes edges
#' that belong to networks (based on their netID) without sites (observation or prediction).
#' This can help to save computation time before calculating edge attributes.
#' 
#' @param sites name(s) of sites.
#' @param keep_netIDs numeric (optional); vector of netIDs to keep
#' @param delete_netIDs numeric (optional); vector of netIDs to delete
#' @param keep boolean; should the original 'edges' be saved? Default is TRUE.
#' @param filename character string; file name to save the original edges vector file; 
#'   defaults to 'edges_o'.
#' 
#' @return Nothing. The function updates 'edges' and (if keep = TRUE) saves 
#' the original file to the file name provided.
#' 
#'
#' @author Mira Kattwinkel  \email{mira.kattwinkel@@gmx.net}
#' @export
#' 
#' @examples
#' \donttest{
#' # Initiate GRASS session
#' if(.Platform$OS.type == "windows"){
#'     gisbase <- "c:/Program Files/GRASS GIS 7.6.0"
#' } else {
#'   gisbase <- "/usr/lib/grass74/"
#' }
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
#' # Check and correct complex confluences (there are no complex confluences in this
#' # example date set; set accum_threshold in derive_streams to a smaller value
#' # to create complex confluences)
#' cj <- check_compl_confluences()
#' if(cj){
#'   correct_compl_confluences()
#' }
#' 
#' calc_edges()
#' calc_sites()
#' restrict_network(sites = "sites", keep = TRUE, filename = "edges_o")
#' 
#' # plot
#' edges <- readVECT('edges', ignore.stderr = TRUE)
#' edges_o <- readVECT('edges_o', ignore.stderr = TRUE)
#' sites <- readVECT('sites', ignore.stderr = TRUE)
#' plot(edges_o, col = "lightblue", lwd = 2)
#' lines(edges, col = "blue4")
#' points(sites, pch = 16, col = "red")
#' legend("topright", col = c("red", "lightblue", "blue4"), lty = c(NA, 1,1),
#' lwd = c(NA,2,1), pch = c(16,NA,NA),
#' legend = c("sites", "edges original", "edges restricted"))
#' }

restrict_network <- function(sites = NULL, 
                             keep_netIDs = NULL, 
                             delete_netIDs = NULL, 
                             keep = TRUE,
                             filename = "edges_o"){
  
  if(! is.null(sites)){
    vect <- execGRASS("g.list",
                    parameters = list(
                      type = "vector"
                    ), intern = TRUE)

    if(!any(sites %in% vect)){
      stop(ifelse(length(sites) == 1, paste(sites, "does not exist."), paste(paste(sites, collapse = ", "), "do not exist.")))
    }
  }
  
  if((sum(is.null(keep_netIDs), is.null(delete_netIDs), is.null(sites)) == 3) |
     (!is.null(keep_netIDs) & ! is.null(delete_netIDs)) |
     (!is.null(sites) & ! is.null(delete_netIDs)))
    stop("Only 'sites' and 'keep_netIDs' can be combined; 'delete_netIDs' must be provided without the other two.")
  
  
  netIDs <- NULL
  if(!is.null(sites)){
    for(i in 1:length(sites)){
      netIDs <- c(netIDs,unique(as.numeric(execGRASS("v.db.select", flags = "quiet", 
                                                     parameters = list(
                                                       map = sites[i], 
                                                       columns = "netID",
                                                       separator = ","
                                                     ), 
                                                     ignore.stderr = TRUE,
                                                     intern = TRUE)[-1])))
    }
  }
  if(!is.null(keep_netIDs)){
    netIDs <- c(netIDs, keep_netIDs)
  }
  netIDs <- unique(netIDs)
  
  if(!is.null(delete_netIDs)){
    all_netIDs <- unlist(strsplit(
      execGRASS("db.select", 
              parameters = list(
                sql = "select netID from edges"
              ), intern = T), 
      split = "\\|"))[-1]
    all_netIDs <- sort(as.numeric(unique(all_netIDs)))
    netIDs <- all_netIDs[-which(all_netIDs %in% delete_netIDs)]
  }
  
  if(keep == TRUE){
    execGRASS("g.copy", flags = c("quiet", "overwrite"),
              parameters = list(
                vector = paste0("edges,", filename)
              ), intern = TRUE, ignore.stderr = TRUE)
    message(paste0("Original edges moved to ", filename, "."))
  }
  
  message(paste("Deleting edges with netIDs other than", paste(netIDs, collapse = ", "), "..."))
  # This deletes the features but keeps them in the attribute table
  # execGRASS("v.edit", flags = c("overwrite", "quiet"),
  #           parameters = list(
  #             map = "edges", 
  #             type = "line",
  #             tool = "delete", 
  #             where = paste0("netID NOT IN (", paste0(netIDs, collapse = "," ), ")")
  #           ))
  a <- execGRASS("v.extract", flags = c("overwrite", "quiet"),
            parameters = list(
              input = filename, 
              output = "edges",
              type = "line",
              where = paste0("netID IN (", paste0(netIDs, collapse = "," ), ")")
            ), intern = TRUE, ignore.stderr = TRUE)
}

