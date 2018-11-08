#' Calculate binary IDs for each stream network.
#'
#' Calculate binary IDs for each stream network built up by '0' and '1'.
#' This function is called by \code{\link{export_ssn}} and there is no need for it
#' be called by the users.
#'
#' @import data.table
#'
#' @return A list with one slot for each network id containing a data frame
#' with 'rid' and 'binaryID' for each segment belonging to this network.
#'
#' @note \code{\link{import_data}}, \code{\link{derive_streams}},
#'   \code{\link{calc_edges}} and \code{\link{calc_sites}} must be run before.
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}; Mira Kattwinkel, \email{mira.kattwinkel@@gmx.net}
#' @export
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
#' # Check and correct complex junctions (there are no complex juctions in this 
#' # example date set)
#' cj <- check_compl_junctions()
#' if(cj){
#'   correct_compl_junctions()
#' }
#' 
#' # Prepare edges
#' calc_edges()
#'
#' # Prepare site
#' calc_sites()
#' 
#' binaries <- calc_binary()
#' head(binaries[[1]])
#' }

calc_binary <- function(){
  vect <- execGRASS("g.list",
                    parameters = list(
                      type = 'vect'
                    ),
                    intern = TRUE)
  if (!'sites_o' %in% vect)
    stop('Sites not found. Did you run import_data()?')
  if (!'edges' %in% vect)
    stop('edges not found. Did you run calc_edges()?')
  if (!'sites' %in% vect)
    stop('sites not found. Did you run calc_sites()?')

  dt.streams<-execGRASS('db.select',
               flags = 'c',
               parameters = list(
               sql = 'select rid,stream,next_str,prev_str01,prev_str02,netID from edges',
               separator = ','
               ), intern = TRUE)

  dt.streams<-do.call(rbind,strsplit(dt.streams,split=","))
  dt.streams<-apply(dt.streams,2,as.numeric)
  colnames(dt.streams)<-c("rid","stream","next_str","prev_str01","prev_str02","netID")
  dt.streams <- data.frame(dt.streams)
  setDT(dt.streams)
  dt.streams[, binaryID := "0"]
  outlets <- dt.streams[next_str == -1, stream]

  for(i in outlets){
    assign_binIDs(dt = dt.streams, id=i, 1, NULL)
  }

  bins<-lapply(outlets, function(x) dt.streams[netID == dt.streams[stream == x, netID], list(rid,binaryID)])
  names(bins)<-  dt.streams[stream %in% outlets, netID]
  return(bins)
}

#' assign_binIDs
#' Recursive function to assign binary id to stream segments
#' 
#' Should be run for all outlets in the network ( = most downstream segments) and fills the binID for all segments
#' @param id: stream segment
#' @param binID: binary ID
#' @param lastbit: last char to be added (0 or 1)
#' @keywords internal
#' 
assign_binIDs <- function(dt, id, binID, lastbit){
  if(dt[stream == id, prev_str01 ] == 0){  # check only one of prev01 and prev02 because they are always both 0
    dt[stream == id, binaryID := paste0(binID, lastbit)]
  } else {
    dt[stream == id, binaryID := paste0(binID,lastbit)]
    assign_binIDs(dt, dt[stream == id, prev_str01], dt[stream == id, binaryID], 0)
    assign_binIDs(dt, dt[stream == id, prev_str02], dt[stream == id, binaryID], 1)
  }
}

