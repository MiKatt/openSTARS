#' Calculate binary IDs for each stream network.
#' 
#' @import data.table
#' 
#' @return A list with one slot for each network id containing a data frame
#' with 'rid' and 'binaryID' for each segment belonging to this network.
#' 
#' @note \code{\link{import_data}}, \code{\link{derive_streams}}, 
#'   \code{\link{calc_edges}} and code{\link{calc_sites}} must be run before.
#' 
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}; Mira Kattwinkel, \email{mira.kattwinkel@@gmx.net}
#' @export
#' 
#' @examples
#' \donttest{
#' library(rgrass7)
#' initGRASS(gisBase = "/usr/lib/grass70/",
#'   home = tempdir(),
#'   override = TRUE)
#' gmeta()
#' dem_path <- system.file("extdata", "nc", "elev_ned_30m.tif", package = "openSTARS")
#' sites_path <- system.file("extdata", "nc", "sites_nc.shp", package = "openSTARS")
#' import_data(dem = dem_path, sites = sites_path)
#' derive_streams()
#' cj <- check_compl_junctions()
#' if(cj){
#'   correct_compl_junctions()
#' }
#' calc_edges()
#' calc_sites()
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
               sql = 'select rid,stream,next_stream,prev_str01,prev_str02,netID from edges',
               separator = ','
               ), intern = TRUE)

  dt.streams<-do.call(rbind,strsplit(dt.streams,split=","))
  dt.streams<-apply(dt.streams,2,as.numeric)
  colnames(dt.streams)<-c("rid","stream","next_stream","prev_str01","prev_str02","netID")
  dt.streams <- data.frame(dt.streams)
  setDT(dt.streams)
  dt.streams[, binaryID := "0"]
  outlets <- dt.streams[next_stream == -1, stream]

  for(i in outlets){
    assign_binIDs(dt = dt.streams, id=i, 1, NULL)
  }
  
  bins<-lapply(outlets, function(x) dt.streams[netID == dt.streams[stream == x, netID], list(rid,binaryID)])
  names(bins)<-  dt.streams[stream %in% outlets, netID]
  return(bins)
}


# #' Recursive function to assign binary id to stream segments
# #' data.table dt.streams must be present containing topological information for the edges
# #' Should be run for all outlets in the network ( = most downstream segments) and fills the binID for all segments
# #' '<<-' "call by reference" assigns to global variable
# #' Must be defined within function, otherwise it does not know dt.streams
# #' @param id: stream segment
# #' @param binID: binary ID
# #' @param lastbit: last char to be added (0 or 1)
# #' @keywords internal
# assign_binIDs<-function(id,binID,lastbit){
#   if(dt.streams[id]$prev_str01 == 0){  # check only one of prev01 and prev02 because they are always both 0
#     dt.streams[id]$binaryID <<- paste0(binID,lastbit)
#   } else {
#     dt.streams[id]$binaryID <<- paste0(binID,lastbit)
#     assign_binIDs(dt.streams[id]$prev_str01,dt.streams[id]$binaryID,0)
#     assign_binIDs(dt.streams[id]$prev_str02,dt.streams[id]$binaryID,1)
#   }
# }
# 
#' Recursive function to assign binary id to stream segments
#' Should be run for all outlets in the network ( = most downstream segments) and fills the binID for all segments
#' @param id: stream segment
#' @param binID: binary ID
#' @param lastbit: last char to be added (0 or 1)
#' @keywords internal
assign_binIDs <- function(dt, id, binID, lastbit){
  if(dt[stream == id,prev_str01] == 0){  # check only one of prev01 and prev02 because they are always both 0
    dt[stream == id, binaryID := paste0(binID, lastbit)]
  } else {
    dt[stream == id, binaryID := paste0(binID,lastbit)]
    assign_binIDs(dt, dt[stream == id,prev_str01], dt[stream==id, binaryID], 0)
    assign_binIDs(dt, dt[stream == id,prev_str02], dt[stream == id, binaryID], 1)
  }
}


# #' workhorse for calc_binary
# #' @param network network ID
# #' @keywords internal
# 
# calc_binary_horse <- function(network) {
#    # empty id cols
#    network$bin_id <- rep(NA, nrow(network))
#    # for each topological dimension
#    for (i in sort(unique(network$topo_dim))) {
#      rows <- which(network$topo_dim == i)
#      # first segment set to one
#      if (i == 1) {
#        network$bin_id[rows] <- 1
#      } else {
#        # actual segments
#        take_segments <- network[rows, ]@data
#        # downstream segments
#        take_down <- network[network$topo_dim == i - 1, ]@data
#        names(take_down)[2] <- 'stream_down'
#        # merge dwn (with bin_id) and actual
#        take_merge <- merge(take_down[ , c('stream_down', 'bin_id')],
#                            take_segments, by.x = 'stream_down', by.y = 'next_stream')
#        # assign 0/1 and paste with downstream id
#        take_merge[ , 'bin_id'] <-  c(aggregate(bin_id.x ~ stream_down, data = take_merge,
#                  FUN = function(x) paste0(x, sample(c(0, 1), 2)))[ , 'bin_id.x'])
#        take_merge$bin_id.x <- NULL
#        take_merge$bin_id.y <- NULL
#        network$bin_id[rows] <- take_merge$bin_id
#      }
#    }
#    out <- network@data[ , c('rid', 'bin_id')]
#    names(out) <- c("rid", "binaryID")
#    return(out)
#  }