#' Calculate total catchment area of a stream segment and assign a network id.
#' 
#' Recursive function to calculate the upstream area of each stream segment and 
#' assign a unique network id. It is called by \code{\link{calc_edges}} for each 
#' outlet and should not be called by the user. 

#' @param dt data.table containing the attributes of the stream segments
#' @param id integer; stream of the stream segment
#' @param netID integer; network ID
#' @keywords internal
#' 
#' @return Total catchement area upstream of the segment
#' 
#' @author Mira Kattwinkel, \email{mira.kattwinkel@@gmx.net}
#' 
#'\dontrun{
#'  outlets <- dt.streams[next_stream == -1]$stream
#'  netID <- 1
#'  for(i in outlets){
#'    calcCatchmArea_assignNetID(dt.streams, id=i, netID)
#'    netID <- netID + 1
#'  }
#'}

calcCatchmArea_assignNetID <- function(dt, id, net_ID){
  if(dt[stream == id,prev_str01] == 0){  # check only one of prev01 and prev02 because they are always both 0
    dt[stream == id, total_area := area]
    dt[stream == id, netID := net_ID]
  } else {
    a1 <- calcCatchmArea_assignNetID(dt, dt[stream == id, prev_str01],net_ID)
    a2 <- calcCatchmArea_assignNetID(dt, dt[stream == id, prev_str02],net_ID)
    dt[stream == id, total_area := a1 + a2 + dt[stream == id, area]]
    dt[stream == id, netID := net_ID]
  }
  return(dt[stream == id, total_area])
}
