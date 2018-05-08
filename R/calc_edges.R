#'Calculate edges for SSN object.
#'
#'@description A vector (lines) map 'edges' is derived from 'streams_v' and
#'  several attributes are assigned.
#'
#'@details Steps include:
#'\itemize{ \item{Assign unique 'rid' to each stream segment}
#'\item{Find different stream networks in the region and assign 'netID'}
#'\item{Calculate segments upstream distance, 'upDist' = flow length from the 
#' upstream node of the stream segment to the outlet of the network}
#'\item{Calculate reach contributing areas (RCA ) per segment,
#''rcaArea' = subcatchment area of each segment in square km}
#'\item{Calculate catchment areas, 'H2OArea' = total catchment area of each
#'segment in square km} }
#'All lengths are rounded to 2 and all areas to 6 decimal places, respectively.
#'
#'@return Nothing. The function produces the following map: \itemize{
#'  \item{'edges':} {derived stream segments with computed attributes needed for
#'  'SSN' (vector)} }
#'
#'@note \code{\link{setup_grass_environment}}, \code{\link{import_data}} and
#'  \code{\link{derive_streams}} must be run before.
#'
#'@author Mira Kattwinkel, \email{mira.kattwinkel@@gmx.net}, 
#'  Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#'@export
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
#' # Prepare edges
#' calc_edges()
#' 
#' # Plot data
#' dem <- readRAST('dem', ignore.stderr = TRUE)
#' edges <- readVECT('edges', ignore.stderr = TRUE)
#' plot(dem, col = terrain.colors(20))
#' lines(edges, col = 'blue')
#' }
#' 

calc_edges <- function() {
  rast <- execGRASS("g.list",
                    parameters = list(
                      type = "rast"
                    ),
                    intern = TRUE)
  # MiKatt: streams_v (vector) is not needed in calculations but streams_r (raster)
  if (!"streams_r" %in% rast)
    stop("Missing data. Did you run derive_streams()?")
  
  temp_dir <- tempdir()

  execGRASS("g.copy",
            flags = c("overwrite", "quiet"),
            parameters = list(
              vector = "streams_v,edges"))

  # Remove points from edges (only lines are needed).
  # Only needed if correct_compl_junctions was not run.
  # get maximum category value plus 1
  nocat<-as.character(max(as.numeric(
                                     execGRASS("v.db.select",
                                               parameters = list(
                                                 map= "edges",
                                                 columns= "cat"),
                                               intern =T )[-1]
                                     ))+1)
  # delete all points
  execGRASS("v.edit",
            flags = c("r", "quiet"),    # r: reverse selection = all
            parameters = list(
              map = "edges",
              type = "point",
              tool = "delete",
              cats = nocat), ignore.stderr = T)

  # calculate basins for streams segments --------
  message("Calculating reach contributing area (RCA)...\n")
  # MiKatt: Could this be done in one step in r.watershed when accumulation map is computed in derive_streams.R?
  # MiKatt: Check if that would be faster: results in approx. two time more basins due to tiny stream snipplets from r.watershed --> keep it as it is.
  execGRASS("r.stream.basins",
            flags = c("overwrite", "quiet"),
            parameters = list(direction = "dirs",
                              stream_rast = "streams_r",
                              basins = "rca"))

  message("Calculating upstream catchment areas... \n")
  # Calculate reach contributing area for each stream segment (=edges.drain_area) --------
  #! Works, but slow
  #! This is used to calculate PI via SSN
  # calculate area (in m^2) of catchments
  # MiKatt: r.stats with flag "a" gives area in sqrt m, not map units!
  areas <- do.call(rbind,
                   strsplit(execGRASS("r.stats",
                                      flags = c("a", "quiet"),
                                      parameters = list(input = "rca"),
                                      intern = TRUE),
                            split = ' '))
  # last row is total and not needed
  areas <- as.data.frame(areas[-nrow(areas), ], stringsAsFactors = FALSE)
  setDT(areas)
  setnames(areas, names(areas),c("cat","area"))
  areas[, names(areas) := lapply(.SD, as.numeric)]

  # calculate upstream area per stream segment
  dt.streams <- do.call(rbind,strsplit(
    execGRASS("db.select",
              parameters = list(
                sql = "select cat, stream, next_str, prev_str01,prev_str02 from edges"
              ),intern = T),
    split = '\\|'))
  colnames(dt.streams) <- dt.streams[1,]
  dt.streams <- data.table(dt.streams[-1,, drop = FALSE], "total_area" = 0, "netID" = -1)
  dt.streams[, names(dt.streams) := lapply(.SD, as.numeric)]
  dt.streams<-merge(dt.streams, areas, by="cat", all = T)  # MiKatt: must be 'cat' not 'stream' because stream_r is based on 'cat'!
  setkey(dt.streams, stream)
  # set catchment area of short segments that do not have a rca (NA) to zero (mainly resulting form correct_compl_junctions())
  dt.streams[is.na(area), area := 0 ]

  # MiKatt: Segments without a next segment (= -1) are outlets of catchments
  # MiKatt: Recursive function to calculate total upstream area of stream segments
  outlets <- dt.streams[next_str == -1, stream]
  netID <- 1
  for(i in outlets){
    calcCatchmArea_assignNetID(dt.streams, id=i, netID)
    netID <- netID + 1
  }

  # MiKatt: area is in m² -> convert to km²
  dt.streams[, area := round(area / 1000000, 6)]
  dt.streams[, total_area := round(total_area / 1000000, 6)]
  dt.streams[, rid := seq_len(nrow(dt.streams)) - 1]
  dt.streams[, OBJECTID := stream]
  dt.streams[,  ":=" (cat = NULL, next_str = NULL, prev_str01 = NULL, prev_str02 = NULL)]
  utils::write.csv(dt.streams, file.path(temp_dir, "stream_network.csv"), row.names = F)
  dtype <- t(gsub("numeric", "Integer", sapply(dt.streams, class)))
  dtype[,c("total_area","area")] <- c("Real", "Real")
  write.table(dtype, file.path(temp_dir, "stream_network.csvt"), quote = T, sep = ",", row.names = F, col.names = F)
  execGRASS("db.in.ogr", flags = c("overwrite","quiet"),
            parameters = list(
              input = file.path(temp_dir, "stream_network.csv"),
              output = "stream_network"
            ),ignore.stderr = T)
  execGRASS("v.db.join", flags = "quiet",
            parameters = list(
              map = "edges",
              column = "stream",
              other_table = "stream_network",
              other_column = "stream"
            ))

  # MiKatt: 2 steps necessary because SQLite is not case sensitive
  execGRASS("v.db.renamecolumn",
            parameters = list(
              map = "edges",
              column = "length,segLength"
            ))
  execGRASS("v.db.renamecolumn",
            parameters = list(
              map = "edges",
              column = "segLength,Length"
            ))
  execGRASS("v.db.update", flags = c("quiet"),
            parameters = list(
              map = "edges",
              column = "Length",
              value = "round(Length, 2)"
            ))
  execGRASS("v.db.renamecolumn", flags = "quiet",
            parameters = list(
              map = "edges",
              column = "cum_length,sourceDist"
            ))
  execGRASS("v.db.update", flags = c("quiet"),
            parameters = list(
              map = "edges",
              column = "sourceDist",
              value = "round(sourceDist, 2)"
            ))
  execGRASS("v.db.renamecolumn", flags = "quiet",
            parameters = list(
              map = "edges",
              column = "out_dist,upDist"
            ))
  execGRASS("v.db.update", flags = c("quiet"),
            parameters = list(
              map = "edges",
              column = "upDist",
              value = "round(upDist, 2)"
            ))
  execGRASS("v.db.renamecolumn", flags = "quiet",
            parameters = list(
              map = "edges",
              column = "total_area,H2OArea"
            ))
  execGRASS("v.db.renamecolumn", flags = "quiet",
            parameters = list(
              map = "edges",
              column = "area,rcaArea"
            ))
  execGRASS("db.droptable", flags = c("quiet","f"),
            parameters = list(
              table = "stream_network"
            ))
}

#' calcCatchmArea_assignNetID
#' @title Calculate total catchment area of a stream segment and assign a network id.
#'
#' @description Recursive function to calculate the upstream area of each stream segment and
#' assign a unique network id. It is called by \code{\link{calc_edges}} for each
#' outlet and should not be called by the user.

#' @param dt data.table containing the attributes of the stream segments
#' @param id integer; 'stream' of the stream segment
#' @param netID integer; network ID
#' @keywords internal
#'
#' @return Total catchment area upstream of the segment
#'
#' @author Mira Kattwinkel, \email{mira.kattwinkel@@gmx.net}
#'
#'@examples
#'\dontrun{
#'  outlets <- dt.streams[next_str == -1, stream]
#'  netID <- 1
#'  for(i in outlets){
#'    calcCatchmArea_assignNetID(dt.streams, id = i, netID)
#'    netID <- netID + 1
#'  }
#'}

calcCatchmArea_assignNetID <- function(dt, id, net_ID){
  if(dt[stream == id, prev_str01,] == 0){  # check only one of prev01 and prev02 because they are always both 0
    dt[stream == id, total_area := area]
    dt[stream == id, netID := net_ID]
  } else {
    a1 <- calcCatchmArea_assignNetID(dt, dt[stream == id, prev_str01], net_ID)
    a2 <- calcCatchmArea_assignNetID(dt, dt[stream == id, prev_str02] ,net_ID)
    dt[stream == id, total_area := a1 + a2 + dt[stream == id, area]]
    dt[stream == id, netID := net_ID]
  }
  return(dt[stream == id, total_area])
}

#' get_cats_edges_in_catchment#' 
#' Returns the cats of this and all upstream edges#' 
#' 
#' @description Recursive function to get the stream_ids from one segment upstream.
#' This function is used internally and is not intended to be called by the user.
#' 
#' @param dt data.table containing the attributes of the stream segments
#' @param str_id integer giving the stream_id ('stream') of the starting edge
#' @keywords internal 
#' @return vector of cat values of all upstream edges and the calling one.
#' @author Mira Kattwinkel, \email{mira.kattwinkel@@gmx.net}
#' 
get_cats_edges_in_catchment<-function(dt, str_id){
  if(dt[stream == str_id, prev_str01] == 0){
    return(dt[stream == str_id, cat])
  } else {
    a1 <- get_cats_edges_in_catchment(dt = dt, dt[stream == str_id, prev_str01])
    a2 <- get_cats_edges_in_catchment(dt = dt, dt[stream == str_id, prev_str02])
    return(c(dt[stream == str_id, cat], a1, a2))
  }
}

