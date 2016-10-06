#' Calcuate edges for SSN object.
#'
#'@description A vector (lines) map 'edges' is derived from 'streams_v' and several
#'  attributes are assigned.
#'
#'@details
#' Steps include:
#' \itemize{
#'  \item{Assign unique 'rid' and 'OBJECTID' to each stream segment}
#'  \item{Find different stream networks in the region and assign 'netID'}
#'  \item{Calculate segments upstream distance, 'upDist' = flow length
#'   to the outlet of the network}
#'  \item{Calculate reach contributing areas (RCA ) per segment, 'rcaArea'
#'   = subcatchment area of each segment}
#'  \item{Calculate catchment areas; 'H2OArea' = total catchment area of each
#'  segment}
#'  All lengths are rounded to 2 and all areas to 4 decimal places, respectively.
#' }
#'
#' @param clean logical; should intermediate layer (rca, reach contributing area)
#'  be removed from GRASS session?
#' @return Nothing. The function produces the following map:
#' \itemize{
#'  \item{'edges'} {derived stream segments with computed attributes needed for SSN (vector)}
#' }
#'
#' @note \code{\link{setup_grass_environment}}, \code{\link{import_data}} and \code{\link{derive_streams}} must be run before.
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}, Mira Kattwinkel, \email{mira.kattwinkel@@gmx.net}
#' @export
#' @examples
#' \donttest{
#' library(rgrass7)
#' initGRASS(gisBase = "/usr/lib/grass70/",
#'   home = tempdir(),
#'   override = TRUE)
#' gmeta()
#' dem_path <- system.file("extdata", "nc", "elev_ned_30m.tif", package = "openSTARS")
#' sites_path <- system.file("extdata", "nc", "sites_nc.shp", package = "openSTARS")
#' setup_grass_environment(dem = dem_path, sites = sites_path)
#' import_data(dem = dem_path, sites = sites_path)
#' derive_streams()
#' #' cj <- check_compl_junctions()
#' if(cj){
#'   correct_compl_junctions()
#' }
#' calc_edges()
#' edges <- readVECT('edges', ignore.stderr = TRUE)
#' head(edges@data)
#' dem <- readRAST('dem', ignore.stderr = TRUE)
#' sites <- readVECT('sites_o', ignore.stderr = TRUE)
#' plot(dem, col = terrain.colors(20))
#' points(sites, pch = 4)
#' lines(edges, col = 'blue')
#' }


calc_edges <- function(clean = TRUE) {
  rast <- execGRASS("g.list",
                    parameters = list(
                      type = 'rast'
                    ),
                    intern = TRUE)
  # MiKatt: streams_v (vector) is not needed in calculations but streams_r (raster)
  if (!'streams_r' %in% rast)
    stop('Missing data. Did you run derive_streams()?')

  # MiKatt: moved to the end of derive_streams() to test for complex confuences after that function
  # # calculate stream topology ----------
  # message('Calculating stream topology...\n')
  # # MiKatt: Is accumulation needed here? r.stream.order: "This map is an option only if Horton's or Hack's ordering is performed." Yes, does not work without.
  # execGRASS("r.stream.order",
  #           flags = c('overwrite', 'quiet','z','m'),
  #           parameters = list(stream_rast = 'streams_r',     # input
  #                             direction = 'dirs',            # input
  #                             elevation = 'dem',             # input
  #                             accumulation = 'accums',       # input
  #                             stream_vect = 'streams_topo'))        # output

  execGRASS("g.copy",
            flags = c('overwrite', 'quiet'),
            parameters = list(
              vector = 'streams_v,edges'))

  # Remove points from edges (only lines are needed).
  # Only needed if correct_compl_junctions was not run.
  # get maximum category value plus 1
  nocat<-as.character(max(as.numeric(
                                     execGRASS("v.db.select",
                                               parameters = list(
                                                 map="edges",
                                                 columns="cat"),
                                               intern=T)[-1]
                                     ))+1)
  # delete all points
  execGRASS("v.edit",
            flags = c("r", "quiet"),    # r: reverse selection = all
            parameters = list(
              map = 'edges',
              type = "point",
              tool = "delete",
              cats = nocat), ignore.stderr = T)

  # calculate basins for streams segments --------
  message('Calculating reach contributing area (RCA)...\n')
  # MiKatt: Could this be done in one step in r.watershed when accumulation map is computed in derive_streams.R?
  # MiKatt: Check if that would be faster: results in approx. two time more basins due to tiny stream snipplets from r.watershed --> keep it as it is.
  execGRASS("r.stream.basins",
            flags = c("overwrite", "quiet"),
            parameters = list(direction = 'dirs',
                              stream_rast = 'streams_r',
                              basins = "rca"))

  message('Calculating upstream catchment areas... \n')
  # Calculate reach contributing area for each stream segment (=edges.drain_area) --------
  #! Works, but slow
  #! This is used to calculate PI via SSN
  # calculate area (in m^2) of basins
  areas <- do.call(rbind,
                   strsplit(execGRASS('r.stats',
                                      flags = c('a', 'quiet'),          # MiKatt: a -> m²
                                      parameters = list(input = 'rca'),
                                      intern = TRUE),
                            split = ' '))
  # last row is total and not needed
  areas <- areas[-nrow(areas), ]

  # v.db.join is slightly faster than read / write VECT

  # calculate upstream area per stream segment
  dt.streams <- do.call(rbind,strsplit(
    execGRASS("db.select",
              parameters = list(
                sql = "select cat, stream, next_stream, prev_str01,prev_str02 from edges"
              ),intern = T),
    split = '\\|'))
  colnames(dt.streams) <- dt.streams[1,]
  dt.streams <- data.table(dt.streams[-1,], "total_area" = 0, "netID" = -1)
  dt.streams[, names(dt.streams) := lapply(.SD, as.numeric)]
  a<-data.table(as.numeric(areas[,1]),as.numeric(areas[,2]))
  names(a)<-c("cat","area")
  dt.streams<-merge(dt.streams,a,by="cat",all=T)  # MiKatt: must be 'cat' not 'stream' because stream_r is based on 'cat'!
  setkey(dt.streams,stream)
  # set catchment area of short segments that do not have a rac (NA) to zero (mainly resulting form correct_compl_junctions())
  dt.streams[is.na(area), area := 0 ]

  # MiKatt: Segments without a next segment (= -1) are outlets of catchments
  # MiKatt: Recursive function to calculate total upstream area of stream segments
  outlets <- dt.streams[next_stream == -1, stream]
  netID <- 1
  for(i in outlets){
    calcCatchmArea_assignNetID(dt.streams, id=i, netID)
    netID <- netID + 1
  }

  dt.streams[,area := round(area / 1000000, 4)]
  dt.streams[,total_area := round(total_area / 1000000, 4)]
  dt.streams[, rid := seq_len(nrow(dt.streams)) - 1]
  dt.streams[, OBJECTID := stream]
  dt.streams[,':=' (cat = NULL, next_stream = NULL, prev_str01 = NULL, prev_str02 = NULL)]
  dir.create("temp")
  write.csv(dt.streams, file.path("temp","stream_network.csv"),row.names = F)
  write.table(t(gsub("numeric","Integer",sapply(dt.streams,class))),file.path("temp","stream_network.csvt"),quote=T,sep=",",row.names = F,col.names = F)
  execGRASS("db.in.ogr", flags = c("overwrite","quiet"),
            parameters = list(
              input = file.path("temp","stream_network.csv"),
              output = "stream_network"
            ),ignore.stderr = T)
  execGRASS("v.db.join", flags = "quiet",
            parameters = list(
              map = "edges",
              column = "stream",
              other_table = "stream_network",
              other_column = "stream"
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
  # MiKatt: 2 steps necessary because SQLite is not case sensitive
  execGRASS('v.db.renamecolumn',
            parameters = list(
              map = 'edges',
              column = 'length,segLength'
            ))
  execGRASS('v.db.renamecolumn',
            parameters = list(
              map = 'edges',
              column = 'segLength,Length'
            ))
  execGRASS("v.db.update", flags = c("quiet"),
            parameters = list(
              map = "edges",
              column = "Length",
              value = "round(Length, 2)"
            ))

  unlink("temp", recursive =T, force = TRUE)
  execGRASS("db.droptable", flags = c("quiet","f"),
            parameters = list(
              table = "stream_network"
            ))

  #######
  # above is slightly faster
  # # calculate upstream area per stream segment
  # streams <- readVECT('edges',
  #                     type = 'line',
  #                     ignore.stderr = TRUE)
  # dt.streams<-data.table(streams@data[c("cat","stream","next_stream","prev_str01","prev_str02")],total_area=0, netID=-1)
  # a<-data.table(as.numeric(areas[,1]),as.numeric(areas[,2]))
  # names(a)<-c("stream","area")
  # dt.streams<-merge(dt.streams,a,by="stream",all=T)
  # setkey(dt.streams,stream)
  # # set watershed area of short segments that to not have a rac (NA) to zero
  # dt.streams[is.na(area), area := 0 ]
  #
  # # MiKatt: Segments without a next segment (= -1) are outlets of watersheds
  # # MiKatt: Recursive function to calculate total upstream area of stream segments
  # outlets <- dt.streams[next_stream == -1]$stream
  # netID <- 1
  # for(i in outlets){
  #   calcTotalArea_assignNetID(id=i, netID)
  #   netID <- netID + 1
  # }
  #
  # streams@data <- merge(streams@data, dt.streams[,.(stream,total_area,area,netID)], by = 'stream')
  # # MiKatt: Why divide by 10000?'
  # streams@data[,'total_area'] <- round(streams@data[,'total_area'],2) #/10000,2)
  # streams@data <- streams@data[,c("stream","next_stream","prev_str01","prev_str02","cum_length","out_dist","length","total_area","area","netID")]
  # #streams@data[,'area'] <- streams@data[,'area']#/10000
  # colnames(streams@data) <- c('stream', 'next_stream', 'prev_str01', 'prev_str02','sourceDist','upDist','Length','H2OArea','rcaArea','netID')
  # streams@data$rid <- seq_len(nrow(streams@data)) - 1 # streams@data$stream
  # streams@data$OBJECTID <- streams@data$stream
  #
  # # MiKatt: Creates new cat column and moves original cat to cat_
  # writeVECT(streams, 'edges',
  #           v.in.ogr_flags = c('overwrite', 'quiet'),
  #           ignore.stderr = TRUE)
  #######

  if (clean) {
    execGRASS("g.remove",
              flags = c('quiet', 'f'),
              parameters = list(
                type = 'raster',
                name = 'rca'
              ))
  }
}

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
#' @return Total catchement area upstream of the segment
#'
#' @author Mira Kattwinkel, \email{mira.kattwinkel@@gmx.net}
#'
#'@examples
#'\dontrun{
#'  outlets <- dt.streams[next_stream == -1, stream]
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

