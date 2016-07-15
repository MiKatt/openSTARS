#'
#' @description
#' Calcuate edges for SSN object.
#'
#' Steps include:
#' \itemize{
#'  \item{Assigning unique "rid" and "OBJECTID" to each stream segment}
#'  \item{Finding different stream networks in the region and assigning "netID"}
#'  \item{Calculation of stream topology, "topo_dim"}
#'  \item{Calculation of segments upstream distance ("upDist")}
#'  \item{Calculation of river contributing areas (RCA) per segment, "rcaArea"}
#'  \item{Calculation of watershed areas, "H2OArea"}
#' }
#'
#' @param clean logical; Should intermediate layer be removed from GRASS session?
#' @return Nothing. The function produces the following maps:
#' \itemize{
#'  \item{"edges"}{derived stream with computed attributes needed for SSN (vector)}
#'  \item{"stream_topo"}{stream topology measures (vector)}
#' }
#'
#' @note \code{\link{import_data}} and \code{\link{derive_streams}} must be run before.
#' Intermediate layers cleaned are:
#' \itemize{
#'  \item{"netID_v"}{network IDs (vector)}
#'  \item{"netID"}{network IDs (raster)}
#'  \item{"rca"}{RCA per segement (raster)}
#' }
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
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
#' import_data(dem = dem_path, sites = sites_path)
#' derive_streams()
#' calc_edges()
#' edges <- readVECT('edges', ignore.stderr = TRUE)
#' head(edges@data)
#' dem <- readRAST('dem', ignore.stderr = TRUE)
#' sites <- readVECT('sites_o', ignore.stderr = TRUE)
#' plot(dem, col = terrain.colors(20))
#' points(sites, pch = 4)
#' lines(edges, col = 'blue')
#' }

calc_edges()

calc_edges <- function(clean = TRUE) {
  require(data.table)
  vect <- execGRASS("g.list",
                    parameters = list(
                      type = 'vect'
                    ),
                    intern = TRUE)
  rast <- execGRASS("g.list",
                    parameters = list(
                      type = 'rast'
                    ),
                    intern = TRUE)
  if (!'streams_v' %in% vect)
    stop('Missing data. Did you run derive_streams()?')
  
  #' Calculate upstream area of every stream segment and assign unique id for each network
  #' Recursive function to calculate total upstream area of stream segments
  #' data.table dt.streams must be present containing topological information and area of the respective area per segment
  #' Should be run for all outlets in the network ( = most downstream segments) and fills the total area for all segments
  #' '<<-' "call by reference" assigns to global variable
  #' Must be defined within function, otherwise it doea not know dt.streams
  #' @param id cat of the stream segment
  #' @param net ID network ID
  #' @keywords internal
  calcTotalArea_assignNetID<-function(id,netID){
    if(dt.streams[id]$prev_str01 == 0){  # check only one of prev01 and prev02 because they are always both 0
      dt.streams[id]$total_area <<- dt.streams[id]$area
      dt.streams[id]$netID <<- netID
    } else {
      a1 <- calcTotalArea_assignNetID(dt.streams[id]$prev_str01,netID)
      a2 <- calcTotalArea_assignNetID(dt.streams[id]$prev_str02,netID)
      dt.streams[id]$total_area <<- a1 + a2 + dt.streams[id]$area
      dt.streams[id]$netID <<- netID
    }
    return(dt.streams[id]$total_area)
  }

  # MiKatt: Can all be done based on streams_topo
  # message('Assigning rid...\n')
  # # Set stream segment id (=edges.rid) --------------------------------------
  # execGRASS("g.copy",
  #           flags = c('overwrite', 'quiet'),
  #           parameters = list(
  #             vector = 'streams_v,edges'))
  # 
  # edges <- readVECT('edges', type = 'line', ignore.stderr = TRUE)
  # edges$rid <- seq_len(nrow(edges)) - 1
  # writeVECT(edges, 'edges',
  #           v.in.ogr_flags = c('overwrite', 'quiet'),
  #           ignore.stderr = TRUE)
  # 
  # # OBJECTID (same as rid?)
  # execGRASS("v.db.addcolumn",
  #           flags = c('quiet'),
  #           parameters = list(map = 'edges',
  #                             columns = 'OBJECTID int'))
  # execGRASS("v.db.update",
  #           flags = c('quiet'),
  #           parameters = list(map = 'edges',
  #                             column = 'OBJECTID',
  #                             value = 'rid'))
  # 
  # # copy cat to cat_o
  # execGRASS("v.db.addcolumn",
  #           flags = c('quiet'),
  #           parameters = list(map = 'edges',
  #                             columns = 'cat_o int'))
  # execGRASS("v.db.update",
  #           parameters = list(
  #             map = 'edges',
  #             column = 'cat_o',
  #             value = 'cat_'
  #           ))
  # 
  # # Set network id (=edges.netID) -------------------------------------------
  # #! Works but slow and not nice
  # message('Setting netID...\n')
  # # Get basins for outlets
  # # MiKatt: Can be done when calculatin upstream watershed area per segment within the same step
  # execGRASS("r.stream.basins",
  #           flags = c("overwrite", "l", "quiet"),          # 'l': only last stream link
  #           parameters = list(direction = "dirs",
  #                             stream_rast = "streams_r",
  #                             basins = "netID"))
  # execGRASS("r.to.vect",
  #           flags = c("overwrite", "quiet"),
  #           parameters = list(input = 'netID',
  #                             output = 'netID_v',
  #                             type = 'area'))
  # execGRASS("v.db.addcolumn",
  #           flags = c('quiet'),
  #           parameters = list(map = 'edges',
  #                             columns = 'netID int'))
  # # MiKatt: slow
  # execGRASS("v.what.vect",
  #           parameters = list(map = 'edges',
  #                             column = 'netID',
  #                             query_map = 'netID_v',
  #                             query_column = 'value',
  #                             dmax = 1))

  # calculate stream topology ----------
  message('Calculating stream topology...\n')
  # MiKatt: Is accumulation needed here? r.stream.order: "This map is an option only if Horton's or Hack's ordering is performed." Yes, does not work without.
  execGRASS("r.stream.order",
            flags = c('overwrite', 'quiet','z','m'),
            parameters = list(stream_rast = 'streams_r',     # input
                              direction = 'dirs',            # input
                              elevation = 'dem',             # input
                              accumulation = 'accums',       # input
                              stream_vect = 'edges'))        # output

  # # Dirty way to remove points from streams_topo
  # streams_topo <- readVECT('streams_topo',
  #                     type = 'line',
  #                     ignore.stderr = TRUE)
  # writeVECT(streams_topo, 'streams_topo', v.in.ogr_flags = 'overwrite')

  # MiKatt: This is much faster (ca. factor 850)
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
              cats = nocat))

  # copy cat to cat_o
  # MiKatt: What is this for?
  # execGRASS("v.db.addcolumn",
  #           flags = c('quiet'),
  #           parameters = list(map = 'streams_topo',
  #                             columns = 'cat_o int'))
  # execGRASS("v.db.update",
  #           parameters = list(
  #             map = 'streams_topo',
  #             column = 'cat_o',
  #             value = 'cat'
  #           ))
  
  # add rid from edges to streams_topo
  # MiKatt: Can one be sure that the tow cat_o columns point to the same object?
  # MiKatt: Why not do everything in streams_topo instead of using edges (= streams_v)
  # MiKatt: slow
  # MiKatt: Can be done for stream_topo based on cat (=  value of streams_r)
  # execGRASS("v.db.join",
  #           parameters = list(
  #             map = 'streams_topo',
  #             column = 'cat_o',
  #             other_table = 'edges',
  #             other_column = 'cat_o',
  #             subset_columns = 'rid'
  #           ))

  # can join using v.what.vect because no overlap of different networks
  # MiKatt: Why not same way as for rid?
  # MiKatt: Can be done when total upstream area is calculated
  # execGRASS("v.db.addcolumn",
  #           flags = c('quiet'),
  #           parameters = list(map = 'streams_topo',
  #                             columns = 'netID int'))
  # # MiKatt: slow
  # # MiKatt: Can be done when calculatin upstream watershed area per segment within the same step
  # execGRASS("v.what.vect",
  #           parameters = list(map = 'streams_topo',
  #                             column = 'netID',
  #                             query_map = 'edges',
  #                             query_column = 'netID',
  #                             dmax = 1))

  # execGRASS('v.info',
  #           parameters = list(
  #             map = 'streams_topo'
  #           ))

  # upstream distance ----------
  # MiKatt: slow
  # MiKatt: Use streams_topo instead of extra edges = streams_v 
  # message('Calculating upDist...\n')
  # execGRASS("v.db.join",
  #           parameters = list(
  #             map = 'edges',
  #             column = 'cat_o',
  #             other_table = 'streams_topo',
  #             other_column = 'cat_o',
  #             subset_columns = 'length,cum_length,topo_dim,out_dist'
  #           ))
  #
  #   # #execGRASS('v.info', flags='e', parameters=list(map = 'streams_topo_2'))
  # # MiKatt: 2 steps necessary because SQLite is not case sensitive
  # MiKatt moved to further below
  #  execGRASS('v.db.renamecolumn',
  #           parameters = list(
  #             map = 'edges',
  #             column = 'length,segLength'
  #           ))
  # execGRASS('v.db.renamecolumn',
  #           parameters = list(
  #             map =  'edges',
  #             column = 'segLength,Length'
  #           ))
  # execGRASS('v.db.renamecolumn',
  #           parameters = list(
  #             map =  'edges',
  #             column = 'cum_length,sourceDist'
  #           ))
  # execGRASS('v.db.renamecolumn',
  #           parameters = list(
  #             map =  'edges',
  #             column = 'out_dist,upDist'
  #           ))


  # calculate basins for streams segments --------
  message('Calculating RCA and area...\n')
  # MiKatt: Could this be done in one step in r.watershed when accumulation map is computed in derive_streams.R?
  # MiKatt: ! Check if that would be faster
  execGRASS("r.stream.basins",
            flags = c("overwrite", "quiet"),
            parameters = list(direction = 'dirs',
                              stream_rast = 'streams_r',
                              basins = "rca"))

  message('Calculating upstream watershed areas \n')
  # Calculate reach contributing area for each stream segment (=edges.drain_area) --------
  #! Works, but slow
  #! This is used to calculate PI via SSN
  # calclate area (in m^2) of basins
  # MiKatt: Would converting raster to vector and calculating its area be faster?
  areas <- do.call(rbind,
                   strsplit(execGRASS('r.stats',
                                      flags = c('a', 'quiet'),
                                      parameters = list(input = 'rca'),
                                      intern = TRUE),
                            split = ' '))
  # Last row is total and not needed
  areas <- areas[-nrow(areas), ]
  
  # calculate upstream area per stream segment
  
  #t1<-Sys.time()
  
  # MiKatt: db.select is faster than readVECT but how to get back data to vector attribute data table?
  # d<-execGRASS('db.select',
  #              flags = 'c',
  #              parameters = list(
  #              sql = 'select cat,next_stream,prev_str01,prev_str02 from edges',
  #              separator = ','
  #              ), intern = TRUE)
  # 
  # d<-do.call(rbind,strsplit(d,split=","))
  # d<-apply(d,2,as.numeric)
  # d<-cbind(d,0,-1)
  # colnames(d)<-c("cat","next_stream","prev_str01","prev_str02","total_area","netID")
  # dt.streams<-as.data.table(d)
  # a<-data.table(as.numeric(areas[,1]),as.numeric(areas[,2]))
  # names(a)<-c("cat","area")
  # dt.streams<-merge(dt.streams,a,by="cat")
  # setkey(dt.streams,cat)
  
  streams <- readVECT('edges',
                      type = 'line',
                      ignore.stderr = TRUE)

  dt.streams<-data.table(streams@data[c("cat","stream","next_stream","prev_str01","prev_str02")],total_area=0, netID=-1)
  a<-data.table(as.numeric(areas[,1]),as.numeric(areas[,2]))
  names(a)<-c("cat","area")
  dt.streams<-merge(dt.streams,a,by="cat")
  setkey(dt.streams,stream)

  # MiKatt: Segments without a next segment are outlets of watersheds
  # MiKatt: Recursive function to calculate total upstream area of stream segments
  outlets <- dt.streams[next_stream == -1]$stream
  netID <- 1
  for(i in outlets){
    calcTotalArea_assignNetID(id=i, netID)
    netID <- netID + 1
  }
  
  #print(Sys.time()-t1) # 13 min

  # MiKatt: Recursion is much faster 
  # up_area <- NA
  # # for each stream segment
  # for (i in seq_len(nrow(streams))) {
  #   # MiKatt: must be cat[i], not cat[1]
  #   cat <- streams$cat[i]
  #   # if no upstream segments take area directly
  #   if (streams@data$prev_str01[i] == 0) {
  #     up_area[i] <- as.numeric(areas[areas[ , 1] == cat , 2])
  #   } else {
  #     # own area
  #     self_area <- as.numeric(areas[areas[ , 1] == cat , 2])
  #     up <- cat_upseg <- c(streams@data$prev_str01[i], streams@data$prev_str02[i])
  #     # accumulate upstream
  #     while (!all(up == 0)) {
  #       upseg <- streams@data[streams@data$cat %in% up[!up == 0], ]
  #       up <- c(upseg$prev_str01, upseg$prev_str02)
  #       cat_upseg <- c(cat_upseg, up[!up == 0])
  #     }
  #     area_upseg <- as.numeric(areas[areas[ , 1] %in% cat_upseg, 2])
  #     # sum upstream and self
  #     up_area[i] <- sum(area_upseg , self_area)
  #   }
  # }
  # #print(Sys.time()-t1) #2.3 hoursgrass db.execute merge
  # areas <- data.frame(rid = streams@data$rid,
  #                     H2OArea = round(up_area / 10000, 2),
  #                     rcaArea = as.numeric(areas[, 2]) / 10000)

  #edges <- readVECT('edges', type = 'line', ignore.stderr = TRUE)

  streams@data <- merge(streams@data, dt.streams, by = 'cat')
  streams@data <- streams@data[,c('cat','stream.x', 'next_stream.x', 'prev_str01.x', 'prev_str02.x','netID','length','cum_length','out_dist','total_area','area')]
  # MiKatt: Why divide by 10000?'
  streams@data[,'total_area'] <- round(streams@data[,'total_area'],2) #/10000,2)
  #streams@data[,'area'] <- streams@data[,'area']#/10000
  colnames(streams@data) <- c('cat_orig','stream', 'next_stream', 'prev_str01', 'prev_str02','netID','Length','sourceDist','upDist','H2OArea','rcaArea')
  streams@data$rid <- seq_len(nrow(streams@data)) - 1 # streams@data$stream
  streams@data$OBJECTID <- streams@data$stream
  
  # MiKatt: Creates new cat column and would move original cat to cat_ 
  writeVECT(streams, 'edges',
            v.in.ogr_flags = c('overwrite', 'quiet'),
            ignore.stderr = TRUE)
  
  # MiKatt: Retrieve orininal cat (=cat_orig) (needed for extraction in v.distance and later intersection with sites)
  execGRASS("v.db.update",
            flags = c('quiet'),
            parameters = list(map = 'edges',
                              column = 'cat',
                              value = 'cat_orig'))

  if (clean) {
    # execGRASS("g.remove",
    #           flags = c('quiet', 'f'),
    #           parameters = list(
    #             type = 'vector',
    #             name = 'netID_v'
    #           ))
    execGRASS("g.remove",
              flags = c('quiet', 'f'),
              parameters = list(
                type = 'raster',
                name = 'netID,rca'
              ))
  }
}

