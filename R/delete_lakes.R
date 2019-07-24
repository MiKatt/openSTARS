#' Delete lakes from stream network
#' 
#' When the stream network is derived from a dem, the streams 
#' will just cross lakes or ponds. However, the flow is disconnected
#' here and the relationship between sampling points upstream and
#' downstream of a lake is not clear. For instance, chemicals could
#' be retained and temperature altered in a lake. This function intersects
#' the stream network with a given vector map of lakes; it deletes the stream
#' segments in the lake, breaks those the cross its borders and 
#' assigns a new, updated topology.
#' 
#' @param lakes character string or object; path to lake vector file (ESRI shape), 
#' name of vector map in the GRASS data base or sp or sf data object.
#' @param keep boolean; should the original 'streams_v' be saved? Default is TRUE.
#' 
#' @return Nothing. The function updates 'streams_v' and (if keep = TRUE) saves 
#' the original file to streams_v_prev_lakes.
#' 
#' @note The column 'out_dist' (flow length from the upstream node of the 
#'  segment to the outlet of the network) is updated based on the new segment length.
#'  In contrast, 'cum_length' is not updated as it is no longer needed and will 
#'  be deleted in calc_edges. 
#' 
#' #' 
#' @author Mira Kattwinkel  \email{mira.kattwinkel@@gmx.net}
#' @export
#' 
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

delete_lakes <- function(lakes, keep = TRUE){
  
  vect <- execGRASS("g.list",
                    parameters = list(
                      type = "vector"
                    ), intern = TRUE)
  
  if(lakes %in% vect){
    if(lakes != "lakes"){
    message(paste("Renaming", lakes, "to 'lakes'"))
    execGRASS("g.copy", flags = c("overwrite", "quiet"), 
              parameters = list(
                vector = paste0(lakes, ",lakes")
              ))
    }
  } else {
    message("Importing lakes ...")
    import_vector_data(data = lakes, name = "lakes")
  }
  
  vect <- execGRASS("g.list",
             parameters = list(
               type = "vector"
             ), intern = TRUE)
  if(!("lakes" %in% vect))
    stop("Import of lakes vector map failed. Please check.")
  
  orig_stream_ids <- as.numeric(as.character(execGRASS("v.db.select",
                                                  parameters = list(
                                                    map = "streams_v",
                                                    columns = "stream"
                                                  ), 
                                                  ignore.stderr = TRUE, 
                                                  intern = TRUE)[-1]))
  if(keep == TRUE){
    execGRASS("g.copy", flags = c("quiet", "overwrite"),
              parameters = list(
                vector = "streams_v,streams_v_prev_lakes"
              ))
    # execGRASS("g.copy", flags = c("quiet", "overwrite"),
    #           parameters = list(
    #             vector = "streams_v_prev_lakes,streams_v,"
    #           ))
  }
  
  message("Deleting streams intersecting with lakes ...")
  # delete streams the intersect with lakes
  execGRASS("v.overlay", flags = c("quiet", "overwrite"),
            parameters = list(
              ainput = "streams_v", 
              binput = "lakes",
              blayer = "1",
              operator = "not",
              output = "streams_wo_lakes"
            ))
  
  # get start and end coordinates of streams
  execGRASS("v.db.addcolumn", parameters = list(
    map = "streams_wo_lakes",
    columns = "end_x double,end_y double,start_x double,start_y double,new_length double"
  ))
  execGRASS("v.to.db", flags = c("quiet"), 
            parameters = list(
              map = "streams_wo_lakes", 
              type = "line", 
              option = "end",
              columns = "end_x,end_y"
            ))
  execGRASS("v.to.db", flags = c("quiet"), 
            parameters = list(
              map = "streams_wo_lakes", 
              type = "line", 
              option = "start",
              columns = "start_x,start_y"
            ))
  execGRASS("v.to.db", flags = c("quiet"), 
            parameters = list(
              map = "streams_wo_lakes", 
              type = "line", 
              option = "length",
              columns = "new_length"
            ))

  sink("temp.txt")
  streams <- readVECT("streams_wo_lakes")
  sink()
  
  ###################
  ## does not work!
  # execGRASS("v.to.rast", flags = c("overwrite", "quiet"),
  #           parameters = list(
  #             input = "streams_wo_lakes",
  #             type = "line",
  #             output = "streams_r_wo_lakes",
  #             use = "cat"
  #           ))
  # execGRASS("r.stream.order",
  #           flags = c("overwrite", "z"), #"quiet", "m"
  #           parameters = list(stream_rast = "streams_r_wo_lakes",     # input
  #                             direction = "dirs",            # input
  #                             elevation = "dem_cond",      # input
  #                             accumulation = "accums",       # input
  #                             stream_vect = "streams_v_order_wo_lakes"))    # output
  #          # ignore.stderr=T)
  # ###################
  
  execGRASS("g.remove", flags = c("f", "quiet"),
            parameters = list(
              type = "vector",
              name = "streams_wo_lakes"
            ))
  dt.streams <- data.table(streams@data)
  dt.streams[, colnames(dt.streams)[grep("^b_", colnames(dt.streams))] := NULL]
  dt.streams[, which(colnames(dt.streams) %in% c("a_cat", "a_cat_")) := NULL]
  colnames(dt.streams) <- gsub("^a_", "", colnames(dt.streams))
  dt.streams[, end_xy := paste(end_x, end_y, sep = "_")]
  dt.streams[, start_xy := paste(start_x, start_y, sep = "_")]
  
  # position of duplicated (= cut in several pieces) streams
  dup_streams <- which(duplicated(dt.streams$stream))
  
  mstream <- max(dt.streams$stream) + 1
  new_str <- seq(mstream, by = 1, length.out = length(dup_streams))
  dt.streams$str_new_lake <- dt.streams$stream
  dt.streams[dup_streams, str_new_lake := as.integer(new_str)]
  # stream of cut streams (= original start or end lies within lake)
  dup_streams <- unique(dt.streams[dup_streams, stream, ])
  cut_streams <- dt.streams[new_length < length & !(stream %in% dup_streams), stream ]
  i0 <- which(dt.streams$stream %in% cut_streams)

  message("Updating topology ...")
  # update topology
  
  # 1. segments cut and / or deleted, no duplicates
  # 1.1 check, if next_str exists
  ii <- which(dt.streams[i0, next_str] %in% dt.streams$stream)
  ## if not, set next_str to -1
  if(length(ii) < length(i0) & length(ii) > 0)
  dt.streams[i0[-ii], next_str := -1]
  dt.streams[i0[-ii], changed := 1]
  
  ## if yes, check if end_coor = start_coor of next_str
  iii <- which(!unlist(lapply(i0[ii], function(x) dt.streams$end_xy[x] == dt.streams[stream == dt.streams[x, next_str], start_xy])))
  ### if not set prev_str of next_str to 0 and next_str of cut_str to -1
  if(length(iii) > 0){
    dt.streams[stream %in% dt.streams$next_str[i0[ii[iii]]], c("prev_str01", "prev_str02") := 0]
    dt.streams[i0[ii[iii]], next_str := -1]
    dt.streams[i0[ii[iii]], changed := 1]
  }
  # 1.2 check, if prev_str exist
  i1 <- which(dt.streams[i0, prev_str01] %in% dt.streams$stream)
  i2 <- which(dt.streams[i0, prev_str02] %in% dt.streams$stream)
  # if not, set prev_str to 0 (it can happen, that one prev_str is deleted, onother is cut, i.e. i1 != i2)
  if(length(i1) < length(i0) & length(i1) > 0){
    dt.streams[i0[-i1], prev_str01 := 0]
    dt.streams[i0[-i1], changed := 1]
  }
  
  if(length(i2) < length(i0) & length(i2) > 0)
    dt.streams[i0[-i2], prev_str02 := 0]
  ii1 <- which(!unlist(lapply(i0[i1], function(x) dt.streams$start_xy[x] == dt.streams[stream == dt.streams[x, prev_str01], end_xy])))
  ii2 <- which(!unlist(lapply(i0[i2], function(x) dt.streams$start_xy[x] == dt.streams[stream == dt.streams[x, prev_str02], end_xy])))
  if(length(ii1) > 0){
    dt.streams[stream %in% dt.streams$prev_str01[i0[i1[ii1]]], next_str := -1]
    dt.streams[i0[i1[ii1]], prev_str01 := 0]
    dt.streams[i0[i1[ii1]], changed := 1]
  }
  if(length(ii2) > 0){
    dt.streams[stream %in% dt.streams$prev_str02[i0[i2[ii2]]], next_str := -1]
    dt.streams[i0[i2[ii2]], prev_str02 := 0]
    dt.streams[i0[i2[ii2]], changed := 1]
    
  }
  
  # 2. duplicate stream segments
  for(i in dup_streams){
    print(i)
    i0 <- which(dt.streams$stream == i)
    # not '==' but '%in% because next_str could be cut into pieces, too, and therefore there are more than one start_xy
    i_wnext <- which(dt.streams[i0, end_xy] %in% dt.streams[stream %in% unique(dt.streams[stream == i, next_str]), start_xy])
    if(length(i_wnext) > 0){
      if(length(i_wnext) < length(i0)){
        dt.streams[i0[-i_wnext], next_str := -1]
      }
      i_next <- which(dt.streams$stream == dt.streams[i0[i_wnext], next_str])
      if(length(i_next) == 1){
        i_p <- which(dt.streams[i_next, c("prev_str01","prev_str02")] == i)
        dt.streams[i_next, c("prev_str01","prev_str02")[i_p] := dt.streams[i0[i_wnext], str_new_lake]]
        dt.streams[i_next, changed := 1]
      } else {
        
        ## find, where end_coor = start_coor of next_str
        iii <- which(unlist(lapply(i_next, function(x) dt.streams$start_xy[x] == dt.streams[i0[i_wnext], end_xy])))
        ### if not set prev_str of next_str to 0 and next_str of cut_str to -1
        if(length(iii) > 0){
          dt.streams[i0[i_wnext], next_str := i_next[iii]] 
          i_p <- which(dt.streams[i_next[iii], c("prev_str01","prev_str02")] == i)
          dt.streams[i_next, c("prev_str01","prev_str02")[i_p] := dt.streams[i0[i_wnext], str_new_lake]]
          dt.streams[i_next, changed := 1]
          
          ### NOT FINISHED!!!
        }
        
        i_p0 <- unlist(lapply(i_next, function(x) which(dt.streams[x, c("prev_str01","prev_str02")] == i)))
        i_p1 <- which(dt.streams[i_next, start_xy] == dt.streams[i0[i_wnext], end_xy])
        dt.streams[i_next[i_p1], c("prev_str01","prev_str02")[i_p0[i_p1]] := dt.streams[i0[i_wnext], str_new_lake]]
        dt.streams[i_next[i_p1], changed := 1]
      }
    }
    # not '==' but '%in% because prev_str could be cut into pieces, too, and therefore there are more than one start_xy
    i_wp1 <- which(dt.streams[i0, start_xy] %in% dt.streams[stream %in% unique(dt.streams[stream == i, prev_str01]), end_xy])
    if(length(i_wp1) > 0){
      if(length(i_wp1) < length(i0)){
        dt.streams[i0[-i_wp1], prev_str01 := 0]
      }
      i_p1 <- which(dt.streams$stream == dt.streams[i0[i_wp1], prev_str01])
      if(length(i_p1) != 1){
        i_p1  <- i_p1[which(dt.streams[i_p1, end_xy] == dt.streams[i0[i_wp1], start_xy])]
      }
      dt.streams[i_p1, next_str := dt.streams[i0[i_wp1], str_new_lake]]
      dt.streams[i_p1, changed := 1]
    } else {
      dt.streams[i0, prev_str01 := 0]
    }
    # not '==' but '%in% because prev_str could be cut into pieces, too, and therefore there are more than one start_xy
    i_wp2 <- which(dt.streams[i0, start_xy] %in% dt.streams[stream %in% unique(dt.streams[stream == i, prev_str02]), end_xy])
    if(length(i_wp2) > 0){
      if(length(i_wp2) < length(i0)){
        dt.streams[i0[-i_wp2], prev_str02 := 0]
      }
      i_p2 <- which(dt.streams$stream == dt.streams[i0[i_wp2], prev_str02])
      if(length(i_p2) != 1){
        i_p2  <- i_p2[which(dt.streams[i_p2, end_xy] == dt.streams[i0[i_wp2], start_xy])]
      }
      dt.streams[i_p2, next_str := dt.streams[i0[i_wp1], str_new_lake]]
      dt.streams[i_p2, changed := 1]
    } else {
      dt.streams[i0, prev_str02 := 0]
    }
    
    dt.streams[i0, stream := str_new_lake]  
    dt.streams[i0, changed := 1] 
    print(colnames(dt.streams))
  }
  
  next_str <- sort(unique(dt.streams[, next_str]))[-1]
  no_str <- next_str[which(is.na(match(next_str, dt.streams$stream)))]
  dt.streams[next_str %in% no_str, next_str := -1]
  
  # # find streams that do not exist any more and update topology
  # stream_ids <- as.numeric(as.character(execGRASS("v.db.select",
  #                                                 parameters = list(
  #                                                   map = "streams_v",
  #                                                   columns = "stream"
  #                                                 ), intern = T)[-1]))
  # stream_del <- stream_ids[which(!(stream_ids %in% dt.streams$stream))]
  # dt.streams[prev_str01 %in% stream_del, `:=`(prev_str01 = 0, changed = 1)]
  # dt.streams[prev_str02 %in% stream_del, `:=`(prev_str02 = 0, changed = 1)]
  # dt.streams[next_str %in% stream_del, `:=`(next_str = -1, changed = 1)]
  
  message("Updating out_dist ...")
  dt.streams[, length := new_length]
  dt.streams[, out_dist := 0]
  outlets <- dt.streams[next_str == -1, stream]
  for(i in outlets){
    calc_outdist(dt.streams, id=i)
  }
  
  
  # delete unneeded columns
  dt.streams[, c("end_x", "end_y", "start_x", "start_y", "end_xy", "start_xy", "str_new_lake", "new_length") := NULL]
  # save updated streams_v
  streams@data <- data.frame(dt.streams)
  sink("temp.txt")
  writeVECT(streams, "streams_v", v.in.ogr_flags = c("overwrite", "quiet", "o"), ignore.stderr = TRUE)
  sink()
  
  # message("Original stream raster moved to streams_r_prev_lakes.\n")
  # execGRASS("g.copy",
  #           flags = c("overwrite", "quiet"),
  #           parameters = list(
  #             raster = "streams_r,streams_r_prev_lakes"))
  # 
  # # use unique "cat" automatically assigned be writeVECT
  # execGRASS("v.to.rast", flags = c("overwrite", "quiet"),
  #           parameters = list(
  #             input = "streams_v",
  #             type = "line",
  #             output = "streams_r",
  #             use = "attr",
  #             attribute_column = "cat"
  #           ))
  try(unlink("temp.txt"), silent = TRUE)
}


#' calc_outdist
#' @title Update flow length from the upstream node of each stream segment 
#'   to the outlet of the network .
#'
#' @description Recursive function to calculate the flow length from the 
#' upstream node of the stream segment to the outlet of the network.
#' It is called by \code{\link{delete_lakes}} for each
#' outlet and should not be called by the user.

#' @param dt data.table containing the attributes of the stream segments
#' @param id integer; 'stream' of the stream segment
#' @keywords internal
#'
#' @return nothing
#'
#' @author Mira Kattwinkel, \email{mira.kattwinkel@@gmx.net}
#'
calc_outdist <- function(dt, id){
  #print(id)
  dt[stream == id, out_dist := out_dist + length]
  if(dt[stream == id, prev_str01,] != 0){  # check only one of prev01 and prev02 because they are always both 0
    dt[stream == dt[stream == id, prev_str01], out_dist := dt[stream == id, out_dist]]
    dt[stream == dt[stream == id, prev_str02], out_dist := dt[stream == id, out_dist]]
    calc_outdist(dt, dt[stream == id, prev_str01])
    calc_outdist(dt, dt[stream == id, prev_str02])
  }
}
