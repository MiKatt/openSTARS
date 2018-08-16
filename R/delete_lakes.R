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
#' @param lakes character string or object; path to lake vector file (ESRI shape) 
#' or sp or sf data object.
#' @param save boolean; should the original 'streams_v' be saved? Default is TRUE.
#' 
#' @return Nothing. The function updates 'streams_v' and (if save = TRUE) saves 
#' the original file to streams_v_prev_lakes.
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
#' 
delete_lakes <- function(lakes, save = TRUE){
  
  proj_dem <- execGRASS("g.proj", flags = c("j", "f"),
                        intern = TRUE, ignore.stderr = TRUE)
  import_vector_data(data = lakes, name = "lakes", proj_dem = proj_dem)
  
  vect <- execGRASS("g.list",
             parameters = list(
               type = "vector"
             ), intern = T)
  
  if(!("lakes" %in% vect))
    stop("Import of lakes vector map failed. Please check.")
  
  orig_stream_ids <- as.numeric(as.character(execGRASS("v.db.select",
                                                  parameters = list(
                                                    map = "streams_v",
                                                    columns = "stream"
                                                  ), intern = T)[-1]))
  if(save == TRUE){
    execGRASS("g.copy", flags = c("quiet", "overwrite"),
              parameters = list(
                vector = "streams_v,streams_v_prev_lakes"
              ))
  }
  
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
    columns = "end_x double,end_y double,start_x double,start_y double"
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
  
  streams <- readVECT("streams_wo_lakes")
  execGRASS("g.remove", flags = c("f", "quiet"),
            parameters = list(
              type = "vector",
              name = "streams_wo_lakes"
            ))
  dt.streams <- data.table(streams@data)
  dt.streams[, colnames(dt.streams)[grep("^b_", colnames(dt.streams))] := NULL]
  dt.streams[, which(colnames(dt.streams) %in% c("a_cat", "a_cat_")) := NULL]
  colnames(dt.streams) <- gsub("^a_", "", colnames(dt.streams))
  
  # position of duplicated (= cut) streams
  dup_streams <- which(duplicated(dt.streams$stream))
  
  mstream <- max(dt.streams$stream) + 1
  new_str <- seq(mstream, by = 1, length.out = length(dup_streams))
  dt.streams$str_new_lake <- dt.streams$stream
  dt.streams[dup_streams, str_new_lake := as.integer(new_str)]
  # stream of cut  streams
  dup_streams <- unique(dt.streams[dup_streams, stream, ])
  
  # update topology
  for(i in dup_streams){
    i0 <- which(dt.streams$stream == i)
    i1 <- which(dt.streams$prev_str01 == i)
    i2 <- which(dt.streams$prev_str02 == i)
    i3 <- which(dt.streams$next_str == i)
    # change prev and next of outgoing and incomming streams of the cut streams
    if(length(i1) > 0){
      ii <- which(dt.streams[i0, end_x] == dt.streams[i1, start_x] & dt.streams[i0, end_y] == dt.streams[i1, start_y])
      if(length(ii) > 0){
        dt.streams[i2, prev_str01 := dt.streams[i0[ii], str_new_lake]]
        dt.streams[i0[-ii], next_str := -1]
        dt.streams[c(i0, i1), changed := 1]
      } 
    } else {
      if(length(i2) > 0){
        ii <- which(dt.streams[i0, end_x] == dt.streams[i2, start_x] & dt.streams[i0, end_y] == dt.streams[i2, start_y])
        if(length(ii) > 0){
          dt.streams[i2, prev_str02 := dt.streams[i0[ii], str_new_lake]]
          dt.streams[i0[-ii], next_str := -1]
          dt.streams[c(i0, i2), changed := 1]
        }
      }
    }
    if(length(i3) > 0){
      ii <- which(!(dt.streams[i3, end_x] %in% dt.streams[i0, start_x]) & !(dt.streams[i3, end_y] %in% dt.streams[i0, start_y]))
      if(length(ii) > 0){
        dt.streams[i3[ii], next_str := -1]
      }
    }
    # change prev and next of cut streams
    dt.streams$stream[i0] <-  dt.streams$str_new_lake[i0]
    i1 <- which(dt.streams$stream == unique(dt.streams[i0, prev_str01]))
    i2 <- which(dt.streams$stream == unique(dt.streams[i0, prev_str02]))
    i3 <-which(dt.streams$stream == unique(dt.streams[i0, next_str]))
    ii <- which(!(dt.streams[i0, start_x] == dt.streams[i1, end_x]) & !(dt.streams[i0, start_y] == dt.streams[i1, end_y]))
    dt.streams[i0[ii], prev_str01 := 0]
    dt.streams[i0[ii], changed := 1]
    ii <- which(!(dt.streams[i0, start_x] == dt.streams[i2, end_x]) & !(dt.streams[i0, start_y] == dt.streams[i2, end_y]))
    dt.streams[i0[ii], prev_str02 := 0]
    dt.streams[i0[ii], changed := 1]
    ii <- which(!(dt.streams[i0, end_x] == dt.streams[i3, start_x]) & !(dt.streams[i0, end_y] == dt.streams[i3, start_y]))
    dt.streams[i0[ii], next_str := -1]
    dt.streams[i0[ii], changed := 1]
  }
  
  # find streams that do not exist any more and update topology
  stream_ids <- as.numeric(as.character(execGRASS("v.db.select",
                                                  parameters = list(
                                                    map = "streams_v",
                                                    columns = "stream"
                                                  ), intern = T)[-1]))
  stream_del <- stream_ids[which(!(stream_ids %in% dt.streams$stream))]
  dt.streams[prev_str01 %in% stream_del, `:=`(prev_str01 = 0, changed = 1)]
  dt.streams[prev_str02 %in% stream_del, `:=`(prev_str02 = 0, changed = 1)]
  dt.streams[next_str %in% stream_del, `:=`(next_str = -1, changed = 1)]
  
  # delete unneeded columns
  dt.streams[, c("end_x", "end_y", "start_x", "start_y", "str_new_lake") := NULL]
  
  # save updated streams_v
  streams@data <- data.frame(dt.streams)
  sink("temp.txt")
  writeVECT(streams, "streams_v", v.in.ogr_flags = c("overwrite", "quiet", "o"), ignore.stderr = TRUE)
  sink()
}
