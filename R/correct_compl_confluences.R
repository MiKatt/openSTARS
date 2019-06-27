#' Correct confluences with three or more inflows.
#'
#' At complex confluences (when more than two line segments flow into a node,
#' i.e. more than two inflows to an outflow), the end of one of the inflows is moved a 
#' tiny bit upstream to one of the other inflows to create a new confluence of
#' two streams (see details below).
#' 
#' @param clean logical; should intermediate files be removed from 'GRASS'
#'   session?
#'   
#' @return Nothing. The function changes features in 'streams_v'. Changed features are
#' marked in the new column 'changed'.
#
#' @details At complex confluences (when more than two line segments flow into a node,
#' i.e. more than two inflows to an outflow), new confluences of only two streams 
#' are created:
#' 1. complex confluences are found based on the fact that the outflow has more than
#' two previous streams
#' 2. the inflow with the shortest cummulative length from its source  is found; 
#' the end of this segment will be moved
#' 3. the inflow with the smallest angle to this inflow is found;
#' this segment will be cut into tow segments close to the juntion using the GRASS function
#' \href{https://grass.osgeo.org/grass74/manuals/v.edit.html}{v.edit}(tool =
#' break) creating a new confluence
#' 4. the shortest inflow found in 2 is moved to the newly created confluence using
#' \href{https://grass.osgeo.org/grass74/manuals/v.edit.html}{v.edit}(tool =
#' vertexmove)
#' 5. all lengths are updated (segment length, cumulative length, i.e. length of the stream
#' from the source, distance to the outlet).
#' The distance the shortest confluence is moved depends on the number of inflows. For three 
#' inflows, it is moved 1/12 time the DEM cellsize upstream, for seven (the extremly rare maximum)
#' 5/12 * cellsize.
#'
#' @note \code{\link{setup_grass_environment}}, \code{\link{import_data}} and
#'   \code{\link{derive_streams}} must be run before.
#'
#' @author Mira Kattwinkel \email{mira.kattwinkel@@gmx.net}
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
#' streams_path <- system.file("extdata", "nc", "streams.shp", package = "openSTARS")
#' setup_grass_environment(dem = dem_path)
#' import_data(dem = dem_path, sites = sites_path, streams = streams_path)
#' gmeta()
#'
#' # Derive streams from DEM
#' derive_streams(burn = 10, accum_threshold = 100, condition = TRUE, clean = TRUE)
#' 
#' # Check and correct complex confluences (there are complex confluences in the 
#' # example date set if the accumulation threshold is low)
#' cj <- check_compl_confluences()
#' if(cj){
#'   correct_compl_confluences()
#' }
#' 
#' # plot
#' dem <- readRAST('dem', ignore.stderr = TRUE)
#' streams <- readVECT('streams_v', ignore.stderr = TRUE)
#' streams_orig <- readVECT('streams_v_o3', ignore.stderr = TRUE)
#' # zoom to a relevant part of the dem
#' plot(dem, col = terrain.colors(20), axes = TRUE, 
#'   xlim = c(640100,640150), ylim = c(219735,219785))
#' lines(streams_orig, col = 'red', lwd = 4)
#' lines(streams, col = 'blue', lty = 2, lwd = 2)
#' legend("bottomright", col = c("red", "blue"), lty = c(1,2), lwd = c(4,2), 
#'   legend = c("original", "corrected"))
#'   
#' plot(streams, col = c("blue", "red")[streams@data$changed+1], lty = 1, lwd = 2)   
#' }

correct_compl_confluences <- function(clean = TRUE){
  cnames<-execGRASS("db.columns",
                    parameters = list(
                      table = "streams_v"
                    ), intern=T)
  
  # temporary directory
  temp_dir <- tempdir()

  # get cellcize of dem raster
  cellsize <- execGRASS("g.region", flags = "p",intern=T)
  cellsize <- as.numeric(do.call(rbind,strsplit(cellsize[grep("res",cellsize)],split=":"))[,2])
  cellsize <- min(cellsize)

  # get maximum prev_str0X from colnames
  max.prev.str <- max(which(paste0("prev_str0", 3:7) %in% cnames)) + 2
  
  # Add column to mark changed features
  execGRASS("v.db.addcolumn", flags = c("quiet"),
            parameters = list(
              map = "streams_v",
              columns = "changed int"
            ))
  # Set 'changed' to '0' for all streams
  execGRASS("v.db.update", flags = c("quiet"),
            parameters = list(
              map = "streams_v",
              column = "changed",
              value = "0"
            ))
  
 for(i in max.prev.str:3){
   # get all segments that are inflows to i-fold complex junction
   dt.junctions <- do.call(rbind, strsplit(
     execGRASS("db.select", 
               parameters = list(
                 sql = paste0("select ", paste0("prev_str0", 1:i, collapse = ", "),  " from streams_v where prev_str0", i, " > 0")
               ), intern = T), 
     split = "\\|"))
   colnames(dt.junctions) <- dt.junctions[1, ]
   dt.junctions <- data.frame(dt.junctions[-1, , drop = F], 
                              stringsAsFactors = FALSE)
   setDT(dt.junctions)
   dt.junctions[, `:=`(names(dt.junctions), lapply(.SD, as.numeric))]
   n <- nrow(dt.junctions)
   dt.junctions <- melt(dt.junctions, measure.vars = colnames(dt.junctions))
   #message(paste0("Removing ",n , " ", i, "-fold confluences."))
   message(paste0("Fixing ",n , " complex confluences with ", i, " upstream segments each"))
   
   # all previous streams
   prev_str <- unique(dt.junctions[, value])
   i0 <- which(prev_str == 0)
   if(length(i0) > 0){
     prev_str <- prev_str[-i0]
   }
   prev_str <- paste0("(", paste(sort(prev_str), collapse = ","), ")", sep = "")
   
   # Create new vector map with all segments that form complex junctions
   execGRASS("v.extract", flags = c("overwrite", "quiet"), 
             parameters = list(
               input = "streams_v", 
               output = "complex_flows", 
               type = "line", 
               where = paste0("stream in ", prev_str)),
             ignore.stderr = T, intern = TRUE
   )
   
   # Create file of point positions 0.8 x cellsize  upstream of junction to get the 
   # flow direction close to the juction; cellsize as coordinate does not work, because some 
   # segements are only one cellsize long; 0.8 lays in the next cell
   points <- file.path(temp_dir, "complex_points.txt")
   dt <- data.table(do.call(rbind, strsplit(
     execGRASS("db.select", 
               parameters = list(
                 sql = paste0("select cat, stream, length, cum_length from streams_v where stream in", prev_str)
               ), intern = T), 
     split = "\\|"))[-1, ])
   setattr(dt, "names", c("cat", "stream", "len", "cum_length"))
   dt[, `:=`(names(dt), lapply(.SD, as.numeric))]
   dt <- merge(dt, dt.junctions, by.y = "value", by.x = "stream")
   #setnames(dt, "value", "stream")
   dt[, newlen := -0.8 * cellsize]
   dt[len < (0.8 * cellsize), newlen:=-len]
   dt[, `:=`(pcat, seq(1, nrow(dt)))]
   write(paste(paste("P ", dt[, pcat], c(dt[, cat]), dt[, newlen], collapse = "\n")), 
         file = points)
   
   # Create point feature with points on complex flows based on points created above
   execGRASS("v.segment", flags = c("overwrite", "quiet"), 
             parameters = list(
               input = "complex_flows", 
               output = "complex_flows_p",
               rules = points),
             ignore.stderr = TRUE, intern =TRUE
   )
   # Get flow direction at these points
   execGRASS("v.db.addtable", flags = c("quiet"), 
             parameters = list(
               map = "complex_flows_p", 
               columns = "dir int"),
             ignore.stderr = TRUE
   )
   execGRASS("v.what.rast", flags = c("quiet"), 
             parameters = list(
               map = "complex_flows_p", 
               raster = "dirs", 
               column = "dir")
   )
   # Get coordinates of ends of juction inflow segments 
   dt.all_inflows <- data.frame(do.call(rbind, strsplit(
     execGRASS("v.to.db", flags = c("p", "quiet"), 
               parameters = list(
                 map = "complex_flows", 
                 type = "line", 
                 option = "end"
               ), intern = T), 
     split = "\\|"))[,-4], stringsAsFactors = FALSE)
   setDT(dt.all_inflows)
   setattr(dt.all_inflows, "names", c("cat", "end_x", "end_y"))
   dt.all_inflows[, `:=`(names(dt.all_inflows), lapply(.SD, as.numeric))]
   setkey(dt.all_inflows, cat)
   
   # Merge endcoordinates with flow directions one cell above juction
   # faster than readVECT
   dt.dirs <- do.call(rbind, strsplit(
     execGRASS("db.select", 
               parameters = list(
                 sql = "select * from complex_flows_p"
               ), intern = T), 
     split = "\\|"))
   colnames(dt.dirs) <- dt.dirs[1, ]
   dt.dirs <- data.frame(dt.dirs[-1, , drop = F], 
                         stringsAsFactors = FALSE)
   setDT(dt.dirs)
   dt.dirs[, `:=`(names(dt.dirs), lapply(.SD, as.numeric))]
   dt.dirs <- merge(dt, dt.dirs, by.y = "cat", by.x = "pcat")
   dt.all_inflows <- merge(dt.dirs, dt.all_inflows, by = "cat")
   
   rm(list = c("dt.dirs", "dt", "dt.junctions"))
   
   dt.junctions <- do.call(rbind, strsplit(
     execGRASS("db.select", 
               parameters = list(
                 sql = paste0("select stream ,", paste0("prev_str0", 1:i, collapse = ", "), " from streams_v where prev_str0", i, " > 0")
               ), intern = T), 
     split = "\\|"))
   colnames(dt.junctions) <- dt.junctions[1, ]
   dt.junctions <- data.frame(dt.junctions[-1, , drop = F], 
                              stringsAsFactors = FALSE)
   setDT(dt.junctions)
   dt.junctions[, `:=`(names(dt.junctions), lapply(.SD, as.numeric))]
   
    # Find the two prev_str with the smallest difference in flow directions -> 'cut_stream' and 'move_stream'
    # "This gives a signed angle for any angles:
    # a = targetA - sourceA
    # a = (a + 180) % 360 - 180"
    # Adjusted for directions (1:8)
    # Direction is "Flow direction is of D8 type with a range of 1 to 8. 
    # Multiplying values with 45 gives degrees CCW [counter clock wise] from East" (r.stream.extraxt help)
    # 8 -> E, 6 -> S, 4 -> W, 2 -> N
    df.move_streams <- data.frame(matrix(nrow = nrow(dt.junctions), ncol = 5))
    colnames(df.move_streams) <- c("stream", "move_stream", "move_stream_prev", "cut_stream", "cut_stream_prev")
    
    for (j in 1:nrow(dt.junctions)) {
      dt <- dt.all_inflows[stream %in% dt.junctions[j, paste0("prev_str0", 1:i), with = F]]
      move.str <- which.min(dt$cum_length)
      dt[-move.str,  `:=`(dif, abs((dt[-move.str, dir] - dt[move.str, dir] + 4)%%8 - 4))]
      cut.str <- which.min(dt[, dif])
      df.move_streams[j, ] <- unlist(c(dt.junctions[j, stream], dt[move.str, .(stream, variable)], dt[ cut.str, .(stream, variable)]))
    }
    ## variable != "stream" because one stream can be stream and prev_stream in compl junctions; should not be duplicated here.
    dt.xy_move <- dt.all_inflows[stream %in% df.move_streams[, "move_stream"], .(stream, end_x, end_y)]
    df.move_streams <- merge(df.move_streams, dt.xy_move, by.x = "move_stream", 
                             by.y = "stream")
    colnames(df.move_streams)[colnames(df.move_streams) == "end_x"] <- "move_end_x"
    colnames(df.move_streams)[colnames(df.move_streams) == "end_y"] <- "move_end_y"
    #df.move_streams[,"move_stream"] <- as.numeric(as.character(df.move_streams[, "move_stream"]))
    #df.move_streams[,"cut_stream"] <- as.numeric(as.character(df.move_streams[, "cut_stream"]))
    df.move_streams <- df.move_streams[order(df.move_streams$cut_stream),]
    df.move_streams$cut_stream_cat <- dt.all_inflows[stream %in% df.move_streams[, "cut_stream"], cat]

    # Create file of point positions (2 - prev_str0X) * 1/12 cellsize upstream of end of outflow to cut outflow
    # P <point id>   <line cat> <offset> [<side offset>]
    points <- file.path(temp_dir, "cut_points.txt")
    # same as -cellsize/(2 * 6) * (5-(i+3)) but shorter
    write(paste(paste("P ", df.move_streams[, "cut_stream_cat"], df.move_streams[, "cut_stream_cat"], c(rep(cellsize/(12) * (2-i), nrow(df.move_streams)))), collapse = "\n"), 
          file = points)
    execGRASS("v.segment", flags = c("overwrite", "quiet"), 
              parameters = list(
                input = "complex_flows",
                output = "complex_flows_cp", 
                rules = points),
              ignore.stderr = TRUE, intern = TRUE
    )
    dt.cut_coords <- data.table(do.call(rbind, strsplit(
      execGRASS("v.out.ascii", flags = c("overwrite", "quiet"), 
                parameters = list(
                  input = "complex_flows_cp"
                ), intern = TRUE, ignore.stderr = TRUE), 
      split = "\\|")))
    dt.cut_coords[, `:=`(names(dt.cut_coords), lapply(.SD, as.numeric))]
    names(dt.cut_coords) <- c("cut_x", "cut_y", "cut_stream_cat")
    df.move_streams <- merge(df.move_streams, dt.cut_coords, by = "cut_stream_cat")
    
    # Save originally derived network to streams_v_oX
    execGRASS("g.copy", flags = c("overwrite", "quiet"), 
              parameters = list(
                vector = paste0("streams_v,streams_v_o", i)
              ), ignore.stderr = TRUE, intern = TRUE)
    
    message(paste0("Original stream topology file moved to 'streams_v_o", i, "'."))
    message("Breaking lines and moving vertices ...")
    
    # Break features at cut coordinates
    for(j in 1:nrow(dt.junctions)){
      execGRASS("v.edit", flags = c("quiet", "overwrite"), 
                parameters = list(
                  map = "streams_v", 
                  type = "line", 
                  tool = "break", 
                  threshold = 1, 
                  coords = c(dt.cut_coords[j, c(cut_x, cut_y)])
                ))
    }
    # Move end vertices of move streams to cut coordinates
    for(j in 1:nrow(df.move_streams)){
      execGRASS("v.edit", flags = c("quiet", "overwrite"), 
                parameters = list(
                  map = "streams_v",
                  type = "line", 
                  tool = "vertexmove",
                  threshold = c(1, cellsize/4, 0), 
                  where = paste0("stream = ", df.move_streams[j,"move_stream"]), 
                  coords = c(df.move_streams[j, "move_end_x"], df.move_streams[j, "move_end_y"]), 
                  move = c(df.move_streams[j, "cut_x"] - df.move_streams[j, "move_end_x"], df.move_streams[j, "cut_y"] - df.move_streams[j, "move_end_y"], 0), 
                  snap = "node"
                ))
    }
    
    # Seems to be the easiest way to assingn new, unique cat values to all features
    sink("temp.txt")
    streams <- readVECT(vname = "streams_v", remove.duplicates = FALSE, 
                        ignore.stderr = TRUE, type = "line")
    sink()
    # MiKatt 20190416: not needed any more, only for debugging
    #streams@data <- data.frame(streams@data, streams$cat)
    #colnames(streams@data)[ncol(streams@data)] <- paste0("cat_old", i) 

    nstream <- max(streams$stream) + 1
    streams$str_new <- streams$stream
    for (j in 1:nrow(df.move_streams)) {
      k <- which(streams$cat == df.move_streams[j, "cut_stream_cat"])
      if (!length(k) > 1) {
        print(j)
      }
      else {
        streams$str_new[k[2]] <- nstream
        nstream <- nstream + 1
      }
    }
    # writeVECT produces new cat column; remove "cat_" from previous loop
    if("cat_" %in% colnames(streams@data)){
      streams@data <- streams@data[- which(colnames(streams@data) == "cat_")]
    }
    sink("temp.txt")
    writeVECT(streams, "streams_v", v.in.ogr_flags = c("overwrite", "quiet", "o"), ignore.stderr = TRUE)
    sink()
    rm("streams")
    
    # Recalculate length of line segments
    execGRASS("v.db.addcolumn", flags = "quiet",
              parameters = list(
                map = "streams_v",
                columns = "length_new double precision"
              ))
    execGRASS("v.to.db", flags = c("quiet"),
              parameters = list(
                map = "streams_v",
                option = "length",
                type = "line",
                columns = "length_new"
              ), ignore.stderr = TRUE)
    # Find new cat_ and new_str of short and long pieces of cut streams
    cut.str <- paste(df.move_streams[, "cut_stream"], collapse = ",")
    cut.str<-paste0("(", cut.str, ")",sep="")
    dt.cut <- do.call(rbind,strsplit(
      execGRASS("db.select",
                parameters = list(
                  sql = paste0("select stream, length, length_new, cat_, str_new from streams_v where stream in", cut.str)
                ),intern = T),
      split = '\\|'))
    colnames(dt.cut)<-dt.cut[1,]
    dt.cut <- data.table(dt.cut[-1,])
    dt.cut[, `:=`(names(dt.cut), lapply(.SD, as.numeric))]
    dt.smallcut <- dt.cut[dt.cut[, .I[length_new == min(length_new)], by=stream]$V1]
    setnames(dt.smallcut,"str_new","str_new_small")
    setnames(dt.smallcut,"cat_","cat_small")
    dt.largecut <- dt.cut[dt.cut[, .I[length_new == max(length_new)], by=stream]$V1]
    setnames(dt.largecut,"str_new","str_new_large")
    setnames(dt.largecut,"cat_","cat_large")
    # find segments with idential length, i.e. in both largecut and smallcut
    if(any(c(nrow(dt.largecut), nrow(dt.smallcut)) > nrow(dt.junctions))){
      i1 <- which(dt.largecut$str_new_large %in% dt.smallcut$str_new_small)
      i2 <- which(dt.smallcut$str_new_small %in% dt.largecut$str_new_large)
      # cat of end piece (i.e. new segment between outflow and move_stream) has the higher cat
      # TODO: is that always the case?
      dt.smallcut <- dt.smallcut[ -dt.smallcut[i2, .I[str_new_small == min(str_new_small)], by = stream]$V1]
      dt.largecut <- dt.largecut[ -dt.largecut[i1, .I[str_new_large == max(str_new_large)], by = stream]$V1]
    }
    df.move_streams <- merge(df.move_streams, dt.smallcut[, .(stream, cat_small, str_new_small)], by.x = "cut_stream", by.y = "stream")
    df.move_streams <- merge(df.move_streams, dt.largecut[, .(stream, cat_large, str_new_large)], by.x = "cut_stream", by.y = "stream")
    dt.move_streams <- data.table(df.move_streams)
    remove(df.move_streams)

    # assign updated stream (str_new) value to 'stream' for cut stream segments
    cut.str<-paste(c(dt.smallcut[, cat_small], dt.largecut[, cat_large]), collapse = ",")
    cut.str<-paste0("(", cut.str, ")",sep="")
    execGRASS("v.db.update", flags = c("quiet"),
              parameters = list(
                map = "streams_v",
                column = "stream",
                where = paste0("cat_ in ",cut.str),
                query_column = "str_new"
              ))
    execGRASS("v.db.dropcolumn", flags = "quiet",
              map = "streams_v",
              columns = "str_new"
              )

    message("Updating topology ...")

    # Using data.table is about 10 times faster than execGRASS(v.db.update)   
    streams <- readVECT(vname = "streams_v", remove.duplicates = FALSE, 
                        ignore.stderr = TRUE, type = "line")
    dt.streams <- data.table(streams@data)
    
    # Must all be in the loop over all junctions, because otherwise the sorting is wrong in the different tables
    for(j in 1:nrow(dt.junctions)){
      jj <- which(dt.move_streams$stream == dt.junctions[j, stream])
      
      # set stream's cut_stream_prev to cat_small if cut_stream_prev < i
      if(dt.move_streams[jj, cut_stream_prev] != i){
        dt.streams[stream == dt.junctions[j, stream], paste0("prev_str0", dt.move_streams[jj, cut_stream_prev]) := dt.move_streams[jj, str_new_small]]
      } else if(dt.move_streams[jj, move_stream_prev] != i){
        # else set stream's move_stream_prev to cat_small if move_stream_prev < i
        dt.streams[stream == dt.junctions[j, stream], paste0("prev_str0", dt.move_streams[jj, move_stream_prev]) := dt.move_streams[jj, str_new_small]]
      }
      # if both cut_str_prev and move_str_prev are != i, set move_str_prev to prev_str0i
      if(dt.move_streams[jj, cut_stream_prev] != i & dt.move_streams[jj, move_stream_prev] != i){
        dt.streams[stream == dt.junctions[j, stream], paste0("prev_str0", dt.move_streams[jj, move_stream_prev]) := dt.junctions[j, paste0("prev_str0",i), with = F]]
      }
      
      # # set stream's cut_stream_prev to cat_small if cut_stream_prev < i
      # # else set stream's move_stream_prev to cat_small
      # if(dt.move_streams[jj, cut_stream_prev] != i){
      #   dt.streams[stream == dt.junctions[j, stream], paste0("prev_str0", dt.move_streams[jj, cut_stream_prev]) := dt.move_streams[jj, str_new_small]]
      # } else {
      #   # set stream's move_str_prev to prev_str0i
      #   if(dt.move_streams[jj, move_stream_prev] != i){
      #     #     dt.streams[stream == dt.junctions[j, stream], paste0("prev_str0", dt.move_streams[jj, move_stream_prev]) := dt.junctions[j, paste0("prev_str0",i), with = F]]
      #     #   } else {
      #     dt.streams[stream == dt.junctions[j, stream], paste0("prev_str0", dt.move_streams[jj, move_stream_prev]) := dt.move_streams[jj, str_new_small]]
      #   }
      # }

    # set prev_str01 of str_new_small to move_stream 
    dt.streams[stream == dt.move_streams[jj, str_new_small],  prev_str01 := as.integer(dt.move_streams[jj, move_stream])]
    # set prev_str02 of str_new_small to str_new_large
    dt.streams[stream == dt.move_streams[jj, str_new_small],  prev_str02 := as.integer(dt.move_streams[jj, str_new_large])]
    
    # set next_str of str_new_small to stream
    dt.streams[stream == dt.move_streams[jj, str_new_small],  next_str := as.integer(dt.junctions[j, stream])]
    
    # set prev_str0>2 of str_new_small to 0
    dt.streams[stream == dt.move_streams[jj, str_new_small],  paste0("prev_str0", i:3) := 0]
    
    # set next_str of str_new_large and move_str_prev to str_new_small
    dt.streams[stream %in% unlist(dt.move_streams[jj, .(str_new_large, move_stream)]),  next_str := as.integer(dt.move_streams[jj, str_new_small])]
    
    # recalculate cum_length
    # cum_length of move_stream: old cum_length - difference between old and new length
    dt.streams[stream == dt.move_streams[jj, move_stream], cum_length := cum_length - (length - length_new)]
    # cum_length of cut_large: old cum_length - difference between old and new length
    dt.streams[stream == dt.move_streams[jj, str_new_large], cum_length := cum_length - (length - length_new)]
    # cum_length of cut_small is old cum_length
      
    # recalculate out_dist
    # out_dist of move_stream: old out_dist + (new length + new length of cut_small - old length)
    length_cut_small <- dt.streams[stream == dt.move_streams[jj, str_new_small], length_new]
    dt.streams[stream == dt.move_streams[jj, move_stream], out_dist := out_dist - (length - length_new + length_cut_small)]
    # out_dist of cut_small: old out_dist - difference between old and new length
    dt.streams[stream == dt.move_streams[jj, str_new_small], out_dist := out_dist - (length - length_new)]
    # out_dist of cut_large is old out_dist
    
    # Set 'changed' to '1' for the changed streams
    str1<-unique(unlist(dt.move_streams[jj, c(move_stream, str_new_small, str_new_large, cut_stream)]))
    dt.streams[stream %in% str1,  changed := 1]
    
    }
    
    # delete column prev_str0X
    dt.streams[,  paste0("prev_str0", i) := NULL]
    
    dt.streams[, length := length_new]
    dt.streams[, length_new := NULL]
    
    if("cat_" %in% colnames(dt.streams)){
      dt.streams[,  cat_ := NULL]
    }
    streams@data <- data.frame(dt.streams)
    sink("temp.txt")
    writeVECT(streams, "streams_v", v.in.ogr_flags = c("overwrite", "quiet", "o"), ignore.stderr = TRUE)
    sink()
    rm("streams")
  }

  # message("Original stream raster moved to 'streams_r_o'.")
  # execGRASS("g.copy",
  #           flags = c("overwrite", "quiet"),
  #           parameters = list(
  #             raster = "streams_r,streams_r_o"))
  
  # # now use automatically assigned "cat"
  # execGRASS("v.to.rast", flags = c("overwrite", "quiet"),
  #           parameters = list(
  #             input = "streams_v",
  #             type = "line",
  #             output = "streams_r",
  #             use = "attr",
  #             attribute_column = "cat"
  #           ))
  
  invisible(file.remove(file.path(temp_dir,"complex_points.txt")))
  invisible(file.remove(file.path(temp_dir,"cut_points.txt")))

  if(clean){
    # Remove temporary vector files
    execGRASS("g.remove",
              flags = c("quiet", "f"),
              parameters = list(
                type = "vector",
                name = c("complex_flows","complex_flows_cp","complex_flows_p")
              ))
    try(unlink("temp.txt"), silent = TRUE)
  }
  message("Complex confluences were removed. Please check changed features in 'streams_v'.")
}
