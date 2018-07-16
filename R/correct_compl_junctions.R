#' Correct junctions with three inflows.
#'
#' At complex junctions (i.e. more than two inflows to an outflow),the inflow
#' with the smalles angle to the shortes inflow is broken into two segments 
#' close to the juntion using the GRASS function
#' \href{https://grass.osgeo.org/grass74/manuals/v.edit.html}{v.edit}(tool =
#' break). Then, the shortes inflow is moved to this new junction using
#' \href{https://grass.osgeo.org/grass74/manuals/v.edit.html}{v.edit}(tool =
#' vertexmove). 
#' 
#' @param clean logical; should intermediate files be removed from 'GRASS'
#'   session?
#' @param celltoldig integer; number of digits the cell size dimensions are
#'  rounded to before it is checked whether they are identical
#'
#' @return Nothing. The function changes features in \itemize{
#'   \item{'streams_v':} {Updated streams with topology (vector)}
#'   \item{'streams_r':} {Updated stream raster (new cat) (raster)} } and copies
#'   the original to \itemize{ \item{'streams_v_o':} {Originally derived streams
#'   with topology (vector)} \item{'streams_r_o':} {Originally derived stream
#'   raster (raster).} }
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
#' # plot
#' dem <- readRAST('dem', ignore.stderr = TRUE)
#' streams <- readVECT('streams_v', ignore.stderr = TRUE)
#' streams_orig <- readVECT('streams_v_o', ignore.stderr = TRUE)
#' # zoom to a relevant part of the dem
#' plot(dem, col = terrain.colors(20), axes = TRUE, 
#'   xlim = c(640050,640200), ylim = c(219700,219850))
#' lines(streams_orig, col = 'red', lwd = 4)
#' lines(streams, col = 'blue', lty = 2, lwd = 2)
#' legend("topright", col = c("red", "blue"), lty = c(1,2), lwd = c(4,2), 
#'   legend = c("original", "corrected"))
#' }

correct_compl_junctions <- function(clean = TRUE, celltoldig = 2){
  GV <- Sys.getenv("GRASS_VERBOSE")
  Sys.setenv("GRASS_VERBOSE" = -1)
  cnames<-execGRASS("db.columns",
                    parameters = list(
                      table = "streams_v"
                    ), intern=T)
  
  # temporary directory
  temp_dir <- tempdir()

  # get cellcize of dem raster
  cellsize <- execGRASS("g.region", flags = "p",intern=T)
  cellsize <- as.numeric(do.call(rbind,strsplit(cellsize[grep("res",cellsize)],split=":"))[,2])
  #if(round(cellsize[1], celltoldig) != round(cellsize[2], celltoldig)){
    #stop("north-south cell size != east-west cell size. Please check")
    cellsize <- min(cellsize)
  #} else cellsize <- cellsize[1]

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
                 sql = paste0("select ", paste0("prev_str0", 1:i, collapse = ", "),  " from streams_v where prev_str03 > 0")
               ), intern = T), 
     split = "\\|"))
   colnames(dt.junctions) <- dt.junctions[1, ]
   dt.junctions <- data.frame(dt.junctions[-1, , drop = F], 
                              stringsAsFactors = FALSE)
   setDT(dt.junctions)
   dt.junctions[, `:=`(names(dt.junctions), lapply(.SD, as.numeric))]
   dt.junctions <- melt(dt.junctions, measure.vars = colnames(dt.junctions))
   
   # all previous streams
   prev_str <- unique(dt.junctions[, value])
   i0 <- which(prev_str == 0)
   if(length(i0) > 0){
     prev_str <- prev_str[-i0]
   }
   prev_str <- paste0("(", paste(prev_str, collapse = ","), ")", sep = "")
   
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
   # str1 <- paste(unlist(c(dt.junctions)), collapse = ",")
   # str1 <- paste0("(", str1, ")", sep = "")
   dt <- data.table(do.call(rbind, strsplit(
     execGRASS("db.select", 
               parameters = list(
                 sql = paste0("select stream, length from streams_v where stream in", prev_str)
               ), intern = T), 
     split = "\\|"))[-1, ])
   setattr(dt, "names", c("stream", "len"))
   dt[, `:=`(names(dt), lapply(.SD, as.numeric))]
   dt <- merge(dt.junctions, dt, by.x = "value", by.y = "stream")
   setnames(dt, "value", "stream")
   dt[, newlen := -0.8 * cellsize]
   dt[len < (0.8 * cellsize), newlen:=-len]
   dt[, `:=`(pcat, seq(1, nrow(dt)))]
   write(paste(paste("P ", dt[, pcat], c(dt[, stream]), dt[, newlen], collapse = "\n")), 
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
               columns = "dir int")
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
   dt.all_inflows <- merge(dt.dirs, dt.all_inflows, by.x = "stream", by.y = "cat")
   
   dt <- do.call(rbind, strsplit(
     execGRASS("db.select", 
               parameters = list(
                 sql = paste0("select stream, cum_length from streams_v where stream in ", prev_str)
               ), intern = T), 
     split = "\\|"))
   colnames(dt) <- dt[1, ]
   dt <- data.frame(dt[-1, , drop = F], 
                    stringsAsFactors = FALSE)
   setDT(dt)
   dt[, `:=`(names(dt), lapply(.SD, as.numeric))]
   
   dt.all_inflows <- merge(dt.all_inflows, dt, by = "stream")
   rm(list = c("dt.dirs", "dt", "dt.junctions"))

   # passt!
   #a <- cbind(sort(unique(dt.all_inflows$stream)), sort(unique(unlist(dt.junctions)))[-1])
   #which(a[,1] != a[,2])
   
   
####   
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
   
    # Create file of point positions  prev_str0X * 1/(2*5) cellsize upstream of end of outflow to cut outflow
    # P <point id>   <line cat> <offset> [<side offset>]
    points <- file.path(temp_dir, "cut_points.txt")
    # same as -cellsize/(2 * 6) * (5-(i+3)) but shorter
    write(paste(paste("P ", df.move_streams[, "cut_stream"], df.move_streams[, "cut_stream"], c(rep(cellsize/(12) * (2-i), nrow(df.move_streams)))), collapse = "\n"), 
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
    names(dt.cut_coords) <- c("cut_x", "cut_y", "cut_stream")
    df.move_streams <- merge(df.move_streams, dt.cut_coords, by = "cut_stream")
    
    # Save originally derived network to streams_v_oX
    execGRASS("g.copy", flags = c("overwrite", "quiet"), 
              parameters = list(
                vector = paste0("streams_v,streams_v_o", i)
              ), ignore.stderr = TRUE, intern = TRUE)
    
    message(paste0("Original stream topology file moved to streams_v_o", i, "."))
    message("Breaking lines and moving vertices ...")
    
    # Break features at cut coordinates
    for (j in 1:nrow(dt.junctions)) {
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
    for (j in 1:nrow(df.move_streams)) {
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
    streams <- readVECT(vname = "streams_v", remove.duplicates = FALSE, 
                        ignore.stderr = TRUE, type = "line")
    streams@data <- data.frame(streams@data, streams$cat)
    colnames(streams@data)[ncol(streams@data)] <- paste0("cat_old", i) 

    ncat <- max(streams$cat) + 1
    for (j in 1:nrow(df.move_streams)) {
      k <- which(streams$cat == df.move_streams[j, "cut_stream"])
      if (!length(k) > 1) {
        print(j)
      }
      else {
        streams$cat[k[2]] <- ncat
        ncat <- ncat + 1
      }
    }
    # writeVECT produces new cat column; remove "cat_" from previous loop
    if("cat_" %in% colnames(streams@data)){
      streams@data <- streams@data[- which(colnames(streams@data) == "cat_")]
    }
    #print("writeVECT")
    writeVECT(streams, "streams_v", v.in.ogr_flags = c("overwrite", "quiet", "o"), ignore.stderr = TRUE)
    rm("streams")
    
    # Recalculate length of line segments
    execGRASS("v.to.db", flags = c("quiet"),
              parameters = list(
                map = "streams_v",
                option = "length",
                type = "line",
                columns = "length"
              ))
    # Find new cat_ of short and long pieces of cut streams
    cut.str <- paste(df.move_streams[, "cut_stream"], collapse = ",")
    cut.str<-paste0("(", cut.str, ")",sep="")
    dt.cut <- do.call(rbind,strsplit(
      execGRASS("db.select",
                parameters = list(
                  sql = paste0("select stream, length, cat_ from streams_v where stream in", cut.str)
                ),intern = T),
      split = '\\|'))
    colnames(dt.cut)<-dt.cut[1,]
    dt.cut <- data.table(dt.cut[-1,])
    dt.cut[,":=" (stream = as.numeric(stream),length = as.numeric(length),cat_ = as.numeric(cat_))]
    dt.smallcut <- dt.cut[dt.cut[, .I[length == min(length)], by=stream]$V1]
    setnames(dt.smallcut,"cat_","cat_small")
    dt.largecut <- dt.cut[dt.cut[, .I[length == max(length)], by=stream]$V1]
    setnames(dt.largecut,"cat_","cat_large")
    
    df.move_streams <- merge(df.move_streams, dt.smallcut[, .(stream, cat_small)], by.x = "cut_stream", by.y = "stream")
    df.move_streams <- merge(df.move_streams, dt.largecut[, .(stream, cat_large)], by.x = "cut_stream", by.y = "stream")
    dt.move_streams <- data.table(df.move_streams)
    remove(df.move_streams)

    # assign updated cat_ value to 'stream' for cut stream segments
    cut.str<-paste(c(dt.smallcut[, cat_small], dt.largecut[, cat_large]), collapse = ",")
    cut.str<-paste0("(", cut.str, ")",sep="")
    execGRASS("v.db.update", flags = c("quiet"),
              parameters = list(
                map = "streams_v",
                column = "stream",
                where = paste0("cat_ in ",cut.str),
                query_column = "cat_"
              ))
    
    # tabs <- c(tables(silent=T)$NAME)
    # tabs <- tabs[-which(tabs == "df.move_streams")]
    # remove(list = tabs)
    
    message("Updating topology ...")
    for(j in 1:nrow(dt.junctions)){
      #print(j)
      jj <- which(dt.move_streams$stream == dt.junctions[j, stream])
      # set the cut prev_strX of stream to cat_small
      execGRASS("v.db.update", flags = c("quiet"),
                parameters = list(
                  map = "streams_v",
                  column = paste0("prev_str0",dt.move_streams[jj, cut_stream_prev]),
                  where = paste0("stream = ",dt.junctions[j, stream]),
                  value = paste0(dt.move_streams[jj, cat_small])
                ))     
      
      # set prev_str01 of cat_small to move_stream 
      execGRASS("v.db.update", flags = c("quiet"),
                parameters = list(
                  map = "streams_v",
                  column = "prev_str01",
                  where = paste0("stream = ",dt.move_streams[jj, cat_small]),
                  value = paste0(dt.move_streams[jj, move_stream])
                )) 
      # set prev_str02 of cat_small to cat_large
      execGRASS("v.db.update", flags = c("quiet"),
                parameters = list(
                  map = "streams_v",
                  column = "prev_str02",
                  where = paste0("stream = ",dt.move_streams[jj, cat_small]),
                  value = paste0(dt.move_streams[jj, cat_large])
                )) 
      # set next_str of cat_small to stream
      execGRASS("v.db.update", flags = c("quiet"),
                parameters = list(
                  map = "streams_v",
                  column = "next_str",
                  where = paste0("stream = ",dt.move_streams[jj, cat_small]),
                  value = paste0(dt.junctions[j, stream])
                )) 
      # set prev_str0>2 of cut_small to 0
      for(k in i:3){
        execGRASS("v.db.update", flags = c("quiet"),
                  parameters = list(
                    map = "streams_v",
                    column = paste0("prev_str0", k),
                    where = paste0("stream = ",dt.move_streams[jj, cat_small]),
                    value = "0"
                  )) 
      }
      # set next_str of cat_large and move_str_prev to cat_small
      execGRASS("v.db.update", flags = c("quiet"),
                parameters = list(
                  map = "streams_v",
                  column = "next_str",
                  where = paste0("stream in ", paste0("(",paste(dt.move_streams[jj, .(cat_large, move_stream)],collapse=","),")")),
                  value = paste0(dt.move_streams[jj, cat_small])
                )) 
      
      # set move_stream_prev of stream to prev_str0i
      if(dt.move_streams[jj, move_stream_prev] != i){
        execGRASS("v.db.update", flags = c("quiet"),
                  parameters = list(
                    map = "streams_v",
                    column = paste0("prev_str0", dt.move_streams[jj, move_stream_prev]),
                    where = paste0("stream = ",dt.junctions[j, stream]),
                    value = paste0(dt.junctions[j, paste0("prev_str0", i), with = F])
                  )) 
      }
      
      # Set 'changed' to '1' for the changed streams
      str1<-paste(unique(unlist(dt.move_streams[jj, c(move_stream, cat_small, cat_large, cut_stream)])),collapse = ",")
      str1<-paste0("(", str1, ")",sep="")
      execGRASS("v.db.update", flags = c("quiet"),
                parameters = list(
                  map = "streams_v",
                  column = "changed",
                  where = paste0("stream in ",str1),
                  value = "1"
                ))
    }

    # delete column prev_str0X
    execGRASS("v.db.dropcolumn", flags = c("quiet"),
              parameters = list(
                map = "streams_v",
                columns = paste0("prev_str0", i)
              ))
    
  }

  message("Original stream raster moved to streams_r_o.\n")
  execGRASS("g.copy",
            flags = c("overwrite", "quiet"),
            parameters = list(
              raster = "streams_r,streams_r_o"))

  # now use automatically assigned "cat"
  execGRASS("v.to.rast", flags = c("overwrite", "quiet"),
            parameters = list(
              input = "streams_v",
              type = "line",
              output = "streams_r",
              use = "attr",
              attribute_column = "cat"
            ))

  if(clean){
    # Remove temporary vector files
    execGRASS("g.remove",
              flags = c("quiet", "f"),
              parameters = list(
                type = "vector",
                name = c("complex_flows","complex_flows_cp","complex_flows_p")
              ))
  }
  message("Complex junctions were removed. Please check changed features in streams_v.\n")
  Sys.setenv("GRASS_VERBOSE"=GV)
}


# ###############
# 
# four.inflows <- FALSE
# # four inflows
# # move prev_str04, split other inflow with smalles angle to prev_str04, remove column prev_str04
# if("prev_str04" %in% cnames){
#   four.inflows <- TRUE  
#   # get stream ID of features that have more than three inflows, and stream IDs of those inflows
#   
#   dt.junctions <- do.call(rbind, strsplit(
#     execGRASS("db.select", 
#               parameters = list(
#                 sql = "select stream, prev_str01, prev_str02, prev_str03, prev_str04 from streams_v where prev_str04 > 0"
#               ), intern = T), 
#     split = "\\|"))
#   colnames(dt.junctions) <- dt.junctions[1, ]
#   dt.junctions <- data.frame(dt.junctions[-1, , drop = F], 
#                              stringsAsFactors = FALSE)
#   setDT(dt.junctions)
#   dt.junctions[, `:=`(names(dt.junctions), lapply(.SD, as.numeric))]
#   
#   # Create new vector with all segments that form complex junctions
#   str1 <- paste(unique(unlist(c(dt.junctions))), collapse = ",")
#   str1 <- paste0("(", str1, ")", sep = "")
#   execGRASS("v.extract", flags = c("overwrite", "quiet"), 
#             parameters = list(
#               input = "streams_v", 
#               output = "complex_flows", 
#               type = "line", 
#               where = paste0("stream in ", str1))
#   )
#   
#   # Create file of point positions 0.8 x cellsize  upstream (inflows) and  
#   # 0.8 x cellsizes downstream (outflows) from junction to get the flow direction
#   # close to the juction; cellsize as coordinate does not work, because some 
#   # segements are only one cellsize long; 0.8 lays in the next cell
#   # for outflows, use cell in which junction lies
#   points <- file.path(temp_dir, "complex_points.txt")
#   str1 <- paste(unlist(c(dt.junctions)), collapse = ",")
#   str1 <- paste0("(", str1, ")", sep = "")
#   dt <- data.table(do.call(rbind, strsplit(
#     execGRASS("db.select", 
#               parameters = list(
#                 sql = paste0("select stream, length from streams_v where stream in", str1)
#               ), intern = T), 
#     split = "\\|"))[-1, ])
#   setattr(dt, "names", c("stream", "len"))
#   dt[, `:=`(names(dt), lapply(.SD, as.numeric))]
#   dt3 <- melt(dt.junctions, measure.vars = colnames(dt.junctions))
#   dt <- merge(dt3, dt, by.x = "value", by.y = "stream")
#   setnames(dt, "value", "stream")
#   dt[, newlen := -0.8 * cellsize]
#   dt[grepl("prev_str",variable) & len < (0.8 * cellsize), newlen:=-len]
#   dt[variable == "stream", newlen := 0.8 * cellsize]
#   dt[variable == "stream" & len < (0.8 * cellsize), newlen:=len]
#   
#   dt[, `:=`(pcat, seq(1, nrow(dt)))]
#   write(paste(paste("P ", dt[, pcat], c(dt[, stream]), dt[, newlen], collapse = "\n")), 
#         file = points)
#   
#   # Create point feature with points on complex flows based on points created above
#   execGRASS("v.segment", flags = c("overwrite", "quiet"), 
#             parameters = list(
#               input = "complex_flows", 
#               output = "complex_flows_p",
#               rules = points)
#   )
#   # Get flow direction at these points
#   execGRASS("v.db.addtable", flags = c("quiet"), 
#             parameters = list(
#               map = "complex_flows_p", 
#               columns = "dir int")
#   )
#   execGRASS("v.what.rast", flags = c("quiet"), 
#             parameters = list(
#               map = "complex_flows_p", 
#               raster = "dirs", 
#               column = "dir")
#   )
#   # Get coordinates of ends of juction segments (only used for the inflows)
#   dt.endcoord <- data.frame(do.call(rbind, strsplit(
#     execGRASS("v.to.db", flags = c("p", "quiet"), 
#               parameters = list(
#                 map = "complex_flows", 
#                 type = "line", 
#                 option = "end"
#               ), intern = T), 
#     split = "\\|"))[,-4], stringsAsFactors = FALSE)
#   setDT(dt.endcoord)
#   setattr(dt.endcoord, "names", c("cat", "end_x", "end_y"))
#   dt.endcoord[, `:=`(names(dt.endcoord), lapply(.SD, as.numeric))]
#   setkey(dt.endcoord, cat)
#   
#   # Merge endcoordinates with flow directions one cell above / below juction
#   dt.dirs <- readVECT("complex_flows_p", ignore.stderr = T)
#   dt.dirs <- merge(dt, dt.dirs, by.y = "cat", by.x = "pcat")
#   dt.endcoord <- merge(dt.dirs, dt.endcoord, by.x = "stream", by.y = "cat")
#   rm(list = c("dt.dirs"))
#   
#   # Find that other prev_str with the smalles differnce in flow direction -> 'cut_stream'
#   # "This gives a signed angle for any angles:
#   # a = targetA - sourceA
#   # a = (a + 180) % 360 - 180"
#   # Adjusted for directions (1:8)
#   # Direction is "Flow direction is of D8 type with a range of 1 to 8. 
#   # Multiplying values with 45 gives degrees CCW from East" (r.stream.extraxt help)
#   # 8 -> E, 6 -> S, 4 -> W, 2 -> N
#   df.move_streams <- data.frame(matrix(nrow = nrow(dt.junctions), ncol = 3))
#   colnames(df.move_streams) <- c("move_stream", "cut_stream", "cut_stream_prev")
#   
#   for (i in 1:nrow(dt.junctions)) {
#     outdir <- (dt.endcoord[stream == dt.junctions[i, prev_str04] & 
#                              variable == "prev_str04", dir] + 4)%%8
#     indirs <- dt.endcoord[stream %in% dt.junctions[i, -1] & 
#                             (grepl("prev_str01", variable) | grepl("prev_str02", variable) | grepl("prev_str03", variable)), .(stream, dir, variable)]
#     indirs[, `:=`(dif, abs((outdir - dir + 4)%%8 - 4))]
#     ## max not min here because minimum difference to other prev_str is needed
#     cs <- indirs[dif == max(dif), ]
#     # if there are tow with same angel to outflow, always take the first for reproducability
#     df.move_streams[i, ] <- c(dt.junctions[i, prev_str04], cs[1, stream], as.character(cs[1, variable]))
#   }
#   ## variable != "stream" because one stream can be stream and prev_stream in compl junctions; should not be duplicated here.
#   dt.xy_move <- dt.endcoord[stream %in% df.move_streams[, "move_stream"] & variable != "stream", .(stream, end_x, end_y)]
#   df.move_streams <- merge(df.move_streams, dt.xy_move, by.x = "move_stream", 
#                            by.y = "stream")
#   colnames(df.move_streams)[colnames(df.move_streams) == "end_x"] <- "move_end_x"
#   colnames(df.move_streams)[colnames(df.move_streams) == "end_y"] <- "move_end_y"
#   df.move_streams[,"move_stream"] <- as.numeric(as.character(df.move_streams[, "move_stream"]))
#   df.move_streams[,"cut_stream"] <- as.numeric(as.character(df.move_streams[, "cut_stream"]))
#   rm("dt.endcoord")
#   
#   # Create file of point positions 1/4 cellsize downstream of start of outflow to cut outflow
#   # P <point id>   <line cat> <offset> [<side offset>]
#   points <- file.path(temp_dir, "cut_points.txt")
#   write(paste(paste("P ", df.move_streams[, "cut_stream"], df.move_streams[, "cut_stream"], c(rep(-cellsize/4, nrow(dt.junctions)))), collapse = "\n"), 
#         file = points)
#   execGRASS("v.segment", flags = c("overwrite", "quiet"), 
#             parameters = list(
#               input = "complex_flows",
#               output = "complex_flows_cp", 
#               rules = points)
#   )
#   dt.cut_coords <- data.table(do.call(rbind, strsplit(
#     execGRASS("v.out.ascii", flags = c("overwrite", "quiet"), 
#               parameters = list(
#                 input = "complex_flows_cp"
#               ), intern = T), 
#     split = "\\|")))
#   dt.cut_coords[, `:=`(names(dt.cut_coords), lapply(.SD, as.numeric))]
#   names(dt.cut_coords) <- c("cut_x", "cut_y", "cut_stream")
#   df.move_streams <- merge(df.move_streams, dt.cut_coords, by = "cut_stream")
#   
#   # Save originally derived network to streams_v_o4
#   execGRASS("g.copy", flags = c("overwrite", "quiet"), 
#             parameters = list(
#               vector = "streams_v,streams_v_o4"
#             ), ignore.stderr = TRUE)
#   
#   message("Original stream topology file moved to streams_v_o4.\n")
#   message("Breaking lines and moving vertices ...")
#   
#   # Break features at cut coordinates
#   for (i in 1:nrow(dt.junctions)) {
#     execGRASS("v.edit", flags = c("quiet", "overwrite"), 
#               parameters = list(
#                 map = "streams_v", 
#                 type = "line", 
#                 tool = "break", 
#                 threshold = 1, 
#                 coords = c(dt.cut_coords[i, c(cut_x, cut_y)])
#               ))
#   }
#   # Move end vertices of move streams to cut coordinates
#   for (i in 1:nrow(df.move_streams)) {
#     execGRASS("v.edit", flags = c("quiet", "overwrite"), 
#               parameters = list(
#                 map = "streams_v",
#                 type = "line", 
#                 tool = "vertexmove",
#                 threshold = c(1, cellsize/4, 0), 
#                 where = paste0("stream = ", df.move_streams[i,"move_stream"]), 
#                 coords = c(df.move_streams[i, "move_end_x"], df.move_streams[i, "move_end_y"]), 
#                 move = c(df.move_streams[i, "cut_x"] - df.move_streams[i, "move_end_x"], df.move_streams[i, "cut_y"] - df.move_streams[i, "move_end_y"], 0), 
#                 snap = "node"
#               ))
#   }
#   
#   # Seems to be the easiest way to assingn new, unique cat values to all features
#   streams <- readVECT(vname = "streams_v", remove.duplicates = FALSE, 
#                       ignore.stderr = T, type = "line")
#   streams$cat_old4 <- streams$cat
#   ncat <- max(streams$cat) + 1
#   for (i in 1:nrow(df.move_streams)) {
#     j <- which(streams$cat == df.move_streams[i, "cut_stream"])
#     if (!length(j) > 1) {
#       print(i)
#     }
#     else {
#       streams$cat[j[2]] <- ncat
#       ncat <- ncat + 1
#     }
#   }
#   # writeVECT produces new cat column;
#   writeVECT(streams, "streams_v", v.in.ogr_flags = c("overwrite", "quiet", "o"), ignore.stderr = TRUE)
#   rm("streams")
#   
#   # Recalculate length of line segments
#   execGRASS("v.to.db", flags = c("quiet"),
#             parameters = list(
#               map = "streams_v",
#               option = "length",
#               type = "line",
#               columns = "length"
#             ))
#   # Find new cat_ of short and long pieces of cut streams
#   str1<-paste(df.move_streams[,"cut_stream"],collapse = ",")
#   str1<-paste0("(", str1, ")",sep="")
#   dt.cut <- do.call(rbind,strsplit(
#     execGRASS("db.select",
#               parameters = list(
#                 sql = paste0("select stream, length, cat_ from streams_v where stream in", str1)
#               ),intern = T),
#     split = '\\|'))
#   colnames(dt.cut)<-dt.cut[1,]
#   dt.cut <- data.table(dt.cut[-1,])
#   dt.cut[,":=" (stream = as.numeric(stream),length = as.numeric(length),cat_ = as.numeric(cat_))]
#   dt.smallcut <- dt.cut[dt.cut[, .I[length == min(length)], by=stream]$V1]
#   setnames(dt.smallcut,"cat_","cat_small")
#   dt.largecut <- dt.cut[dt.cut[, .I[length == max(length)], by=stream]$V1]
#   setnames(dt.largecut,"cat_","cat_large")
#   
#   if(any(df.move_streams[,"cut_stream"] %in% dt.junctions[,stream]))
#   {
#     print("stream cut in prev04")
#   }
#   # TODO do this properly!
#   # dt.junctions <- merge(dt.junctions,df.move_streams,by.x="stream",by.y = "cut_stream")
#   # dt.junctions <- merge(dt.junctions, dt.smallcut[,.(stream,cat_small)],by="stream")
#   # dt.junctions <- merge(dt.junctions, dt.largecut[,.(stream,cat_large)],by="stream")
#   # 
#   # # change prev_str0X if it was also cat_small of cut_stream to new stream id
#   # i_cut <- which(dt.junctions[,cat_small] %in% unlist(dt.junctions[,paste0("prev_str0",1:3)]))
#   # if(length(i_cut) > 0){
#   #   # find where cat_small is previous stream
#   #   i_prev <- do.call(rbind, lapply(i_cut, function(x) 
#   #     which(dt.junctions[, paste0("prev_str0", 1:3)] == dt.junctions[x, cat_small], arr.ind = TRUE)))
#   #   ii <- i_prev[,1]
#   #   jj <- paste0("prev_str0", 1:3)[i_prev[,2]]
#   #   jjj <- which(colnames(dt.junctions) %in% jj)
#   #   # make data frame
#   #   setDF(dt.junctions)
#   #   # use matrix for indexing because differnt columns might be changed for
#   #   # different rows
#   #   # change previous stream to cat_large
#   #   dt.junctions[cbind(ii, jjj)]  <- dt.junctions[i_cut, "cat_large"]
#   #   setDT(dt.junctions)
#   # }
#   # 
#   # # change move_stream if it was also cat_small or cut_stream to new stream id
#   # i_cut <- which(dt.junctions[,cat_small] %in% dt.junctions[,move_stream])
#   # if(length(i_cut) > 0){
#   #   # find where cat_small is move stream
#   #   i_mov <- c(do.call(rbind,lapply(i_cut, function(x) which(dt.junctions[,move_stream] == dt.junctions[x,cat_small]))))
#   #   # change move stream to cat_large
#   #   dt.junctions[i_mov, "move_stream"] <- dt.junctions[i_cut,  cat_large]
#   # }
#   
#   df.move_streams <- merge(df.move_streams, dt.smallcut[,.(stream,cat_small)],by.x="cut_stream", by.y = "stream")
#   df.move_streams <- merge(df.move_streams, dt.largecut[,.(stream,cat_large)],by.x="cut_stream", by.y = "stream")
#   dt.junctions <- merge(dt.junctions, df.move_streams, by.x = "prev_str04", by.y = "move_stream")
#   
#   
#   # assign updated cat_ value to 'stream' for cut stream segments
#   str1<-paste(c(dt.smallcut[,cat_small],dt.largecut[,cat_large]),collapse = ",")
#   str1<-paste0("(", str1, ")",sep="")
#   execGRASS("v.db.update", flags = c("quiet"),
#             parameters = list(
#               map = "streams_v",
#               column = "stream",
#               where = paste0("cat_ in ",str1),
#               query_column = "cat_"
#             ))
#   
#   tabs <- c(tables(silent=T)$NAME)
#   tabs <- tabs[-which(tabs == "df.move_streams")]
#   remove(list = tabs)
#   
#   message("Updating topology ...")
#   
#   for(i in 1:nrow(dt.junctions)){
#     # set the cut prev_strX of stream to cat_small
#     execGRASS("v.db.update", flags = c("quiet"),
#               parameters = list(
#                 map = "streams_v",
#                 column = dt.junctions[i, cut_stream_prev],
#                 where = paste0("stream = ",dt.junctions[i, stream]),
#                 value = paste0(dt.junctions[i, cat_small])
#               ))     
#     
#     # set prev_str01 of cat_small to prev_str04
#     execGRASS("v.db.update", flags = c("quiet"),
#               parameters = list(
#                 map = "streams_v",
#                 column = "prev_str01",
#                 where = paste0("stream = ",dt.junctions[i, cat_small]),
#                 value = paste0(dt.junctions[i, prev_str04])
#               )) 
#     # set prev_str02 of cat_small to cat_large
#     execGRASS("v.db.update", flags = c("quiet"),
#               parameters = list(
#                 map = "streams_v",
#                 column = "prev_str02",
#                 where = paste0("stream = ",dt.junctions[i, cat_small]),
#                 value = paste0(dt.junctions[i, cat_large])
#               )) 
#     # set next_str of cat_small to stream
#     execGRASS("v.db.update", flags = c("quiet"),
#               parameters = list(
#                 map = "streams_v",
#                 column = "next_str",
#                 where = paste0("stream = ",dt.junctions[i, cat_small]),
#                 value = paste0(dt.junctions[i, stream])
#               )) 
#     # set next_str of cat_large and prev_str04 to cat_small
#     execGRASS("v.db.update", flags = c("quiet"),
#               parameters = list(
#                 map = "streams_v",
#                 column = "next_str",
#                 where = paste0("stream in ",paste0("(",paste(dt.junctions[i,c(cat_large,prev_str04)],collapse=","),")")),
#                 value = paste0(dt.junctions[i, cat_small])
#               )) 
#   }
#   
#   # Mark changed features
#   execGRASS("v.db.addcolumn", flags = c("quiet"),
#             parameters = list(
#               map = "streams_v",
#               columns = "changed int"
#             ))
#   # Set 'changed' to '0' for all streams
#   execGRASS("v.db.update", flags = c("quiet"),
#             parameters = list(
#               map = "streams_v",
#               column = "changed",
#               value = "0"
#             ))
#   # Set 'changed' to '1' for the changed ones
#   for(i in 1:nrow(dt.junctions)){
#     #print(i)
#     str1<-paste(unique(unlist(dt.junctions[i,c(prev_str04,cat_small,cat_large, cut_stream)])),collapse = ",")
#     str1<-paste0("(", str1, ")",sep="")
#     execGRASS("v.db.update", flags = c("quiet"),
#               parameters = list(
#                 map = "streams_v",
#                 column = "changed",
#                 where = paste0("stream in ",str1),
#                 value = "1"
#               ))
#   }
#   # delete column prev_str04
#   execGRASS("v.db.dropcolumn", flags = c("quiet"),
#             parameters = list(
#               map = "streams_v",
#               columns = "prev_str04"
#             ))
# }
# 
# # three inflows:
# # split outflow, move one inflow downstream remove column prev_str03
# # get stream ID of features that have more than two inflows, and stream IDs of those inflows
# dt.junctions<-do.call(rbind,strsplit(
#   execGRASS("db.select",
#             parameters = list(
#               sql = "select stream, prev_str01, prev_str02, prev_str03 from streams_v where prev_str03 > 0"
#             ), intern=T),
#   split='\\|'))
# colnames(dt.junctions) <- dt.junctions[1,]
# dt.junctions <- data.frame(dt.junctions[-1,,drop=F], stringsAsFactors = FALSE)
# setDT(dt.junctions)
# dt.junctions[, names(dt.junctions) := lapply(.SD, as.numeric)]
# 
# # Create new vector with all segments that form complex junctions
# str1<-paste(unique(unlist(c(dt.junctions))),collapse = ",")
# str1<-paste0("(", str1, ")",sep="")
# execGRASS("v.extract",
#           flags = c("overwrite","quiet"),
#           parameters = list(
#             input = "streams_v",
#             output = "complex_flows",
#             type = "line",
#             where = paste0("stream in ",str1)
#           ))
# 
# # Create file of point positions 0.8 x cellsize  upstream (inflows) and  
# # 0.1 x cellsizes downstream (outflows) from junction to get the flow direction
# # close to the juction; cellsize as coordinate does not work, because some 
# # segements are only one cellsize long; 0.8 lays in the next cell
# # for outflows, use cell in which junction lies
# points <- file.path(temp_dir,"complex_points.txt")
# # MiKatt: Old method could lead to identical point id if one segment is cut in more than one piece (start and end)
# #         write(paste(paste("P ",c(df.junctions),c(df.junctions),c(rep(0.5*cellsize,nrow(df.junctions)),
# #                           rep(-0.5*cellsize,(ncol(df.junctions)-1)*nrow(df.junctions)))),collapse="\n"), file = points)
# str1<-paste(unlist(c(dt.junctions)),collapse = ",")
# str1<-paste0("(", str1, ")",sep="")
# dt<-data.table(do.call(rbind,strsplit(
#   execGRASS("db.select",
#             parameters = list(
#               sql = paste0("select stream, length from streams_v where stream in",str1)
#             ), intern=T),
#   split='\\|'))[-1,])
# setattr(dt,"names",c("stream","len"))
# dt[, names(dt) := lapply(.SD, as.numeric)]
# dt3 <- melt(dt.junctions, measure.vars = colnames(dt.junctions))
# dt <- merge(dt3, dt, by.x = "value", by.y = "stream")
# setnames(dt, "value","stream")
# dt[, newlen := -0.8 * cellsize]
# dt[grepl("prev_str",variable) & len < (0.8 * cellsize), newlen:=-len]
# dt[variable == "stream", newlen := 0.1 * cellsize]
# dt[variable == "stream" & len < (0.1 * cellsize), newlen:=len]
# dt[, pcat := seq(1, nrow(dt))]
# write(paste(paste("P ", dt[, pcat], c(dt[, stream]), dt[, newlen], collapse="\n")),
#       file = points)
# # Create point feature with points on complex flows based on points created above
# execGRASS("v.segment",
#           flags = c("overwrite","quiet"),
#           parameters = list(
#             input = "complex_flows",
#             output = "complex_flows_p",
#             rules = points
#           ))
# # Get flow direction at these points
# execGRASS("v.db.addtable", flags = c("quiet"),
#           parameters = list(
#             map = "complex_flows_p",
#             columns = "dir int"
#           ))
# execGRASS("v.what.rast", flags = c("quiet"),
#           parameters = list(
#             map = "complex_flows_p",
#             raster = "dirs",
#             column = "dir"
#           ))
# 
# # Get coordinates of ends of juction segments (only used for the inflows)
# dt.endcoord<-data.frame(do.call(rbind,strsplit(
#   execGRASS("v.to.db", flags =c("p","quiet"),
#             parameters = list(
#               map = "complex_flows", #"streams_v",
#               type = "line",
#               option = "end"
#             ),intern=T),
#   split = '\\|'))[,-4], stringsAsFactors = FALSE)
# setDT(dt.endcoord)
# setattr(dt.endcoord,"names",c("cat","end_x","end_y"))
# dt.endcoord[, names(dt.endcoord) := lapply(.SD, as.numeric)]
# setkey(dt.endcoord,cat)
# 
# # Merge endcoordinates with flow directions one cell above / below juction
# dt.dirs<-readVECT("complex_flows_p",ignore.stderr = T)
# dt.dirs<-merge(dt,dt.dirs,by.y="cat",by.x="pcat")
# dt.endcoord<- merge(dt.dirs,dt.endcoord,by.x="stream",by.y="cat")
# rm(list=c("dt.dirs"))
# 
# # Find that inflow with the largest differnce in flow direction to the outflow (= smallest angle) -> 'move_stream'
# # "This gives a signed angle for any angles:
# # a = targetA - sourceA
# # a = (a + 180) % 360 - 180"
# # Adjusted for directions (1:8)
# # Direction is "Flow direction is of D8 type with a range of 1 to 8. 
# # Multiplying values with 45 gives degrees CCW from East" (r.stream.extraxt help)
# # 8 -> E, 6 -> S, 4 -> W, 2 -> N
# df.move_streams<-matrix(nrow=nrow(dt.junctions),ncol=2)
# colnames(df.move_streams)<-c("move_stream","cut_stream")
# for(i in 1:nrow(dt.junctions)){
#   # direction of outflow, turned around
#   outdir <- (dt.endcoord[stream==dt.junctions[i, stream] & variable == "stream",dir] + 4) %% 8
#   # cat and direction of inflows
#   indirs <- dt.endcoord[stream %in% dt.junctions[i,-1] & grepl("prev_",variable),.(stream,dir)]
#   # difference between inflow and outflow directions;
#   indirs[, dif:=abs((outdir-dir+4) %% 8 -4)]
#   ms <- indirs[dif==min(dif),][1,stream] # if there are tow inflows with same angel to outflow, always take the first for reproducability
#   df.move_streams[i,] <- c(ms,dt.junctions[i,stream])
# }
# ## variable != "stream" because one stream can be stream and prev_stream in compl junctions; should not be duplicated here.
# dt.xy_move<-dt.endcoord[stream %in% df.move_streams[,"move_stream"] & variable != "stream",.(stream,end_x,end_y)]
# df.move_streams <- merge(df.move_streams, dt.xy_move,by.x="move_stream",by.y="stream")
# colnames(df.move_streams)[colnames(df.move_streams) == "end_x"] <- "move_end_x"
# colnames(df.move_streams)[colnames(df.move_streams) == "end_y"] <- "move_end_y"
# rm("dt.endcoord")
# 
# # Create file of point positions 1/4 cellsize downstream of start of outflow to cut outflow
# # P <point id>   <line cat> <offset> [<side offset>]
# points <- file.path(temp_dir,"cut_points.txt")
# write(paste(paste("P ",dt.junctions[,stream],dt.junctions[,stream],c(rep(cellsize/4,nrow(dt.junctions)))),collapse="\n"), file = points)
# execGRASS("v.segment",
#           flags = c("overwrite","quiet"),
#           parameters = list(
#             input = "complex_flows",
#             output = "complex_flows_cp",
#             rules = points
#           ))
# dt.cut_coords<-data.table(do.call(rbind,strsplit(
#   execGRASS("v.out.ascii",
#             flags = c("overwrite", "quiet"),
#             parameters = list(
#               input = "complex_flows_cp"
#             ),intern=T),
#   split='\\|')))
# dt.cut_coords[, names(dt.cut_coords) := lapply(.SD, as.numeric)]
# names(dt.cut_coords) <- c("cut_x","cut_y","cut_stream")
# df.move_streams <- merge(df.move_streams,dt.cut_coords,by="cut_stream")
# 
# # Save originally derived network to streams_v_o3
# execGRASS("g.copy",
#           flags = c("overwrite", "quiet"),
#           parameters = list(
#             vector = "streams_v,streams_v_o3"), ignore.stderr = TRUE)
# 
# message("Original stream topology file moved to streams_v_o3.\n")
# message("Breaking lines and moving vertices ...")
# 
# # Break features at cut coordinates
# for(i in 1:nrow(dt.junctions)){
#   #print(i)
#   execGRASS("v.edit",
#             flags = c("quiet","overwrite"),
#             parameters = list(
#               map = "streams_v",
#               type = "line",
#               tool = "break",
#               threshold = 1,
#               #where = paste0("stream = ",df.junctions[i,"stream"]),
#               coords = c(dt.cut_coords[i,c(cut_x,cut_y)])
#             ))
# }
# # Move end vertices of move streams to cut coordinates
# for(i in 1:nrow(df.move_streams)){
#   #print(i)
#   execGRASS("v.edit",
#             flags = c("quiet","overwrite"),
#             parameters = list(
#               map = "streams_v",
#               type = "line",
#               tool = "vertexmove",
#               threshold = c(1,cellsize/4,0),
#               where = paste0("stream = ",df.move_streams[i,"move_stream"]),
#               coords = c(df.move_streams[i,"move_end_x"],df.move_streams[i,"move_end_y"]),
#               move = c(df.move_streams[i,"cut_x"]- df.move_streams[i,"move_end_x"],df.move_streams[i,"cut_y"]- df.move_streams[i,"move_end_y"],0),
#               snap = "node"
#             ))
# }
# 
# # Seems to be the easiest way to assingn new, unique cat values to all features
# streams <- readVECT(vname = "streams_v", remove.duplicates = FALSE, ignore.stderr = T, type = "line")
# streams$cat_old3 <- streams$cat
# ncat <- max(streams$cat) +1
# for(i in 1:nrow(dt.junctions)){
#   j <- which(streams$cat == dt.junctions[i,stream])
#   if(!length(j) > 1){
#     print(i)
#   } else {
#     streams$cat[j[2]] <- ncat
#     ncat <- ncat +1
#   }
# }
# # writeVECT produces new cat column;
# # IMPORTANT: Take care not to base calculations on that but to use manually updated cat_ (= new 'stream'))
# # 20180219: override projection check
# if(four.inflows == TRUE){
#   streams$cat4_ <- streams$cat_
#   streams@data <- streams@data[,-which(colnames(streams@data) == "cat_")]
# }
# writeVECT(streams, "streams_v", v.in.ogr_flags = c("overwrite", "quiet", "o"), ignore.stderr = TRUE)
# rm("streams")
# 
# # Recalculate length of line segments
# execGRASS("v.to.db", flags = c("quiet"),
#           parameters = list(
#             map = "streams_v",
#             option = "length",
#             type = "line",
#             columns = "length"
#           ))
# # Find new cat_ of short and long pieces of cut streams
# str1<-paste(dt.junctions[,stream],collapse = ",")
# str1<-paste0("(", str1, ")",sep="")
# dt.cut <- do.call(rbind,strsplit(
#   execGRASS("db.select",
#             parameters = list(
#               sql = paste0("select stream, length, cat_ from streams_v where stream in", str1)
#             ),intern = T),
#   split = '\\|'))
# colnames(dt.cut)<-dt.cut[1,]
# dt.cut <- data.table(dt.cut[-1,])
# dt.cut[,":=" (stream = as.numeric(stream),length = as.numeric(length),cat_ = as.numeric(cat_))]
# dt.smallcut <- dt.cut[dt.cut[, .I[length == min(length)], by=stream]$V1]
# setnames(dt.smallcut,"cat_","cat_small")
# dt.largecut <- dt.cut[dt.cut[, .I[length == max(length)], by=stream]$V1]
# setnames(dt.largecut,"cat_","cat_large")
# 
# dt.junctions <- merge(dt.junctions,df.move_streams,by.x="stream",by.y = "cut_stream")
# dt.junctions <- merge(dt.junctions, dt.smallcut[,.(stream,cat_small)],by="stream")
# dt.junctions <- merge(dt.junctions, dt.largecut[,.(stream,cat_large)],by="stream")
# 
# # change prev_str0X if it was also cat_small of cut_stream to new stream id
# i_cut <- which(dt.junctions[,cat_small] %in% unlist(dt.junctions[,paste0("prev_str0",1:3)]))
# if(length(i_cut) > 0){
#   # find where cat_small is previous stream
#   i_prev <- do.call(rbind, lapply(i_cut, function(x) 
#     which(dt.junctions[, paste0("prev_str0", 1:3)] == dt.junctions[x, cat_small], arr.ind = TRUE)))
#   ii <- i_prev[,1]
#   jj <- paste0("prev_str0", 1:3)[i_prev[,2]]
#   jjj <- unlist(lapply(jj, function(x) which(colnames(dt.junctions) == x)))
#   
#   # make data frame
#   setDF(dt.junctions)
#   # use matrix for indexing because differnt columns might be changed for
#   # different rows
#   # change previous stream to cat_large
#   dt.junctions[cbind(ii, jjj)]  <- dt.junctions[i_cut, "cat_large"]
#   setDT(dt.junctions)
# }
# 
# # change move_stream if it was also cat_small or cut_stream to new stream id
# i_cut <- which(dt.junctions[,cat_small] %in% dt.junctions[,move_stream])
# if(length(i_cut) > 0){
#   # find where cat_small is move stream
#   i_mov <- c(do.call(rbind,lapply(i_cut, function(x) which(dt.junctions[,move_stream] == dt.junctions[x,cat_small]))))
#   # change move stream to cat_large
#   dt.junctions[i_mov, "move_stream"] <- dt.junctions[i_cut,  cat_large]
# }
# 
# # assign updated cat_ value to 'stream' for cut stream segments
# str1<-paste(c(dt.smallcut[,cat_small],dt.largecut[,cat_large]),collapse = ",")
# str1<-paste0("(", str1, ")",sep="")
# execGRASS("v.db.update", flags = c("quiet"),
#           parameters = list(
#             map = "streams_v",
#             column = "stream",
#             where = paste0("cat_ in ",str1),
#             query_column = "cat_"
#           ))
# 
# tabs <- c(tables(silent=T)$NAME)
# tabs <- tabs[-which(tabs == "dt.junctions")]
# remove(list = tabs)
# 
# message("Updating topology ...")
# 
# for(i in 1:nrow(dt.junctions)){
#   # set "next_str" of cat_small and move_stream to cat_large
#   execGRASS("v.db.update", flags = c("quiet"),
#             parameters = list(
#               map = "streams_v",
#               column = "next_str",
#               where = paste0("stream in ",paste0("(",paste(dt.junctions[i,c(cat_small,move_stream)],collapse=","),")")),
#               value = paste0(dt.junctions[i,cat_large])
#             ))
#   # set "prev_str01" and "prev_str02" of cat_small to the not moved streams
#   prev <- as.data.frame(dt.junctions[i,c(prev_str01, prev_str02, prev_str03)])
#   prev <- prev[prev != dt.junctions[i,move_stream]]
#   execGRASS("v.db.update", flags = c("quiet"),
#             parameters = list(
#               map = "streams_v",
#               column = "prev_str01",
#               where = paste0("stream == ",dt.junctions[i,cat_small]),
#               value = paste0(prev[1])
#             ))
#   execGRASS("v.db.update", flags = c("quiet"),
#             parameters = list(
#               map = "streams_v",
#               column = "prev_str02",
#               where = paste0("stream == ",dt.junctions[i,cat_small]),
#               value = paste0(prev[2])
#             ))
#   # set "next_str" of prev_str01 and prev_str02 to cat_small
#   execGRASS("v.db.update", flags = c("quiet"),
#             parameters = list(
#               map = "streams_v",
#               column = "next_str",
#               where = paste0("stream in ",paste0("(",paste(prev,collapse=","),")")),
#               value = paste(dt.junctions[i,cat_small])
#             ))
#   # set "prev_str01" and "prev_str02" of cat_large to cat_small and move_stream
#   prev <- c(dt.junctions[i,c(cat_small, move_stream)])
#   execGRASS("v.db.update", flags = c("quiet"),
#             parameters = list(
#               map = "streams_v",
#               column = "prev_str01",
#               where = paste0("stream == ",dt.junctions[i,cat_large]),
#               value = paste0(prev[1])
#             ))
#   execGRASS("v.db.update", flags = c("quiet"),
#             parameters = list(
#               map = "streams_v",
#               column = "prev_str02",
#               where = paste0("stream == ",dt.junctions[i,cat_large]),
#               value = paste0(prev[2])
#             ))
#   # set 'prev_str01' or 'prev_str02' of next_str of cat_large to cat_large
#   ns<-execGRASS("db.select",
#               parameters = list(
#                 sql = paste0("select next_str from streams_v where stream == ", dt.junctions[i,cat_large])
#               ),intern = T)[2]
#   prev.ns<-unlist(strsplit(
#                 execGRASS("db.select",
#                   parameters = list(
#                     sql = paste0("select prev_str01, prev_str02 from streams_v where stream == ", ns)
#                   ),intern = T)[2],
#              split = '\\|'))
#   prev<-which(as.numeric(prev.ns) == dt.junctions[i,cat_small])
#   if(length(prev)>0){
#     prev<-paste0("prev_str0",prev)
#     execGRASS("v.db.update", flags = c("quiet"),
#               parameters = list(
#                 map = "streams_v",
#                 column = prev,
#                 where = paste0("stream == ",ns),
#                 value = paste0(dt.junctions[i,cat_large])
#               ))
#   }
# }
# 
# # Mark changed features
# if(! four.inflows){
#   execGRASS("v.db.addcolumn", flags = c("quiet"),
#             parameters = list(
#               map = "streams_v",
#               columns = "changed int"
#             ))
#   # Set 'changed' to '0' for all streams
#   execGRASS("v.db.update", flags = c("quiet"),
#             parameters = list(
#               map = "streams_v",
#               column = "changed",
#               value = "0"
#             ))
# }
# # Set 'changed' to '1' for the changed ones
# for(i in 1:nrow(dt.junctions)){
#   #print(i)
#   str1<-paste(unique(unlist(dt.junctions[i,c(prev_str01,prev_str02,prev_str03,cat_small,cat_large)])),collapse = ",")
#   str1<-paste0("(", str1, ")",sep="")
#   execGRASS("v.db.update", flags = c("quiet"),
#             parameters = list(
#               map = "streams_v",
#               column = "changed",
#               where = paste0("stream in ",str1),
#               value = "1"
#             ))
# }
# 
# # !MiKatt: crashes when str1 contains many streams but works row wise on df.junctions
# # str1<-paste(sort(unique(unlist(df.junctions[c("prev_str01","prev_str02","prev_str03","cat_small","cat_large")]))),collapse = ",")
# # str1<-paste0("(", str1, ")",sep="")
# # execGRASS("v.db.update", flags = c("quiet"),
# #           parameters = list(
# #             map = "streams_v",
# #             column = "changed2",
# #             where = paste0("stream in ",str1),
# #             value = "1"
# #           ))
# # execGRASS("v.db.update", flags = c("quiet"),
# #           parameters = list(
# #             map = "streams_v",
# #             column = "changed",
# #             where = paste0("stream not in ",str1),
# #             value = "0"
# #           ))
# 
# # delete column prev_str03
# execGRASS("v.db.dropcolumn", flags = c("quiet"),
#           parameters = list(
#             map = "streams_v",
#             columns = "prev_str03"
#           ))
# 
