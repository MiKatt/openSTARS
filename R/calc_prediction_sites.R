#'Calculate prediction sites for 'SSN' object.
#'
#'@description A vector (points) map of prediction sites is created and several
#'attributes are assigned.
#'
#'@param predictions string giving the name for the prediction sites map.
#'@param dist number giving the distance between the points to create in map
#'  units.
#'@param nsites integer giving the approximate number of sites to create
#'@param netIDs integer (optional): create prediction sites only on streams with
#'  these netID(s).
#'
#'@details Either \code{dist} or \code{nsites} must be provided. If \code{dist}
#'is NULL, it is estimated by dividing the total stream length in the map by
#'\code{nsites}; the number of sites actually derived might therefore be a bit
#'smaller than \code{nsites}.
#'
#'Steps include: 
#'\itemize{ 
#'\item{Place points on edges with given distance from each other} 
#'\item{Save the point coordinates in NEAR_X and NEAR_Y.}
#'\item{Assign unique identifiers (needed by the 'SSN' package) 'pid'
#'and 'locID'.} 
#'\item{Get 'rid' and 'netID' of the stream segment the site
#'intersects with (from map 'edges').} 
#'\item{Calculate upstream distance for
#'each point ('upDist').} 
#'\item{Calculate distance ratio ('distRatio') between
#'position of the site on the edge (= distance traveled from lower end of the
#'edge to the site) and the total length of the edge.} }
#'
#''pid' and 'locID' are identical, unique numbers. 'upDist' is calculated using
#'\href{https://grass.osgeo.org/grass72/manuals/addons/r.stream.distance.html}{r.stream.distance}.
#'Points are created using
#'\href{https://grass.osgeo.org/grass72/manuals/v.segment.html}{v.segment}.
#'
#'@note \code{\link{import_data}}, \code{\link{derive_streams}} and
#'  \code{\link{calc_edges}} must be run before.
#'
#'@author Mira Kattwinkel \email{mira.kattwinkel@@gmx.net}
#'@export
#'
#' @examples
#' \donttest{
#' # Initiate GRASS session
#' if(.Platform$OS.type == "windows"){
#'   gisbase = "c:/Program Files/GRASS GIS 7.6"
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
#' check_compl_confluences()
#' calc_edges()
#' calc_sites()
#' calc_prediction_sites(predictions = "preds", dist = 2500)
#' 
#' library(sp)
#' dem <- readRAST('dem', ignore.stderr = TRUE)
#' sites <- readVECT('sites', ignore.stderr = TRUE)
#' preds <- readVECT('preds', ignore.stderr = TRUE)
#' edges <- readVECT('edges', ignore.stderr = TRUE)
#' plot(dem, col = terrain.colors(20))
#' lines(edges, col = 'blue', lwd = 2)
#' points(sites, pch = 4)
#' points(preds, pch = 19, col = "steelblue")
#' }

calc_prediction_sites <- function(predictions, dist = NULL, nsites = 10,
                                  netIDs = NULL) {
  
  # MiKatt 20200717
  # WARNING: Values in column <cat> will be overwritten

  vect <- execGRASS("g.list",
                    parameters = list(
                      type = "vect"
                    ),
                    intern = TRUE)
  if (!"edges" %in% vect)
    stop("Edges not found. Did you run calc_edges()?")
  if(predictions %in% vect)
    execGRASS("g.remove",
              flags = c("quiet", "f"),
              parameters = list(
                type = "vector",
                name = predictions
              ))

  if(all(is.null(c(dist, nsites))))
    stop("Either the distance between prediction sites (dist) or the number of
         prediction sites (nsites) must be given.")
  
  temp_dir <- tempdir()

  dt.streams <- do.call(rbind,strsplit(
    execGRASS("db.select",
              parameters = list(
                sql = "select cat, stream, next_str, prev_str01,prev_str02,netID,Length from edges"
              ),intern = T),
    split = '\\|'))
  colnames(dt.streams) <- dt.streams[1,]
  dt.streams <- data.table(dt.streams[-1,,drop = FALSE])
  dt.streams[, names(dt.streams) := lapply(.SD, as.numeric)]
  dt.streams[, offset := 0]

  # omit all segements that do not belong to the netIDs given
  if(!is.null(netIDs)){
    dt.streams[!(netID %in% netIDs), offset := NA]
    dt.streams <- stats::na.omit(dt.streams, cols = "offset")
  }
  if(nrow(dt.streams) == 0)
    stop("No streams to place prediction points on. Please check netIDs.")

  if(is.null(dist))
    dist <- ceiling(sum(dt.streams[,Length]) / nsites)

  message("Calculating point positions ...")
  outlets <- dt.streams[next_str == -1, stream]
  for(i in outlets){
    calc_offset(dt.streams, id=i, offs = 0, dist)
  }

  pt <- 1
  str1 <- NULL
  for(i in 1:nrow(dt.streams)){
    offs <- dt.streams[i, "offset", with = FALSE]
    while(offs > 0){
      str1 <- paste0(str1, "\n", paste("P", pt, dt.streams[i, "cat", with = FALSE], offs, sep=" "))
      offs <- offs - dist
      pt <- pt + 1
    }
  }
  str1 <- substring(str1, 2)
  write(str1, file.path(temp_dir,"pt.txt"))

  execGRASS("v.segment", flags = c("overwrite", "quiet"),
            parameters = list(
              input = "edges",
              output = predictions,
              rules = file.path(temp_dir,"pt.txt")
            ))

  # MiKatt: No line break in long strings on Windows!
  message("Creating attribute table ...")
  execGRASS("v.db.addtable", flags = c("quiet"),
            parameters = list(
              map = predictions,
              columns = "cat_edge int,str_edge int,dist double precision,nx double precision,ny double precision,pid int,loc int,net int,rid int,out_dist double,distalong double precision,ratio double precision"
           ), ignore.stderr = TRUE)

  # MiKatt: Necessary to get upper and lower case column names
  execGRASS("v.db.renamecolumn", flags = "quiet",
            parameters = list(
              map = predictions,
              column = "loc,locID"
            ))
  execGRASS("v.db.renamecolumn", flags = "quiet",
            parameters = list(
              map = predictions,
              column = "net,netID"
            ))
  execGRASS("v.db.renamecolumn", flags = "quiet",
            parameters = list(
              map = predictions,
              column = "out_dist,upDist"
            ))
  execGRASS("v.db.renamecolumn", flags = "quiet",
            parameters = list(
              map = predictions,
              column = "nx,NEAR_X"
            ))
  execGRASS("v.db.renamecolumn", flags = "quiet",
            parameters = list(
              map = predictions,
              column = "ny,NEAR_Y"
            ))
  
  message("Setting cat_edge ...")
  # MiKatt: additionally get x and y coordinate
  execGRASS("v.distance",
            flags = c("overwrite", "quiet"),
            parameters = list(from = predictions,
                              to = "edges",
                              upload = "cat,dist,to_x,to_y",
                              column = "cat_edge,dist,NEAR_X,NEAR_Y"))

  message("Setting pid and locID ...")
  execGRASS("v.db.update",
            parameters = list(map = predictions,
                              column = "pid",
                              value = "cat"))
  execGRASS("v.db.update",
            parameters = list(map = predictions,
                              column = "locID",
                              value = "pid"))

  # Set netID and rid from network ---------
  message("Assigning netID and rid ...")

  sql_str<- paste0("UPDATE ", predictions, " SET rid=(SELECT rid FROM edges WHERE ",
                   predictions,".cat_edge=edges.cat)")
  execGRASS("db.execute",
            parameters = list(
              sql = sql_str
            ))
  sql_str<- paste0("UPDATE ", predictions, " SET netID=(SELECT netID FROM edges WHERE ",
                   predictions,".cat_edge=edges.cat)")
  execGRASS("db.execute",
            parameters = list(
              sql = sql_str
            ))
  sql_str<- paste0("UPDATE ", predictions, " SET str_edge=(SELECT stream FROM edges WHERE ",
                   predictions,".cat_edge=edges.cat)")
  execGRASS("db.execute",
            parameters = list(
              sql = sql_str
            ))

  # Calculate upDist ---------
  message("Calculating upDist ...")
  ## MiKatt was not exact enough, results in identical upDist if two points lay
  ##        in the same raster cell
  
  # execGRASS("r.stream.distance",
  #           flags = c("overwrite", "quiet", "o"),
  #           parameters = list(
  #             stream_rast = "streams_r",
  #             direction = "dirs",
  #             method = "downstream",
  #             distance = "upDist"
  #           ))
  # 
  # execGRASS("v.what.rast",
  #           flags = c("quiet"),
  #           parameters = list(
  #             map = predictions,
  #             raster = "upDist",
  #             column = "upDist"
  #           ))
  # MiKatt: ! Round upDist to m

  execGRASS("v.distance", flags = c("quiet"),
            parameters =list(
              from = predictions,
              to = "edges",
              to_type = "line",
              upload = "to_along",
              column = "distalong"
            ))
  sql_str <- paste0('UPDATE ', predictions, ' SET upDist=',
                    'round(((SELECT upDist FROM edges WHERE edges.cat=', 
                    predictions, '.cat_edge)-distalong),2)')
  execGRASS("db.execute",
            parameters = list(
              sql=sql_str
            ))
  # Calculate distRatio = distance from lower end of edge to site / length edge
  message("Calculating distance ratio ...")
  
  sql_str <- paste0('UPDATE ', predictions, ' SET ratio=1-',
                    'distalong/',
                    '(SELECT Length FROM edges WHERE edges.cat=', predictions, '.cat_edge)')
  execGRASS("db.execute",
            parameters = list(
              sql=sql_str
            ))
  execGRASS("v.db.dropcolumn",
            map = predictions,
            columns = "cat_edge")
}

#' Calculate offset
#' @description Recursive function to calculate the offset from the downstream
#' junction needed to place points with fixed distance along the streams.
#' It is called by \code{\link{calc_prediction_sites}} for each
#' outlet and should not be called by the user.
#'
#' @param dt data.table containing the attributes of the stream segments
#' @param id integer; 'stream' of the stream segment
#' @param offs number; offset from outlet of the stream segment (downstream);
#' equals the length of the segment if the point shall be placed directly at the
#' downstream junction.
#' @param dist number giving the distance between the points to create in map units.
#' @keywords internal
#'
#' @return Nothing; change 'offset' in dt.
#'
#' @author Mira Kattwinkel, \email{mira.kattwinkel@@gmx.net}
#'
#'@examples
#'\dontrun{
#'  outlets <- dt.streams[next_str == -1, stream]
#'  netID <- 1
#'  for(i in outlets){
#'    calc_offset(dt.streams, id = i, offs = 0, dist = 200)
#'  }
#'}

calc_offset <- function(dt, id, offs, dist){
  if(dt[stream == id, prev_str01,] == 0){  # check only one of prev01 and prev02 because they are always both 0
     dt[stream == id, offset := floor(Length - offs)]
  } else {
    dt[stream == id, offset := floor(Length - offs)]
    if(offs < 0){
      offs <- offs + dt[stream == id, Length]
      } else {
      offs <- dist - (dt[stream == id, Length - offs] %% dist)
    }
    calc_offset(dt, dt[stream == id, prev_str01], offs, dist)
    calc_offset(dt, dt[stream == id, prev_str02] ,offs, dist)
  }
}
