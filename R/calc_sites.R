#'Calculate sites for SSN object.
#'
#'@importFrom methods as
#'@import sp
#'
#'@description A vector (points) map 'sites' is derived and several attributes
#'are assigned.
#'
#'@param locid_col character (optional); column name in the sites attribute 
#'  table giving a unique site identifier. If not provided, it is created
#'  automatically (based on the 'cat' field; default).
#'@param pid_col character (optional); column name in the sites attribute table 
#' that distinguishes between repeated measurements at a sampling site, e.g. by 
#' date. If not provided, it is created automatically.
#'@param pred_sites character vector (optional); names for prediction sites 
#'(loaded with \code{import_data}).
#'
#'@details Steps include:
#'\itemize{
#'\item{Snap points to derived network (edges). 'dist'
#'gives the distance of the original position to the closest streams segment. 
#'If this is a too large value consider running \code{\link{derive_streams}} again with
#'smaller value for \code{accum_threshold} and/or \code{min_stream_length}.}
#'\item{Assign unique 'pid' and 'locID' (needed by the 'SSN' package).}
#'\item{Get 'rid' and 'netID' of the
#'stream segment the site intersects with (from map "edges").}
#'\item{Calculate upstream distance for each point ('upDist').}
#' \item{Calculate distance ratio
#'('ratio') between position of site on edge (distance traveled from lower
#'end of the edge to the site) and the total length of the edge.} }
#'Often, survey sites do not lay exactly on the stream network (due to GPS imprecision,
#'stream representation as lines, derivation of streams from dem, etc.). To
#'assign an exact position of the sites on the network they are moved to the
#'closest stream segment (snapped) using the GRASS function
#'\href{https://grass.osgeo.org/grass74/manuals/v.distance.html}{v.distance}.
#'
#' If \code{locid_col} and \code{pid_col} are not provided, 'pid' and 'locID' 
#' are identical, unique numbers. If they are provided, they are created based
#' on these columns (as numbers, not as text). Note that repeated measurements
#' can be joined to the sites at a later step. Then, 'pid' needs to be updated 
#' accordingly (NOT YET IMPLEMENTED). 
#' 
#' 'upDist' is calculated using
#'\href{https://grass.osgeo.org/grass73/manuals/v.distance.html}{v.distance} with 
#'upload = "to_along" which gives the distance along the stream segment to the next
#'upstream node ('distalong'). 'upDist' is the difference between the 'upDist' 
#' of the edge the point lies on and 'distalong'.
#'
#'If prediction sites have been created outside of this package they can be 
#'processed here as well. They must have been imported with \code{\link{import_data}}
#'before. Alternatively, prediction sites can be created using
#'\code{\link{calc_prediction_sites}}.
#'
#'@note \code{\link{import_data}}, \code{\link{derive_streams}} and
#'  \code{\link{calc_edges}} must be run before.
#'
#'@author Mira Kattwinkel \email{mira.kattwinkel@@gmx.net}, Eduard Szoecs, 
#' \email{eduardszoecs@@gmail.com}, 
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
#' derive_streams(burn = 0, accum_threshold = 700, condition = TRUE, clean = TRUE)
#' 
#' # Check and correct complex junctions (there are no complex juctions in this
#' # example date set)
#' cj <- check_compl_junctions()
#' if(cj){
#'   correct_compl_junctions()
#' }
#'
#' # Prepare edges
#' calc_edges()
#'
#' # Prepare site
#' calc_sites()
#' # Plot data
#' dem <- readRAST('dem', ignore.stderr = TRUE)
#' edges <- readVECT('edges', ignore.stderr = TRUE)
#' sites <- readVECT('sites', ignore.stderr = TRUE)
#' sites_o <- readVECT('sites_o', ignore.stderr = TRUE)
#' plot(dem, col = terrain.colors(20),axes = TRUE)
#' lines(edges, col = 'blue')
#' points(sites, pch = 4)
#' points(sites_o, pch = 1)
#' legend("topright", pch = c(1, 4), legend = c("original", "corrected"))
#'}


calc_sites <- function(locid_col = NULL, pid_col = NULL, pred_sites = NULL, maxdist = NULL) {
  vect <- execGRASS("g.list",
                    parameters = list(
                      type = "vect"
                    ),
                    intern = TRUE)
  rast <- execGRASS("g.list",
                    parameters = list(
                      type = "rast"
                    ),
                    intern = TRUE)
  if (!"sites_o" %in% vect)
    stop("Sites not found. Did you run import_data()?")
  if (!"edges" %in% vect)
    stop("Edges not found. Did you run calc_edges()?")
  
  if(!is.null(pred_sites)){
   i <- grep("_o$",pred_sites)
   if(length(i) > 0){
    pred_sites[-i] <- paste0(pred_sites[-i],"_o")
   } else
     pred_sites <- paste0(pred_sites,"_o")     
   if (any(!pred_sites %in% vect))
     stop("Prediction sites not found. Did you run import_data() on them?")
  }
  
  site_maps <- c("sites", pred_sites)
  site_maps <- sub("_o$","", site_maps)
  s <- sapply(site_maps, prepare_sites, locid_c = locid_col, pid_c = pid_col)
  
  execGRASS("v.db.dropcolumn",
            map = "sites",
            columns = "cat_edge")
}

#' Snap sites to streams and calculate attributes
#' @param sites_map character; name of sites map (observation or prediction sites)
#' as created by \code{import_data}.
#' @param locid_c character (optional); column name in the sites attribute 
#'  table giving a unique site identifier. 
#'@param pid_c character (optional); column name in the sites attribute table 
#' that distinguishes between repeated measurements at a sampling site.
#' 
#' @details 
#' This function is called by \code{calc_sites} and should not be called directly.
#' Sites are snapped to the streams and upstream distance is calculated.
#'

prepare_sites <- function(sites_map, locid_c = NULL, pid_c = NULL, maxdist = NULL){
  execGRASS("g.copy",
            flags = c("overwrite", "quiet"),
            parameters = list(
              vector = paste0(paste0(sites_map,"_o"), ",",sites_map)))
  
  message(paste0("Preparing sites '", sites_map, "' ..."))
  # Snap sites to streams --------
  message("Snapping sites to streams ...")
  # add 5 columns holding: stream, distance and coordinates of nearest streams
  
  # drop columns if they are in sites
  cnames <- execGRASS("db.columns", flags = "quiet",
                      parameters = list(
                        table = "sites"
                      ), intern = T)
  if(any(i <- which(c("cat_edge","str_edge","dist","xm", "ym") %in% cnames))){
    execGRASS("v.db.dropcolumn", flags = "quiet",
              map = "sites",
              columns = paste0(c("cat_edge","str_edge","dist","xm", "ym")[i], collapse = ","))
  }
  
  execGRASS("v.db.addcolumn",
            parameters = list(
              map = sites_map,
              columns = "cat_edge int,str_edge int,dist double precision,xm double precision,ym double precision"
            ))
  # calc distance
  # MiKatt: additionally get cat of nearest edge for later joining of netID and rid
  execGRASS("v.distance",
            flags = c("overwrite", "quiet"),
            parameters = list(from = sites_map,
                              to = "edges",
                              #output = "connectors",
                              upload = "cat,dist,to_x,to_y",
                              column = "cat_edge,dist,xm,ym"))
  #! This is in R faster than in GRASS!? (which has to write to hard-drive)
  #! Other possibilities in GRASS to change coordinates?
  #! use r.stream.snap alternatively?
  sites <- readVECT(sites_map, type = "point", ignore.stderr = TRUE)
  proj4 <- proj4string(sites)
  sites <-  as(sites, "data.frame")
  coordinates(sites) <-  ~ xm + ym
  proj4string(sites) <- proj4
  names(sites)[names(sites) %in% c( "coords.x1", "coords.x2")] <- c("xm", "ym")
  sites$cat_ <- NULL
  
  # get actual maximum snapping distance
  mdist <- max(sites@data$dist)
  message(paste("Maximum sapping distance found:", mdist, "m"))
  # compare to given one
  if(!is.null(maxdist) & mdist > maxdist){
    i <- which(sites@data$dist >= maxdist)
    sites <- sites[-i]
    message(paste0(length(i), "sites with snapping distance > maxdist (", maxdist," m). Sites were deleted."))
  }
  
  ### Setting pid -----------
  # MiKatt: pid is for "repeated measurements at a singel location" 
  #         (Peterson & Ver Hoef, 2014: Stars: An ArcGIS Toolset Used to 
  #         Calculate the Spatial Information Neede to Fit SPatial Statistical 
  #         Models to Stream Network Data, p. 13)
  #         Hence locID = pid is ok if there are not repeated measurements
  #         otherwise assign seperatelly
  # MiKatt: Also keep user defined site IDs / site names
  # Here && in if clause to first check is.null
  message("Setting pid and locID ...")
  if(!is.null(locid_c) && locid_c %in% colnames(sites@data) ){
    sites@data$locID <- as.numeric(as.factor(sites@data[,locid_c]))
  } else {  
    sites@data$locID <- sites@data$cat
  }
  if(!is.null(pid_c) && pid_c %in% colnames(sites@data)){
    sites@data$pid <- as.numeric(as.factor(sites@data[,pid_c]))
  } else {
    sites@data$pid <- sites@data$locID
  }
  
  # 20180219: override projection check
  writeVECT(sites, vname = sites_map,
            v.in.ogr_flags = c("overwrite", "quiet", "o"),
            ignore.stderr = TRUE)
  rm(sites)
  
  # Set netID and rid from network ---------
  message("Assigning netID and rid ...")
  
  # MiKatt: This seems to be faster
  execGRASS("v.db.addcolumn",
            flags = c("quiet"),
            parameters = list(map = sites_map,
                              columns = "netID int, rid int"))
  execGRASS("db.execute",
            parameters = list(
              sql=paste0('UPDATE ', sites_map, ' SET rid=(SELECT rid FROM edges WHERE ', sites_map, '.cat_edge=edges.cat)')
            ))
  execGRASS("db.execute",
            parameters = list(
              sql=paste0('UPDATE ', sites_map, ' SET netID=(SELECT netID FROM edges WHERE ', sites_map, '.cat_edge=edges.cat)')
            ))
  execGRASS("db.execute",
            parameters = list(
              sql=paste0('UPDATE ', sites_map, ' SET str_edge=(SELECT stream FROM edges WHERE ', sites_map, '.cat_edge=edges.cat)')
            ))
  
  # Calculate upDist ---------
  # MiKatt: Distance of every raster cell from the outlet
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
  # execGRASS("v.db.addcolumn",
  #           parameters = list(map = sites_map,
  #                             columns = "upDist double"))
  # execGRASS("v.what.rast",
  #           flags = c("quiet"),
  #           parameters = list(
  #             map = sites_map,
  #             raster = "upDist",
  #             column = "upDist"
  #           ))
  # execGRASS("v.db.update", flags = c("quiet"),
  #           parameters = list(
  #             map = sites_map,
  #             column = "upDist",
  #             value = "round(upDist, 2)"
  #           ))
  # Calculate ratio = distance from lower end of edge to site / length edge
  # message("Calculating distance ratio...\n")
  # execGRASS("v.db.addcolumn",
  #           flags = c("quiet"),
  #           parameters = list(
  #             map = sites_map,
  #             columns = "ratio double precision"
  #           ))
  # sql_str <- paste0('UPDATE ', sites_map, ' SET ratio=1-',
  #                   'round((((SELECT upDist FROM edges WHERE edges.cat=', sites_map, '.cat_edge)-upDist)),2)/',
  #                   '(SELECT Length FROM edges WHERE edges.cat=', sites_map, '.cat_edge)')
  # execGRASS("db.execute",
  #           parameters = list(
  #             sql=sql_str
  #           )) 
  # ### 
  execGRASS("v.db.addcolumn",
            parameters = list(map = sites_map,
                              columns = "upDist double precision, distalong double precision"))
  execGRASS("v.distance", flags = c("quiet"),
            parameters =list(
              from = sites_map,
              to = "edges",
              to_type = "line",
              upload = "to_along",
              column = "distalong"
            ))
  sql_str <- paste0('UPDATE ', sites_map, ' SET upDist=',
                    'round(((SELECT upDist FROM edges WHERE edges.cat=', 
                    sites_map, '.cat_edge)-distalong),2)')
  execGRASS("db.execute",
            parameters = list(
              sql=sql_str
            ))

  execGRASS("v.db.addcolumn",
            flags = c("quiet"),
            parameters = list(
              map = sites_map,
              columns = "ratio double precision"
            ))
  sql_str <- paste0('UPDATE ', sites_map, ' SET ratio=1-',
                    'distalong/',
                    '(SELECT Length FROM edges WHERE edges.cat=', sites_map, '.cat_edge)')
  execGRASS("db.execute",
            parameters = list(
              sql=sql_str
            ))
}

