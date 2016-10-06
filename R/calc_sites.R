#' Calculate sites for SSN object.
#'
#' @importFrom methods as
#' @import sp
#'
#' @description
#' A vector (points) map 'sites' is derived and several attributes are assigned.
#'
#'@details
#' Steps include:
#' \itemize{
#'  \item{Snap points to derived network. 'dist' gives the distance of the original position to the closest streams segement.}
#'  \item{Assign unique 'pid' and 'locID'.}
#'  \item{Get 'rid' and 'netID' of the stream segment the site intersects with (from map 'edges').}
#'  \item{Calculate upstream distance for each point ('upDist').}
#'  \item{Calculate distance ratio ('distRatio) between position of site on edge
#'    (distance traveled from lower end of theedge to the site) and the total
#'     length of the edge.}
#' }
#' Often, survey sites do not lay exactly on the stream network (due to GPS
#' inprecision, stream representation as lines, derivation of streams from dem, etc.).
#' To assigne an exact position of the sites on the network they are moved to
#' the closest stream segment (snapped) using
#' \href{https://grass.osgeo.org/grass73/manuals/v.distance.html}{v.distance}.
#'
#' 'pid' and 'locID' are identical, unique numbers. 'upDist' is calculated using
#' #' \href{https://grass.osgeo.org/grass73/manuals/r.stream.distance.html}{r.stream.distance}.
#'
#' @note \code{\link{import_data}}, \code{\link{derive_streams}} and
#'   \code{\link{calc_edges}} must be run before.
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}, Mira Kattwinkel \email{mira.kattwinkel@@gmx.net}
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
#' cj <- check_compl_junctions()
#' if(cj){
#'   correct_compl_junctions()
#' }
#' calc_edges()
#' calc_sites()

#' dem <- readRAST('dem', ignore.stderr = TRUE)
#' sites <- readVECT('sites_o', ignore.stderr = TRUE)
#' edges <- readVECT('edges', ignore.stderr = TRUE)
#' plot(dem, col = terrain.colors(20))
#' points(sites, pch = 4)
#' lines(edges, col = 'blue')
#' }

calc_sites <- function() {
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
  if (!'sites_o' %in% vect)
    stop('Sites not found. Did you run import_data()?')
  if (!'edges' %in% vect)
    stop('Edges not found. Did you run calc_edges()?')
  execGRASS("g.copy",
            flags = c('overwrite', 'quiet'),
            parameters = list(
              vector = 'sites_o,sites'))

  # Snap sites to streams --------
  message('Snapping sites to streams...\n')
  # add 4 columns holding: stream, distance and coordinates of nearest streams
  execGRASS("v.db.addcolumn",
            parameters = list(
              map = "sites",
              columns = "cat_edge int, dist double precision, xm double precision, ym double precision"
            ))
  # calc distance
  # MiKatt: additionally get cat of nearest edge for later joining of netID and rid
  execGRASS("v.distance",
            flags = c("overwrite", 'quiet'),
            parameters = list(from = 'sites',
                              to = 'edges',
                              #output = 'connectors',
                              upload = 'cat,dist,to_x,to_y',
                              column = 'cat_edge,dist,xm,ym'))
  #! This is in R faster than in GRASS!? (which has to write to hard-drive)
  #! Other possibilities in GRASS to change coordinates?
  #! use r.stream.snap alternatively?
  sites <- readVECT('sites', type = 'point', ignore.stderr = TRUE)
  proj4 <- proj4string(sites)
  sites <-  as(sites, "data.frame")
  coordinates(sites) <-  ~ xm + ym
  proj4string(sites) <- proj4
  names(sites)[names(sites) %in% c( "coords.x1", "coords.x2")] <- c('xm', 'ym')
  sites$cat_ <- NULL
  writeVECT(sites, vname = 'sites',
            v.in.ogr_flags = c('overwrite', 'quiet'),
            ignore.stderr = TRUE)
  nsites <- nrow(sites)
  sites.coor <- data.table(sites$cat,sites$xm,sites$ym)
  rm(sites)

  ### Setting pid -----------
  #! pid and locID identical???
  # MiKatt: pid is for "repeated measurements at a singel location" (Peterson & Ver Hoef, 2014: Stars: An ArcGIS Toolset Used to Calculate the Spatial Information Neede to Fit SPatial Statistical Models to Stream Network Data, p. 13)
  # MiKatt: Hence locID = pid seems to be ok
  # MiKatt: Also keep user defined site IDs / site names
  message('Setting pid and locID...\n')
  execGRASS("v.db.addcolumn",
            parameters = list(map = 'sites',
                              columns = 'pid int, locID int'))
  execGRASS("v.db.update",
            parameters = list(map = 'sites',
                              column = 'pid',
                              value = 'cat'))
  execGRASS("v.db.update",
            parameters = list(map = 'sites',
                              column = 'locID',
                              value = 'pid'))

  # Set netID and rid from network ---------
  message('Assigning netID and rid...\n')

  # MiKatt: This seems to be faster
  execGRASS("v.db.addcolumn",
            flags = c('quiet'),
            parameters = list(map = 'sites',
                              columns = 'netID int, rid int'))
  execGRASS('db.execute',
            parameters = list(
              sql="UPDATE sites SET rid=(SELECT rid FROM edges WHERE sites.cat_edge=edges.cat)"
            ))
  execGRASS('db.execute',
            parameters = list(
              sql="UPDATE sites SET netID=(SELECT netID FROM edges WHERE sites.cat_edge=edges.cat)"
            ))

  # Calculate upDist ---------
  # MiKatt: Distance of every raster cell from the outlet
  message('Calculating upDist...\n')
  execGRASS('r.stream.distance',
            flags = c('overwrite', 'quiet', 'o'),
            parameters = list(
              stream_rast = 'streams_r',
              direction = 'dirs',
              method = 'downstream',
              distance = 'upDist'
            ))
  execGRASS("v.db.addcolumn",
            parameters = list(map = 'sites',
                              columns = 'upDist double'))
  execGRASS('v.what.rast',
            flags = c('quiet'),
            parameters = list(
              map = 'sites',
              raster = 'upDist',
              column = 'upDist'
            ))
  execGRASS("v.db.update", flags = c("quiet"),
            parameters = list(
              map = "sites",
              column = "upDist",
              value = "round(upDist, 2)"
            ))

  # Calculate distRatio = distance from lower end of edge to site / length edge
  message('Calculating distance ratio...\n')
  execGRASS("v.db.addcolumn",
            flags = c('quiet'),
            parameters = list(
              map = 'sites',
              columns = 'distRatio double precision'
            ))
  sql_str <- paste0("UPDATE sites SET distRatio=1-",
                    "round((((SELECT upDist FROM edges WHERE edges.cat=sites.cat_edge)-upDist)),2)/",
                    "(SELECT Length FROM edges WHERE edges.cat=sites.cat_edge)")
  execGRASS('db.execute',
            parameters = list(
              sql=sql_str
            ))

  # MiKatt: Moved to function calculate_sites_attributes
  # # Calculate and H20predictors for each site -----
  # # MiKatt: ! There might be a faster way to do that
  # message('Calculating H20Area...\n')
  #
  # sites <- readVECT('sites', ignore.stderr = FALSE)
  # take_area <- NA
  # take_avg_slope <- NA
  # #! add here more predictors!
  # for (i in seq_along(sites@data$pid)) {
  #   take <- sites@data$pid[i]
  #   # subset sites
  #   execGRASS('v.extract',
  #             flags = c("overwrite", "quiet"),
  #             parameters = list(
  #               input = 'sites',
  #               where = paste0('pid=', take),
  #               output = 'take_site'))
  #   # calc drainage area
  #   execGRASS("r.stream.basins",
  #             flags = c("overwrite", "l", "quiet"),
  #             parameters = list(direction = "dirs",
  #                               points = 'take_site',
  #                               basins = "take_area"))
  #   # calc drainage area size
  #   take_area[i] <- strsplit(execGRASS('r.stats',
  #                                   flags = c('a', 'quiet'),
  #                                   parameters = list(input = 'take_area'),
  #                                   intern = TRUE)[1], split = ' ')[[1]][[2]]
  #
  #   # calc average slope per watershed
  #   # set mask to the current basin
  #   execGRASS("r.mask",
  #             flags = c("overwrite", "quiet"),
  #             parameters = list(
  #               raster = "take_area"))
  #   avg <- execGRASS("r.univar",
  #             flags = c("overwrite", "quiet","g"),
  #             parameters = list(
  #               map = "slope"), intern = TRUE)
  #   avg <- setDT(data.frame(do.call(rbind,strsplit(avg,"="))))
  #   take_avg_slope[i] <- avg[X1 == "mean",as.numeric(as.character(X2))]
  #   execGRASS("r.mask",
  #             flags = c("r", "quiet"))
  #
  # }
  # # MiKatt: For edges, area was divided by 10000. Changed it there to have the same units
  # sites$H2OArea <- round(as.numeric(take_area), 2)
  # sites$avgSlope <- round(as.numeric(take_avg_slope), 2)
  # sites$cat_ <- NULL
  # writeVECT(sites, vname = 'sites',
  #           v.in.ogr_flags = c('overwrite', 'quiet'),
  #           ignore.stderr = TRUE)

  #####
  # # MiKatt does not work!
  # execGRASS("g.copy",
  #           flags = c('overwrite', 'quiet'),
  #           parameters = list(
  #             vector = 'edges,edges_e'))
  #
  # # additionally break streams at sites
  # dt.coord<-do.call(rbind,strsplit(
  #   execGRASS("v.to.db", flags =c("p","quiet"),
  #             parameters = list(
  #               map = "sites",
  #               option = "coor"
  #             ),intern=T),
  #   split = '\\|'))
  # colnames(dt.coord)<-c("cat","x","y","z")
  # dt.coord <- data.table(dt.coord[,-4])
  # dt.coord[,':='(x = as.numeric(x), y = as.numeric(y),cat = as.numeric(cat))]
  #
  # execGRASS("v.edit",
  #           flags = c('quiet',"overwrite"),
  #           parameters = list(
  #             map = 'edges_e',
  #             type = 'line',
  #             tool = 'break',
  #             threshold = 1,
  #             #where = paste0('stream = ',df.junctions[i,"stream"]),
  #             coords = c(dt.coord[,c(x,y),by=cat][,V1])
  #               #c(dt.coord[,x],dt.coord[,y]) # c(3506012.25,5778000,3502071.75,5771477.75) # #paste0("c(",paste(dt.coord[,x],dt.coord[,y],collapse = ",",sep=","),")")
  #           ))
  # execGRASS("v.category", flags=c("overwrite"),
  #           parameters = list(
  #             input = "edges_e",
  #             output = "edges_e2",
  #             cat = -1,
  #             option = "del"
  #           ))
  #
  # execGRASS("v.category", flags=c("overwrite","quiet"),
  #           parameters = list(
  #             input = "edges_e2",
  #             output = "edges_e",
  #             option = "add"
  #           ), ignore.stderr = T)
  #
  # execGRASS("v.to.rast", flags = c("overwrite", "quiet"),
  #           parameters = list(
  #             input = "edges_e",
  #             type = "line",
  #             output = "edges_re",
  #             use = "cat"
  #           ))
  # execGRASS("r.stream.basins",
  #           flags = c("overwrite", "quiet"),
  #           parameters = list(direction = 'dirs',
  #                             stream_rast = 'edges_re',
  #                             basins = "rca_e"))
  # areas <- do.call(rbind,
  #                  strsplit(execGRASS('r.stats',
  #                                     flags = c('a', 'quiet'),          # MiKatt: a -> m²
  #                                     parameters = list(input = 'rca_e'),
  #                                     intern = TRUE),
  #                           split = ' '))
  # # last row is total and not needed
  # areas <- areas[-nrow(areas), ]
  #
  # dt.streams <- do.call(rbind,strsplit(
  #   execGRASS("db.select",
  #             parameters = list(
  #               sql = "select cat,stream, next_stream, prev_str01,prev_str02,rid,H2OArea from edges_e"
  #             ),intern = T),
  #   split = '\\|'))
  # colnames(dt.streams)<-dt.streams[1,]
  # dt.streams<-data.table(dt.streams[-1,])
  # dt.streams[, ':=' (cat = as.numeric(cat),stream = as.numeric(stream), next_stream = as.numeric(next_stream),
  #                    prev_str01 = as.numeric(prev_str01), prev_str02 = as.numeric(prev_str02),
  #                    rid = as.numeric(rid), H2OArea = as.numeric(H2OArea))]
  # a<-data.table(as.numeric(areas[,1]),as.numeric(areas[,2]))
  # names(a)<-c("cat","area")
  # dt.streams<-merge(dt.streams,a,by="cat",all=T)
  # setkey(dt.streams,stream)
  #
  # sites.rid <- as.numeric(execGRASS("db.select",
  #                                   parameters = list(
  #                                     sql = "select rid from sites"
  #                                   ),intern = T)[-1])

}

