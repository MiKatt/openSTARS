#' Calculate sites for SSN.
#'
#' @description
#' Calcuate sites for SSN object.
#'
#' Steps include:
#' \itemize{
#'  \item{Snap points to derived network. 'dist' give the distance to streams.}
#'  \item{Assign unique 'pid' and 'locID'.}
#'  \item{Set underlying 'rid' and 'netID' from stream network.}
#'  \item{Calculate upstream distance for each point ('upDist')}
#'  \item{Calculate watershed for each point, 'H2Oarea'}
#'  \item{Calculate watershed predictors for each point from raster files [currently not implemented]}
#' }
#'
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' library(rgrass7)
#' initGRASS(gisBase = "/usr/lib/grass70/",
#'   home = tempdir())
#' gmeta()
#' dem_path <- system.file("extdata", "nc", "elev_ned_30m.tif", package = "openSTARS")
#' sites_path <- system.file("extdata", "nc", "sites_nc.shp", package = "openSTARS")
#' import_data(dem = dem_path, sites = sites_path)
#' derive_streams()
#' calc_edges()
#' calc_sites()

#' dem <- readRAST('dem', ignore.stderr = TRUE)
#' sites <- readVECT('sites_o', ignore.stderr = TRUE)
#' edges <- readVECT('edges', ignore.stderr = TRUE)
#' plot(dem, col = terrain.colors(20))
#' points(sites, pch = 4)
#' lines(edges, col = 'blue')
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
  # add 2 columns holding distance id and coordinates of nearest streams
  execGRASS("v.db.addcolumn",
            parameters = list(
              map = "sites",
              columns = "dist double precision, xm double precision, ym double precision"
            ))
  # calc distance
  execGRASS("v.distance",
            flags = c("overwrite", 'quiet'),
            parameters = list(from = 'sites',
                              to = 'edges',
                              output = 'connectors',
                              upload = 'dist,to_x,to_y',
                              column = 'dist,xm,ym'))
  #! This is in R faster than in GRASS!? (which has to write to hard-drive)
  #! Other possibilities in GRASS to change coordinates?
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

  ### Setting pid -----------
  #! pid and locID identical???
  message('Setting pid and locID...\n')
  execGRASS("v.db.addcolumn",
            parameters = list(map = 'sites',
                              columns = 'pid int'))
  execGRASS("v.db.update",
            parameters = list(map = 'sites',
                              column = 'pid',
                              value = 'cat'))
  execGRASS("v.db.addcolumn",
            parameters = list(map = 'sites',
                              columns = 'locID int'))
  execGRASS("v.db.update",
            parameters = list(map = 'sites',
                              column = 'locID',
                              value = 'pid'))

  # Set netID and rid from network ---------
  message('Assigning netID and rid...\n')

  execGRASS("v.db.addcolumn",
            flags = c('quiet'),
            parameters = list(map = 'sites',
                              columns = 'netID int'))
  execGRASS("v.what.vect",
            parameters = list(map = 'sites',
                              column = 'netID',
                              query_map = 'edges',
                              query_column = 'netID',
                              dmax = 5))
  execGRASS("v.db.addcolumn",
            flags = c('quiet'),
            parameters = list(map = 'sites',
                              columns = 'rid int'))
  execGRASS("v.what.vect",
            parameters = list(map = 'sites',
                              column = 'rid',
                              query_map = 'edges',
                              query_column = 'rid',
                              dmax = 5))

  # Calculate and upDist ---------
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

  # Calculate and H20predictors for each site -----
  message('Calculating H20Area...\n')
  sites <- readVECT('sites', ignore.stderr = FALSE)
  take_area <- NA
  #! add here more predictors!
  for (i in seq_along(sites@data$pid)) {
    take <- sites@data$pid[i]
    # subset sites
    execGRASS('v.extract',
              flags = c("overwrite", "quiet"),
              parameters = list(
                input = 'sites',
                where = paste0('pid=', take),
                output = 'take_site'))
    # calc drainage area
    execGRASS("r.stream.basins",
              flags = c("overwrite", "l", "quiet"),
              parameters = list(direction = "dirs",
                                points = 'take_site',
                                basins = "take_area"))
    # calc drainage area size
    take_area[i] <- strsplit(execGRASS('r.stats',
                                    flags = c('a', 'quiet'),
                                    parameters = list(input = 'take_area'),
                                    intern = TRUE)[1], split = ' ')[[1]][[2]]
  }
  sites$H2OArea <- round(as.numeric(take_area), 2)
  sites$cat_ <- NULL
  writeVECT(sites, vname = 'sites',
            v.in.ogr_flags = c('overwrite', 'quiet'),
            ignore.stderr = TRUE)
}