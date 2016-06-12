#' Calculate binary IDs for each stream network
#' @importFrom stats aggregate
#'
#' @return A list of data.frames (for each network one), with rid and the binary code for each segment.
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
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
#' calc_sites()
#' binaries <- calc_binary()
#' head(binaries[[1]])
calc_binary <- function(){
  vect <- execGRASS("g.list",
                    parameters = list(
                      type = 'vect'
                    ),
                    intern = TRUE)
  if (!'sites_o' %in% vect)
    stop('Sites not found. Did you run import_data()?')
  if (!'edges' %in% vect)
    stop('edges not found. Did you run calc_edges()?')
  if (!'sites' %in% vect)
    stop('sites not found. Did you run calc_sites()?')

  # for each network ------
  streams <- readVECT('streams_topo', type = 'line', ignore.stderr = TRUE)

  # for each segment -----
  bins <- lapply(unique(streams$netID), function(y) {
    calc_binary_horse(streams[streams$netID == y, ])
  })
  names(bins) <- unique(streams$netID)
  return(bins)
}

#' workhorse for calc_binary
#' @param network network ID
#' @keywords internal
calc_binary_horse <- function(network) {
  # empty id cols
  network$bin_id <- rep(NA, nrow(network))
  # for each topological dimension
  for (i in sort(unique(network$topo_dim))) {
    rows <- which(network$topo_dim == i)
    # first segment set to one
    if (i == 1) {
      network$bin_id[rows] <- 1
    } else {
      # actual segments
      take_segments <- network[rows, ]@data
      # downstream segments
      take_down <- network[network$topo_dim == i - 1, ]@data
      names(take_down)[2] <- 'stream_down'
      # merge dwn (with bin_id) and actual
      take_merge <- merge(take_down[ , c('stream_down', 'bin_id')],
                          take_segments, by.x = 'stream_down', by.y = 'next_stream')
      # assign 0/1 and paste with downstream id
      take_merge[ , 'bin_id'] <-  c(aggregate(bin_id.x ~ stream_down, data = take_merge,
                FUN = function(x) paste0(x, sample(c(0, 1), 2)))[ , 'bin_id.x'])
      take_merge$bin_id.x <- NULL
      take_merge$bin_id.y <- NULL
      network$bin_id[rows] <- take_merge$bin_id
    }
  }
  out <- network@data[ , c('rid', 'bin_id')]
  names(out) <- c("rid", "binaryID")
  return(out)
}