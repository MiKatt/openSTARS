#' Derive stream network from DEM.
#'
#' Streams are derived from a DEM using
#' \href{https://grass.osgeo.org/grass70/manuals/r.stream.extract.html}{r.stream.extract}.
#' If a stream network is available (see \code{\link{import_data}}) and
#' burn > 0 it will be first burned into DEM.
#'
#' @param burn numeric; How many meters should the streams be burned in DEM.
#' @param accumulation numeric; accumulation threshold to use.
#'
#' @return Nothing. The function produces the following maps:
#' \itemize{
#'  \item{"streams_r"}{derived streams (raster)}
#'  \item{"streams_v"}{derived streams (vector)}
#'  \item{"dirs"}{flow directions (raster)}
#'  \item{"accums"}{accumulation values (raster)}
#' }
#'
#' @note \code{\link{import_data}} must be run before
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#'
#' @examples
#' library(rgrass7)
#' initGRASS(gisBase = "/usr/lib/grass70/",
#'   home = tempdir())
#' gmeta()
#' dem_path <- system.file("extdata", "nc", "elev_ned_30m.tif", package = "openSTARS")
#' sites_path <- system.file("extdata", "nc", "sites_nc.shp", package = "openSTARS")
#' import_data(dem = dem_path, sites = sites_path)
#' derive_streams()
#' dem <- readRAST('dem', ignore.stderr = TRUE)
#' sites <- readVECT('sites_o', ignore.stderr = TRUE)
#' streams <- readVECT('streams_v', ignore.stderr = TRUE)
#' plot(dem, col = terrain.colors(20))
#' points(sites, pch = 4)
#' lines(streams, col = 'blue')


derive_streams <- function(burn = 5, at = 700) {
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
  if (!'dem' %in% rast)
    stop('DEM not found. Did you run import_data()?')
  if ('streams_o' %in% vect) {
    message('Burning streams into DEM...\n')
    # rasterize streams, set value to 1
    execGRASS("v.to.rast",
              flags = c('overwrite', 'quiet'),
              parameters = list(
                input = 'streams_o',
                type = 'line',
                output = 'streams_or',
                use = 'val',
                value = 1
              ))

    # burn streams 5m into dem -------------
    execGRASS("r.mapcalc",
              flags = c('overwrite', 'quiet'),
              parameters = list(
                expression =
                  paste0('\"dem = if(isnull(streams_or),  dem, dem-',
                         burn, ')\"')
              ))
    # remove streams_or
    execGRASS("g.remove",
              flags = c('quiet', 'remove'),
              parameters = list(
                rast = 'streams_or'
              ))
  }
  message('Deriving streams from DEM...\n')
  execGRASS("r.stream.extract",
            flags =  c('overwrite', 'quiet'),
            parameters = list(elevation = "dem",
                              threshold = at, # use ATRIC to get this value?
                              stream_raster = "streams_r",  # raster
                              stream_vector = "streams_v",  # vector
                              direction = 'dirs'))          # flow direction
  # remove segments without length ------------
  execGRASS("v.clean",
            flags = c('overwrite', 'quiet'),
            parameters = list(
              input = "streams_v",
              output = "streams_vc",
              type = 'line',
              tool = 'rmline'
            ))
  # calculate flow accumulation --------------
  execGRASS("r.watershed",
            flags = c('overwrite', 'quiet'),
            parameters = list(
              elevation = "dem",
              accumulation = 'accums'
            ))
}