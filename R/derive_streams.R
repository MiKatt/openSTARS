#' Derive stream network from DEM.
#'
#' Streams are derived from a DEM using
#' \href{https://grass.osgeo.org/grass70/manuals/r.stream.extract.html}{r.stream.extract}.
#' If a stream network is available (see \code{\link{import_data}}) and
#' burn > 0 it will be first burned into DEM.
#'
#' @param burn numeric; How many meters should the streams be burned in DEM.
#' @param at numeric; accumulation threshold to use.
#' @param condition logical; should conditioning using r.hydrodem run?
#' @param clean logical; Should intermediate layer be removed from GRASS session?
#'
#' @return Nothing. The function produces the following maps:
#' \itemize{
#'  \item{"streams_r"}{derived streams (raster)}
#'  \item{"streams_v"}{derived streams (vector)}
#'  \item{"dirs"}{flow directions (raster)}
#'  \item{"accums"}{accumulation values (raster)}
#' }
#'
#' @note \code{\link{import_data}} must be run before.
#' Intermediate layers cleaned are:
#' \itemize{
#'  \item{"streams_or"}{raster of imported streams (raster)}
#'  \item{"streams_vr"}{uncleaned vector of derived network (vector)}
#' }
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#'
#' @examples
#' \donttest{
#' library(rgrass7)
#' initGRASS(gisBase = "/usr/lib/grass70/",
#'   home = tempdir(),
#'   override = TRUE)
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
#' }

# MiKatt: I would suggest a different parameter name for 'at' because this is often a plotting parameter. Maybe 'accumthresh'?
derive_streams <- function(burn = 5, at = 700, condition = TRUE, clean = TRUE) {
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

  if (condition) {
    message('Conditioning DEM...\n')
    # MiKatt: Make 'mod' and 'size' user defined (optionally)?
    execGRASS('r.hydrodem',
              flags = c('overwrite'),
              parameters = list(
                input = 'dem',
                output = 'dem'
              ))
  }


  if ('streams_o' %in% vect & burn > 0) {
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

    # burn streams into dem -------------
    #! is r.carve?
    # MiKatt: r.carcve seems to be intended for this use but not yet ready ("The module does not operate yet in latitude-longitude locations. It has not been thoroughly tested, so not all options may work properly - but this was the intention. "). Unclear, what's the meaning of "subtracts a default-depth + additional-depth from a DEM".
    execGRASS("r.mapcalc",
              flags = c('overwrite', 'quiet'),
              parameters = list(
                expression =
                  paste0('\"dem = if(isnull(streams_or),  dem, dem-',
                         burn, ')\"')
              ))
    # MiKatt: remove temporary stream raster file 'streams_or'
    if (clean) {
      execGRASS("g.remove",
              flags = c('quiet', 'f'),
              parameters = list(
                type = 'raster',
                name = 'streams_or'
              ))
    }
  }
  
  # MiKatt: Using r.watershed to derive streams, flow directions and accumulation would be faster (plus r.to.vect and v.clean). 
  # MiKatt: ! Test speed difference with larger dem.
  # MiKatt: -> produces many very small segments, often close to intersections => Why?
  # MiKatt: -> that seems to be independent of the convercence value.
  # MiKatt: -> r.thin does not help much.
  # MiKatt: Would it make sense to calculate the accumulation raster first with r.watershed (is done down below for stream order) to use the same accumluation here?
  # MiKatt: -> seems to generate identical results.
  message('Deriving streams from DEM...\n')
  execGRASS("r.stream.extract",
            flags =  c('overwrite', 'quiet'),
            parameters = list(elevation = "dem",
                              threshold = at, # use ATRIC to get this value?
                              stream_raster = "streams_r",  # output raster
                              stream_vector = "streams_vr", # ouput vector
                              direction = 'dirs'))          # output raster flow direction

  # execGRASS('r.info',
  #           parameters = list(
  #             map = 'streams_r'
  #           ))

  # remove segments without length ------------
  execGRASS("v.clean",
            flags = c('overwrite', 'quiet'),
            parameters = list(
              input = "streams_vr",
              output = "streams_v",
              type = 'line',
              tool = 'rmline'
            ))
  if (clean) {
  # remove streams_vr
  execGRASS("g.remove",
            flags = c('quiet', 'f'),
            parameters = list(
              type = 'vector',
              name = 'streams_vr'
            ))
  }
  # calculate flow accumulation --------------
  execGRASS("r.watershed",
            flags = c('overwrite', 'quiet'),
            parameters = list(
              elevation = "dem",
              accumulation = 'accums'
            ))
} 