#' Derive stream network from DEM.
#'
#' Streams are derived from a DEM using
#' \href{https://grass.osgeo.org/grass70/manuals/r.stream.extract.html}{r.stream.extract}.
#' If a stream network is available (see \code{\link{import_data}}) and
#' burn > 0 it will be first burned into DEM.
#' Stream topolology is derived using
#' \href{https://grass.osgeo.org/grass70/manuals/r.stream.order.html}{r.stream.order}. 
#'
#' @param burn numeric; how many meters should the streams be burned into the DEM?
#' @param at numeric; accumulation threshold to use.
#' @param condition logical; should the DEM be conditioned using r.hydrodem?
#' @param clean logical; should intermediate layer be removed from GRASS session?
#'
#' @return Nothing. The function produces the following maps:
#' \itemize{
#'  \item{"streams_r"}{derived streams (raster)}
#'  \item{"streams_v"}{derived streams with topology (vector)}
#'  \item{"dirs"}{flow directions (raster)}
#'  \item{"accums"}{accumulation values (raster)}
#' }
#'
#' @note \code{\link{setup_grass_environment}} and \code{\link{import_data}} 
#' must be run before.
#' Intermediate layers cleaned are:
#' \itemize{
#'  \item{"streams_or"}{raster of imported streams (raster)}
#' }
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}, Mira Kattwinkel \email{kattwinkel-mira@@uni-landau.de}
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
#' setup_grass_environment(dem = dem_path, sites = sites_path)
#' import_data(dem = dem_path, sites = sites_path)
#' derive_streams()
#' dem <- readRAST('dem', ignore.stderr = TRUE)
#' sites <- readVECT('sites_o', ignore.stderr = TRUE)
#' streams <- readVECT('streams_v', ignore.stderr = TRUE)
#' plot(dem, col = terrain.colors(20))
#' points(sites, pch = 4)
#' lines(streams, col = 'blue')
#' }

# MiKatt: I would suggest a differnt parameter name for 'at' because this is often a plotting parameter. Maybe 'accumthresh'?
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
    # MiKatt: r.carve seems to be intended for this use but not yet ready 
    #  ("The module does not operate yet in latitude-longitude locations. 
    #  It has not been thoroughly tested, so not all options may work properly - 
    #  but this was the intention. "). Unclear, what't the meaning of 
    #  "subtracts a default-depth + additional-depth from a DEM".
    
    # MiKatt: On Windows r.mapcalc produces warings when trying to overwrite dem:
    # MiKatt: Unable to rename null file '.../PERMANENT/.tmp/unknown/548.1' to '.../PERMANENT/cell_misc/dem/null': File exists
    # MiKatt: Unable to rename cell file '.../PERMANENT/.tmp/unknown/548.0' to '.../PERMANENT/fcell/dem':  File exists
    # MiKatt: Solution: 1) write r.mapcalc results to dem2, 2) copy dem2 to dem, 3) delete dem2 
    if(.Platform$OS.type == "windows"){
      execGRASS("r.mapcalc",
                flags = c('overwrite', 'quiet'),
                parameters = list(
                  expression =
                    paste0('\"dem2 = if(isnull(streams_or),  dem, dem-',
                           burn, ')\"')
                ))
      execGRASS("g.copy",
                flags = c('overwrite', 'quiet'),
                parameters = list(
                  raster = 'dem2,dem'))
      execGRASS("g.remove",
                flags = c('quiet', 'f'),
                parameters = list(
                  type = 'raster',
                  name = 'dem2'
                ))
    } else{ 
      execGRASS("r.mapcalc",
                flags = c('overwrite', 'quiet'),
                parameters = list(
                  expression =
                    paste0('\"dem = if(isnull(streams_or),  dem, dem-',
                           burn, ')\"')
                ))
    }
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
  
  # calculate flow accumulation --------------
  # MiKatt: Is needed to derive stream topology (r.stream.order)
  # MiKatt: moved here from last step in this function so it can be used as 
  #  input for r.stream.extract (-> faster + identical streams?)
  # MiKatt: flow directions raster is slightly different, streams seem to be identical
  execGRASS("r.watershed",
            flags = c('overwrite', 'quiet'),
            parameters = list(
              elevation = "dem",
              accumulation = 'accums'
            ))
  
  # MiKatt: Using r.watershed to derive streams, flow directions and accumulation might be faster (plus r.to.vect and v.clean). 
  # MiKatt: -> produces many very small segments, often close to intersections => Why?
  # MiKatt: -> seems to be independent of the convercence value.
  # MiKatt: -> r.thin does not help much.
  message('Deriving streams from DEM...\n')
  # MiKatt: streams_vr (vector map) is not needed for the subsequent calculations; removed to speed up calculations
  # MiKatt: Known Windows issue ([GRASS-dev] [GRASS GIS] #2919): "Missing value for parameter <d8cut>"; default value infinity is not used even if accumulation map is given.
  # MiKatt: Solution: set d8cut to total number of cells in g.region.
    ncell <- execGRASS("g.region",flags="p",intern=T)
    ncell <- as.numeric(unlist(strsplit(ncell[grep("cells",ncell)],split=":"))[2])
    execGRASS("r.stream.extract",
            flags =  c('overwrite', 'quiet'),
            parameters = list(elevation = "dem",
                              accumulation = "accums",
                              threshold = at, # use ATRIC to get this value?
                              d8cut = ncell,
                              stream_raster = "streams_r",  # output raster
                              direction = 'dirs'))          # output raster flow direction
    
    # MiKatt: Moved here from calc_edges() to be able to test for complex confluences after derive_streams()
    # calculate stream topology ----------
    message('Calculating stream topology...\n')
    # MiKatt: Is accumulation needed here? r.stream.order: "This map is an option only if Horton's or Hack's ordering is performed." 
    # MiKatt: Yes, does not work without.
    execGRASS("r.stream.order",
              flags = c('overwrite', 'quiet','z','m'),
              parameters = list(stream_rast = 'streams_r',     # input
                                direction = 'dirs',            # input
                                elevation = 'dem',             # input
                                accumulation = 'accums',       # input
                                stream_vect = 'streams_v'),    # output
              ignore.stderr=T)
              
    # delete unused columns
    execGRASS("v.db.dropcolumn", flags = c("quiet"),
              parameters = list(
                map = "streams_v",
                columns = c("strahler","horton","shreve","hack","topo_dim","scheidegger","drwal_old","stright",
                            "sinosoid","source_elev","outlet_elev","elev_drop","out_drop","gradient")
              ))
}