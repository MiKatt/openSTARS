#' Derive stream network from DEM.
#'
#' Streams are derived from a digital elevation model (DEM) using the GRASS function
#' \href{https://grass.osgeo.org/grass78/manuals/r.stream.extract.html}{r.stream.extract}.
#' If a stream network is available (see \code{\link{import_data}}) and burn > 0
#' it will be first burnt into DEM. Stream topology is derived using the GRASS function
#' \href{https://grass.osgeo.org/grass78/manuals/addons/r.stream.order.html}{r.stream.order}.
#'
#' @param burn numeric; how many meters should the streams be burned into the
#'   DEM? Only applicable if a mapped stream network is provided in \code{\link{import_data}}.
#'   Defaults to 0.
#' @param accum_threshold integer; accumulation threshold to use (i.e. minimum
#'   flow accumulation value in cells that will initiate a new stream). A small value
#'   results in many small streams. Defaults to 700 but a reasonable value 
#'   strongly depends on the raster resolution. See details below.
#' @param min_stream_length integer: minimum stream length in number of DEM
#'   raster cells; shorter first order stream segments are deleted. Defaults to 0
#'   but a reasonable value strongly depends on the raster resolution. See details below.
#' @param condition logical; should the DEM be conditioned using the GRASS function
#'   \href{https://grass.osgeo.org/grass78/manuals/addons/r.hydrodem.html}{r.hydrodem};
#'    default: TRUE.
#' @param dem_name character vector, optional; default: 'dem'; useful if
#'   conditioned and / or burnt in DEM raster from previous runs shall be used.
#' @param clean logical; should intermediate raster layer of imported streams
#'   ('streams_or') be removed from the GRASS session? Defaults to TRUE.
#' @param mem logical; should -m flag in the GRASS function 
#' \href{https://grass.osgeo.org/grass78/manuals/r.watershed.html}{r.watershed} 
#'  be used (for data preparation)? Defaults to FALSE; if set to TRUE the calculation
#'  uses disk swap mode, i.e. it is not carried out in the RAM but also using disk space.
#'  Useful for large data sets but also slower.
#'   
#' @return Nothing. The function produces the following maps:
#' \itemize{
#'  \item{'streams_v'} {derived streams with topology (vector)}
#'  \item{'dirs'} {flow directions (raster)}
#'  \item{'accums'} {accumulation values (raster)}
#'  \item{'dem_cond'} {conditioned dem (raster) if  \code{condition} is TRUE}
#'  \item{'dem_[cond]_burn[X]'} {burnt in DEM (raster) if burn is > 0}
#' } The original GRASS map 'dem' is not modified if \code{condition} is TRUE and / or \code{burn} > 0.
#'
#' @details For details on \code{accum_threshold} and \code{min_stream_length}
#' see the parameters 'threshold' and 'stream_length' at
#' \href{https://grass.osgeo.org/grass78/manuals/r.stream.extract.html}{r.stream.extract}.
#' It might be useful to not burn in the whole available stream network but only 
#' parts of it (e.g., larger streams with higher Strahler stream order only). 
#' For this, the stream network needs to be pre-processed (parts could be deleted)
#' before loading it with \code{import_data}.
#'
#' @note \code{\link{setup_grass_environment}} and \code{\link{import_data}}
#'   must be run before.

#' @author Mira Kattwinkel \email{mira.kattwinkel@@gmx.net}, Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#'
#' @examples
#' \donttest{
#' dem_path <- system.file("extdata", "nc", "elev_ned_30m.tif", package = "openSTARS")
#' if(.Platform$OS.type == "windows"){
#'   grass_program_path = "c:/Program Files/GRASS GIS 7.6"
#'   } else {
#'   grass_program_path = "/usr/lib/grass78/"
#'   }
#' 
#' setup_grass_environment(dem = dem_path, 
#'                         gisBase = grass_program_path,      
#'                         remove_GISRC = TRUE,
#'                         override = TRUE
#'                         )
#' gmeta()
#'                         
#' # Load files into GRASS
#' dem_path <- system.file("extdata", "nc", "elev_ned_30m.tif", package = "openSTARS")
#' sites_path <- system.file("extdata", "nc", "sites_nc.shp", package = "openSTARS")
#' streams_path <- system.file("extdata", "nc", "streams.shp", package = "openSTARS")
#' import_data(dem = dem_path, sites = sites_path, streams = streams_path)
#'
#' # Derive streams from DEM
#' derive_streams(burn = 10, accum_threshold = 700, condition = TRUE, clean = TRUE)
#' 
#' # Plot
#' library(sp)
#' dem <- readRAST('dem', ignore.stderr = TRUE)
#' sites <- readVECT('sites_o', ignore.stderr = TRUE)
#' streams_o <- readVECT('streams_o', ignore.stderr = TRUE)
#' streams <- readVECT('streams_v', ignore.stderr = TRUE)
#' plot(dem, col = terrain.colors(20))
#' lines(streams, col = 'blue', lwd = 2)
#' lines(streams_o, col = 'lightblue', lwd = 1)
#' legend("topright", col = c("lightblue", "blue"),  lwd = c(1,2), 
#'        legend = c("read in streams for burn in", "derived streams"))#' }
#' 

derive_streams <- function(burn = 0, accum_threshold = 700, condition = TRUE,
                           min_stream_length = 0, dem_name = NULL, clean = TRUE,
                           mem = FALSE) {

  if(condition == TRUE & (ifelse(is.null(dem_name), FALSE, dem_name != "dem")))
    stop("Only an unmodified DEM should be used for conditioning.")

  if(burn != 0 & (ifelse(is.null(dem_name), FALSE, grepl("burn", dem_name))))
    stop("Only an unburnt DEM should be used for burn in.")

  if(is.null(dem_name))
    dem_name <- "dem"
  dem_name_out <- dem_name

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
  if (!dem_name %in% rast)
    stop("DEM not found. Did you run import_data()?")


  if (condition) {
    message("Conditioning DEM ...")
    # MiKatt: Make 'mod' and 'size' user defined (optionally)?
    execGRASS("r.hydrodem",
              flags = c("overwrite"),
              parameters = list(
                input = dem_name,
                output = "dem_cond"
              ))
    dem_name <- "dem_cond"
    dem_name_out <- "dem_cond"
  }
  if ("streams_o" %in% vect & burn > 0) {
    message("Burning streams into DEM ...")
    # rasterize streams, set value to 1
    execGRASS("v.to.rast",
              flags = c("overwrite", "quiet"),
              parameters = list(
                input = "streams_o",
                type = "line",
                output = "streams_or",
                use = "val",
                value = 1
              ))

    dem_name_out <- paste0(dem_name, "_burn", burn)

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
                flags = c("overwrite", "quiet"),
                parameters = list(
                  expression =
                    paste0('\"dem2 = if(isnull(streams_or),  ', dem_name, ', ', dem_name,'-',
                         burn, ')\"')
                ))
      execGRASS("g.copy",
                flags = c("overwrite", "quiet"),
                parameters = list(
                  raster = paste0("dem2,", dem_name_out)
                  ))
      execGRASS("g.remove",
                flags = c("quiet", "f"),
                parameters = list(
                  type = "raster",
                  name = "dem2"
                ))
    } else{
      execGRASS("r.mapcalc",
                flags = c("overwrite", "quiet"),
                parameters = list(
                  expression =
                    paste0('\"',dem_name_out,' = if(isnull(streams_or),  ', dem_name, ', ', dem_name,'-',
                           burn, ')\"')
                ))
    }
    # MiKatt: remove temporary stream raster file 'streams_or'
    if (clean) {
      execGRASS("g.remove",
                flags = c("quiet", "f"),
                parameters = list(
                  type = "raster",
                  name = "streams_or"
                ))
    }
  }

  # calculate flow accumulation --------------
    if(mem){
      fl <- "m" 
    } else {
      fl <- NULL
    }
    execGRASS("r.watershed", flags = c("overwrite", "quiet", fl),
            parameters = list(
              elevation = dem_name_out,
              accumulation = "accums"
            ))

  # MiKatt: Using r.watershed to derive streams, flow directions and accumulation might be faster (plus r.to.vect and v.clean).
  # MiKatt: BUT -> produces many very small segments, often close to intersections
  # MiKatt: -> seems to be independent of the convercence value
  # MiKatt: -> r.thin does not help much
  message("Deriving streams from DEM ...")
  # MiKatt: Known Windows issue ([GRASS-dev] [GRASS GIS] #2919): "Missing value for parameter <d8cut>"; default value infinity is not used even if accumulation map is given.
  # MiKatt: Solution: set d8cut to total number of cells in g.region.
    ncell <- execGRASS("g.region",flags="p",intern=T)
    ncell <- as.numeric(unlist(strsplit(ncell[grep("cells",ncell)],split=":"))[2])
    execGRASS("r.stream.extract",
            flags =  c("overwrite", "quiet"),
            parameters = list(elevation = dem_name_out,
                              accumulation = "accums",
                              threshold = accum_threshold, # use ATRIC to get this value?
                              d8cut = ncell,
                              stream_length = min_stream_length,
                              stream_raster = "streams_r",
                              direction = "dirs"))

    # calculate stream topology ----------
    message("Calculating stream topology ...")
    # MiKatt: Is accumulation needed here? r.stream.order: "This map is an option only if Horton's or Hack's ordering is performed."
    # MiKatt: Yes, does not work without.
    execGRASS("r.stream.order",
              flags = c("overwrite", "quiet","z","m"),
              parameters = list(stream_rast = "streams_r",     # input
                                direction = "dirs",            # input
                                elevation = dem_name_out,      # input
                                accumulation = "accums",       # input
                                stream_vect = "streams_v"),    # output
              ignore.stderr=T)
    execGRASS("g.remove", flags = c("f", "quiet"),
              type = "raster",
              name = "streams_r"
    )

    # MiKatt: ESRI shape files must not have column names with more than 10 characters
    execGRASS("v.db.renamecolumn", flags = "quiet",
              parameters = list(
                map = "streams_v",
                column = "next_stream,next_str"
              ))
    # to keep column "next_str" next to prev_str
    execGRASS("v.db.renamecolumn", flags = "quiet",
              parameters = list(
                map = "streams_v",
                column = "flow_accum, flow_accu"
              ))

    # delete unused columns
    execGRASS("v.db.dropcolumn", flags = c("quiet"),
              parameters = list(
                map = "streams_v",
                columns = c("strahler","horton","shreve","hack","topo_dim","scheidegger","drwal_old","stright",
                            "sinosoid","source_elev","outlet_elev","elev_drop","out_drop","gradient")
              ))
    
    # delete stream segments with zero length
    a <- execGRASS("v.extract", flags = c("overwrite", "quiet"),
                   parameters = list(
                     input = "streams_v", 
                     output = "streams_v1",
                     type = "line",
                     where = paste0("length > 0")
                   ), intern = TRUE, ignore.stderr = TRUE)
    
    execGRASS("g.copy", flags = c("overwrite", "quiet"),
              parameters = list(
                vector = "streams_v1,streams_v"
                ), intern = TRUE, ignore.stderr = TRUE)
    
    execGRASS("g.remove", flags = c("quiet", "f"),
              parameters = list(
                type = "vector",
                name = "streams_v1"))
    
    message("Derived streams saved as 'streams_v'.")
}

#' Calculate RAM needed for deriving the stream network from DEM
#' 
#' See GRASS function \href{https://grass.osgeo.org/grass78/manuals/r.watershed.html}{r.watershed}.
#' 
#' @param dem character; path to DEM raster file.
#' 
#' @keywords internal
#' 
#' @return MB of RAM needed to derive the stream network with \code{mem = F} in 
#' \code{\link{derive_streams}}.
#' 
watershed_memory <- function(dem) {
  nc <-  raster::ncell(raster::raster(dem))
  ram <- 31 * nc / 1000000
  message('A maximum of ', ram, ' MB are needed to process this raster.')
  return(ram)
}
