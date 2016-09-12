#' Calcuate attributes of the sites.
#'
#' For each site the total catchment area is calculated. Additionally, other 
#' attributes can be derived based on given raster maps. This fuction calculates
#' exact values for catchments derived with 
#' \href{https://grass.osgeo.org/grass70/manuals/addons/r.stream.basins.html}{r.stream.basins}.
#' This can take a considerable time if there are many sites.
#' Catchment raster maps can optionally be stored as "catchm_X" (X = pid). 
#' 
#' @import progress
#'
#' @param input_raster character vector (optional); name of additional raster 
#'   maps to calculate attributes from.
#' @param stat character vector (optionlal); statistics to be calulated, one of:
#'   "min","max", "mean", "stddev","variance","sum", "median" or "precentile_X".
#' @param attr_name character vector; column name for the attributes to be caculated.
#' @param round_dig integer; number of digits to round results to, default = 2.
#' @param keep_basins boolean; shall raster maps of all the watersheds be kept? default = FALSE

#' @return Nothing. The function appends new columns to the sites feature attribute table
#' \itemize{
#'  \item{"H2OAreakm2"}{Total watershed area of the watershed upstream of each site.}
#'  \item{attr_name}{Additional optional attributes calculated based on input_raster maps.}
#' }
#'
#' @note \code{\link{import_data}}, \code{\link{derive_streams}}, 
#'   \code{\link{calc_edges}} and code{\link{calc_sites}} must be run before.

#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}, Mira Kattwinkel, \email{kattwinkel-mira@@uni-landau.de}
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
#' import_data(dem = dem_path, sites = sites_path)
#' derive_streams()
#' #' cj <- check_compl_junctions()
#' if(cj)
#'   correct_compl_junctions()
#' calc_edges()
#' calc_sites()
#' execGRASS("r.slope.aspect", flags = c("overwrite","quiet"),
#' parameters = list(
#'   elevation = "dem",
#'   slope = "slope"
#'   ))
#' calc_sites_attributes(input_raster = "slope",  stat = "mean", attr_name = "avgSlopeP")
#' }

calc_sites_attributes <- function(input_raster = NULL, stat = NULL, attr_name = NULL, round_dig = 2, keep_basins = FALSE){
  
  if(length(input_raster) != length(stat) | length(input_raster) != length(attr_name) | length(attr_name) != length(stat))
    stop(paste0("There must be the same number of input raster files (",length(input_raster), "), statistics to calculate (",
                length(stat), ") and attribute names (", length(attr_name),")."))
  
  if(any(!stat %in% c("min","max", "mean", "stddev","variance","sum","median")))
    stop('Statistisc to calculate must be one of "min","max", "mean", "stddev","variance","sum", "median" or "precentile_X".')
  
  if(length(round_dig) == 1)
    round_dig <- rep(round_dig, length(stat)+1)
  
  rast <- execGRASS("g.list",
                    parameters = list(
                      type = 'rast'
                    ),
                    intern = TRUE)
  if ("MASK" %in% rast)
    execGRASS("r.mask",flags = c("r", "quiet"))
  
  d.sites <- readVECT("sites", ignore.stderr = FALSE)
  
  message("Intersecting attributes of ",nrow(d.sites@data)," sites...\n")
  
  # progress bar
  pb <- progress_bar$new(total = nrow(d.sites@data))

  dat <- matrix(nrow = nrow(d.sites),ncol = length(attr_name)+2)
  colnames(dat) <- c("H2OArea", attr_name,"pid")
  for (i in seq_along(d.sites@data$pid)) {
    #message(i)
    take <- d.sites@data$pid[i]
    dat[i,"pid"] <- take
    coord <- coordinates(d.sites[i,])
    # MiKatt: no need to write point vector to hard drive; just use coordinates as outlet
    # subset sites
    # execGRASS('v.extract',
    #           flags = c("overwrite", "quiet"),
    #           parameters = list(
    #             input = sites,
    #             where = paste0('pid=', take),
    #             output = 'take_site'))
    
    # always calculate drainage area in km^2
    execGRASS("r.stream.basins",
              flags = c("overwrite", "l", "quiet"),
              parameters = list(direction = "dirs",
                               # points = 'take_site',
                               coordinates = coord,
                                basins = paste0("catchm_",i)))
    dat[i,"H2OArea"] <- round(as.numeric(as.character(strsplit(execGRASS('r.stats',
                                   flags = c('a', 'quiet'),
                                   parameters = list(input = paste0('catchm_',i)),
                                   intern = TRUE)[1], split = ' ')[[1]][[2]])),round_dig[1])/1000000
    
    # calculate unviriate statitics per watershed
    # set mask to the current basin
    execGRASS("r.mask",
              flags = c("overwrite", "quiet"),
              parameters = list(
                raster = paste0("catchm_",i)))
    if(length(stat) > 0){
      for(j in 1:length(stat)){
        if(stat[j] == "median"){
          st <- execGRASS("r.univar",
                          flags = c("overwrite", "quiet","g", "e"),
                          parameters = list(
                            map = input_raster[j]), intern = TRUE)
        } else {
          if(grepl("precentile",stat[j])){
            p <- as.numeric(as.character(unlist(strsplit(stat[j],"_"))[2]))
            st <- execGRASS("r.univar",
                            flags = c("overwrite", "quiet","g", "e"),
                            parameters = list(
                              map = input_raster[j], 
                              percentile = p), intern = TRUE)
          } else{
            st <- execGRASS("r.univar",
                            flags = c("overwrite", "quiet","g"),
                            parameters = list(
                              map = input_raster[j]), intern = TRUE)
          }
        }
        st <- setDT(data.frame(do.call(rbind,strsplit(st,"="))))
        dat[i,j+1] <- round(st[X1 == stat[j],as.numeric(as.character(X2))],round_dig[j+1])
      }
    }
    # Remove the mask!
    execGRASS("r.mask",
              flags = c("r", "quiet"))
    
    # Delete watershed raster
    if (!keep_basins) {
      execGRASS("g.remove",
                flags = c('quiet', 'f'),
                parameters = list(
                  type = 'raster',
                  name = paste0('catchm_',i)
                ))
    }
    pb$tick()
  }
  d.sites@data <- merge(d.sites@data, dat, by="pid")
  d.sites$cat_ <- NULL
  writeVECT(d.sites, vname = 'sites',
            v.in.ogr_flags = c('overwrite', 'quiet'),
            ignore.stderr = TRUE)
}