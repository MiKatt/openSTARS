#' Calcuate attributes of the sites.
#'
#' For each site (observation or prediction) the total catchment area is
#' calculated ('H2OArea'). Additionally, other attributes (predictor variables)
#' can be derived based on given raster maps. This fuction calculates
#' exact values for catchments derived with
#' \href{https://grass.osgeo.org/grass70/manuals/addons/r.stream.basins.html}{r.stream.basins}
#' and can take considerable time if there are many sites.
#' Catchment raster maps can optionally be stored as "catchm_X" (X = pid).

#' @import progress
#'
#' @param sites_map character:name of the sites the attributes shall be
#' calculated for. "sites" refers to the observation or prediction sites.
#' @param input_raster character vector (optional); name of additional raster
#'   maps to calculate attributes from.
#' @param stat character vector (optional); statistics to be calulated, one of:
#'   min, max, mean, stddev, variance, sum, median or precentile_X (where X
#'   gives the desired percentile e.g. 25 for the first).
#' @param attr_name character vector (optional); column name for the attributes
#'   to be calculated. Attribute names must not be longer than 10 characters.
#' @param round_dig integer; number of digits to round results to. Can be a vector
#'   of different values or just one value for all attributes.
#' @param keep_basins boolean; shall raster maps of all the watersheds be kept?

#' @return Nothing. The function appends new columns to the 'sites' attribute table
#' \itemize{
#'  \item{'H2OArea':} {Total watershed area of the watershed upstream of each site.}
#'  \item{attr_name:} {Additional optional attributes calculated based on input_raster maps.}
#' }
#'
#' @note \code{\link{import_data}}, \code{\link{derive_streams}},
#'   \code{\link{calc_edges}} and \code{\link{calc_sites}} or
#'   \code{\link{calc_prediction_sites}} must be run before.

#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}, Mira Kattwinkel, \email{mira.kattwinkel@@gmx.net}
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

calc_attributes_sites_exact <- function(sites_map = "sites",
                                        input_raster = NULL,
                                        stat = NULL,
                                        attr_name = NULL,
                                        round_dig = 2,
                                        keep_basins = FALSE){

  if(length(input_raster) != length(stat) | length(input_raster) != length(attr_name) | length(attr_name) != length(stat))
    stop(paste0("There must be the same number of input raster files (",length(input_raster), "), statistics to calculate (",
                length(stat), ") and attribute names (", length(attr_name),")."))

  if(!is.null(stat) & any(stat %in% c("min","max", "mean", "stddev","variance","sum","median", "percent") + grepl("percentile", stat)) == 0) # TRUE = 1, FALSE = 0
    stop('Statistisc to calculate must be one of "min","max", "mean", "stddev","variance","sum", "median", "precentile_X" or "precent".')

  if(length(round_dig) == 1)
    round_dig <- rep(round_dig, length(stat)+1)
  if(length(round_dig) == length(stat))
    round_dig <- c(round_dig[1], round_dig)

  rast <- execGRASS("g.list",
                    parameters = list(
                      type = 'rast'
                    ),
                    intern = TRUE)
  if ("MASK" %in% rast)
    execGRASS("r.mask",flags = c("r", "quiet"))

  d.sites <- readVECT(sites_map, ignore.stderr = FALSE)

  message("Intersecting attributes for ",nrow(d.sites@data)," sites...\n")

  # progress bar
  pb <- progress_bar$new(total = nrow(d.sites@data))

  dat <- matrix(nrow = nrow(d.sites),ncol = length(attr_name)+2)
  colnames(dat) <- c("H2OArea", attr_name,"pid")
  for (i in seq_along(d.sites@data$pid)) {
    #message(i)
    pid <- d.sites@data$pid[i]
    dat[i,"pid"]  <- pid

    coord <- coordinates(d.sites[i,])
    # always calculate drainage area in km^2
    execGRASS("r.stream.basins",
              flags = c("overwrite", "l", "quiet"),
              parameters = list(direction = "dirs",
                               coordinates = coord,
                               basins = paste0("catchm_",pid)))
    dat[i,"H2OArea"] <- round(as.numeric(as.character(strsplit(
                            execGRASS('r.stats',
                                   flags = c('a', 'quiet'),
                                   parameters = list(input = paste0('catchm_',pid)),
                                   intern = TRUE)[1], split = ' ')[[1]][[2]]))/1000000,round_dig[1])

    # calculate unviriate statitics per watershed
    # set mask to the current basin
    execGRASS("r.mask",
              flags = c("overwrite", "quiet"),
              parameters = list(
                raster = paste0("catchm_",pid)))
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
        if(nrow(st) > 0){
          st[,X2 := as.numeric(as.character(X2))]
          if(grepl("percent", stat[j]))
            # if codes as 1 and 0, "mean" gives ratio
            dat[i,j+1] <- round(st[X1 == "mean",X2],round_dig[j+1])
            # This does not work
            # dat[i,j+1] <- round((st[X1 == "cells", X2] - st[X1 == "null_cells", X2])/
            #                      st[X1 == "cells", X2], round_dig[j+1])
          else
            dat[i,j+1] <- round(st[X1 == stat[j],X2],round_dig[j+1])
        } else
          dat[i,j+1] <- 0
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
                  name = paste0('catchm_',pid)
                ))
    }
    pb$tick()
  }

  # Join attributes to edges attribute table
  message("Joining tables...")
  dir.create("temp")
  write.csv(dat, file.path("temp","sites_attributes_exact.csv"),row.names = F)
  write.table(t(gsub("numeric","Real",apply(dat,2,class))),file.path("temp","sites_attributes_exact.csvt"),quote=T,sep=",",row.names = F,col.names = F)
  execGRASS("db.in.ogr", flags = c("overwrite","quiet"),
            parameters = list(
              input = file.path("temp","sites_attributes_exact.csv"),
              output = "sites_attributes_exact"
            ),ignore.stderr = T)
  execGRASS("v.db.join", flags = "quiet",
            parameters = list(
              map = sites_map,
              column = "pid",
              other_table = "sites_attributes_exact",
              other_column = "pid"
            ))
  unlink("temp", recursive = TRUE, force = TRUE)
  execGRASS("db.droptable", flags = c("quiet","f"),
            parameters = list(
              table = "sites_attributes_exact"
            ))

  ## MiKatt: Gives WARNING "Width for column XX set to 255 (was not specified by OGR),
  ##   some strings may be truncated!
  # d.sites@data <- merge(d.sites@data, dat, by="pid")
  # d.sites$cat_ <- NULL
  # writeVECT(d.sites, vname = 'sites',
  #          v.in.ogr_flags = c('overwrite', 'quiet'),
  #          ignore.stderr = TRUE)
}
