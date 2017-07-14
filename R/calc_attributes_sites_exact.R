#' Calcuate attributes of the sites.
#'
#' For each site (observation or prediction) the total catchment area is
#' calculated ('H2OArea'). Additionally, other attributes (predictor variables)
#' can be derived based on given raster maps. This fuction calculates
#' exact values for catchments derived with
#' \href{https://grass.osgeo.org/grass70/manuals/addons/r.stream.basins.html}{r.stream.basins}
#' and can take considerable time if there are many sites.
#' Catchment raster maps can optionally be stored as "sitesname_catchm_X" (X = locID).

#' @import progress
#'
#' @param sites_map character; name of the sites (observation or prediction) 
#' attributes shall be calculated for. "sites" (default) refers to the observation sites.
#' @param input_raster character vector (optional); name of additional raster
#'   maps to calculate attributes from.
#' @param stat character vector (optional); statistics to be calulated, one of:
#'   min, max, mean, stddev, variance, sum, median or precentile_X (where X
#'   gives the desired percentile e.g. 25 for the first). Must be provided if 
#'   \code{input_raster} are given.
#' @param attr_name character vector (optional); column name for the attributes
#'   to be calculated. Attribute names must not be longer than 10 characters.
#'   Must be provided if \code{input_raster} are given.
#' @param round_dig integer; number of digits to round results to. Can be a vector
#'   of different values or just one value for all attributes.
#' @param calc_basin_area boolean; shall the catchment area be calculated? (Useful
#'  if the fuction has been called before with \code{keep_basins = TRUE}.)
#' @param keep_basins boolean; shall raster maps of all the watersheds be kept?
#' @param temp_dir string; temporary directory with read and write access to 
#'   store intermediate files.
#'
#' @return Nothing. The function appends new columns to the \code{sites_map} attribute table
#' \itemize{
#'  \item{'H2OArea':} {Total watershed area of the watershed upstream of each site.}
#'  \item{attr_name:} {Additional optional attributes calculated based on input_raster maps.}
#' }
#' Please note that for sampling points that lie in the same dem raster cell 
#'  along a stream identical values are calculated, because identical watersheds
#'  are derived.
#'
#' @note \code{\link{import_data}}, \code{\link{derive_streams}},
#'   \code{\link{calc_edges}} and \code{\link{calc_sites}} or
#'   \code{\link{calc_prediction_sites}} must be run before.
#'   
#' If \code{calc_basin_area = F} but there are no raster maps called 'sitesname_catchm_x' 
#' with x = locID of all sites the catchments (and their area) are derived. 
#'   
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}, Mira Kattwinkel, \email{mira.kattwinkel@@gmx.net}
#' @export
#' @examples
#' \donttest{
#' # Initiate GRASS session
#' initGRASS(gisBase = "/usr/lib/grass72/",
#'     home = tempdir(),
#'     override = TRUE)
#'
#' # Load files into GRASS
#' dem_path <- system.file("extdata", "nc", "elev_ned_30m.tif", package = "openSTARS")
#' sites_path <- system.file("extdata", "nc", "sites_nc.shp", package = "openSTARS")
#' setup_grass_environment(dem = dem_path, sites = sites_path)
#' import_data(dem = dem_path, sites = sites_path)
#' gmeta()
#'
#' # Derive streams from DEM
#' derive_streams(burn = 0, accum_threshold = 700, condition = TRUE, clean = TRUE)
#'
#' # Prepare edges
#' calc_edges()
#' execGRASS("r.slope.aspect", flags = c("overwrite","quiet"),
#' parameters = list(
#'   elevation = "dem",
#'   slope = "slope"
#'   ))
#' calc_attributes_edges(input_raster = "slope", stat = "max", attr_name = "maxSlo")
#'
#' # Prepare sites
#' calc_sites()
#' calc_attributes_sites_approx(input_attr_name = "maxSlo", stat = "max")
#' calc_attributes_sites_exact(input_raster = "slope", attr_name = "maxSloE", stat = "max")
#'
#' # Plot data
#' dem <- readRAST('dem', ignore.stderr = TRUE)
#' edges <- readVECT('edges', ignore.stderr = TRUE)
#' sites <- readVECT('sites', ignore.stderr = TRUE)
#' plot(dem, col = terrain.colors(20))
#' mm <- range(c(edges$maxSlo_e,sites$maxSlo,sites$maxSloE))
#' b <- seq(from=mm[1],to=mm[2]+diff(mm)*0.01,length.out=10)
#' c_ramp <- colorRampPalette(c("blue", "red"))
#' cols <- c_ramp(length(b))[as.numeric(cut(edges$maxSlo_e,breaks = b,right=F))]
#' plot(edges,col=cols,add=T, lwd=2)
#' cols <- c_ramp(length(b))[as.numeric(cut(sites$maxSlo,breaks = b,right=F))]
#' points(sites, pch = 19, col = cols)
#' cols <- c_ramp(length(b))[as.numeric(cut(sites$maxSloE,breaks = b,right=F))]
#' points(sites, pch = 21,bg = cols,cex=0.7)
#' # points in the middle of the map indicate a difference in max slope between
#' # approximate and exact calculation
#' }

calc_attributes_sites_exact <- function(sites_map = "sites",
                                        input_raster = NULL,
                                        stat = NULL,
                                        attr_name = NULL,
                                        round_dig = 2,
                                        calc_basin_area = TRUE,
                                        keep_basins = FALSE,
                                        temp_dir = "temp"){

  if(length(input_raster) != length(stat) | length(input_raster) != length(attr_name) | length(attr_name) != length(stat))
    stop(paste0("There must be the same number of input raster files (",length(input_raster), "), statistics to calculate (",
                length(stat), ") and attribute names (", length(attr_name),")."))

  if(!is.null(stat) & any(stat %in% c("min","max", "mean", "stddev","variance","sum","median", "percent") + grepl("percentile", stat)) == 0) # TRUE = 1, FALSE = 0
    stop('Statistisc to calculate must be one of "min","max", "mean", "stddev","variance","sum", "median", "precentile_X" or "precent".')

  if(is.null(stat) & !calc_basin_area)
    stop("Either the catchment areas are calculated or a statistic to calculate must be provided.")
  
  if(temp_dir == "temp")
    temp_dir <- file.path(path.expand("~"), temp_dir)
  
  if(length(round_dig) == 1)
    round_dig <- rep(round_dig, length(stat)+1)
  if(length(round_dig) == length(stat))
    round_dig <- c(round_dig[1], round_dig)

  rast <- execGRASS("g.list",
                    parameters = list(
                      type = "rast"
                    ),
                    intern = TRUE)
  if ("MASK" %in% rast)
    execGRASS("r.mask",flags = c("r", "quiet"))

  d.sites <- readVECT(sites_map, ignore.stderr = FALSE)
  
  if(!all(paste0(sites_map,"_catchm_",d.sites@data$locID) %in% rast)){
    calc_basin_area <- TRUE
  }
  if(any(d.sites@data$ratio == 0) & calc_basin_area){
    d.edges <- readVECT("edges", ignore.stderr = FALSE)
    dt.edges <- setDT(d.edges@data)
    dt.edges[, colnames(dt.edges)[-which(colnames(dt.edges) %in% c("cat", "stream","prev_str01","prev_str02","rid","H2OArea"))] := NULL]
    rm(d.edges)
  }

  message("Intersecting attributes for ",nrow(d.sites@data)," sites...\n")

  # progress bar
  pb <- progress_bar$new(total = nrow(d.sites@data))
  
  locIDs <- unique(d.sites@data$locID)
  if(calc_basin_area){
    dat <- matrix(nrow = length(locIDs), ncol = length(attr_name)+2)
    colnames(dat) <- c("H2OArea", attr_name, "locID")
  } else {
    dat <- matrix(nrow = length(locIDs), ncol = length(attr_name) + 1)
    colnames(dat) <- c(attr_name, "locID")
  }
  for (i in seq_along(locIDs)) {
    #message(i)
    locID <- locIDs[i]
    dat[i,"locID"]  <-  locID
    # get first entry from d.sites@data with locID
    ii <- which(d.sites@data$locID == locID)[1] 

    # calculate drainage area in km^2
    if(calc_basin_area){
      # If the distance ratio is 0, the site lies within the outflow cell of
      # the edge; then, r.stream.basins will extract a too large basin including
      # the second tributary of the confluence
      if(d.sites@data$ratio[ii] == 0){
        j <- which(dt.edges[, "rid"] == d.sites@data$rid[ii])
        # use $ here to get single value from data.table to match dimensions of dat
        dat[i,"H2OArea"] <- round(dt.edges$H2OArea[j], round_dig[1])
        cats <- get_cats_edges_in_catchment(dt.edges, dt.edges[j, "stream"])

        ## more than 106 categories in r.mask crashes (i.e. no MASK is created)
        ## workaround:
        n <- length(cats) %/% 100
        if(n == 0){
          execGRASS("r.mask", flags = c("overwrite","quiet"),
                    parameters = list(
                      raster = "rca",
                      maskcats = paste0(cats, collapse = " "))
          )
          execGRASS("r.mapcalc", flags = c("overwrite","quiet"),
                    parameters = list(
                      expression = paste0("'", paste0(sites_map, "_catchm_",locID), "=MASK'")
                    ))
          execGRASS("r.mask", flags = c("r","quiet")
          )
        } else {
          count <- 0
          for(j in 1:n){
            execGRASS("r.mask", flags = c("overwrite","quiet"),
                      parameters = list(
                        raster = "rca",
                        maskcats = paste0(cats[((j-1)*100+1):(j*100)], collapse = " "))
            )
            execGRASS("r.mapcalc", flags = c("overwrite","quiet"),
                      parameters = list(
                        expression = paste0("'temp", j, "=MASK'")
                      ))
            execGRASS("r.mask", flags = c("r","quiet")
            )
            count <- count + 1
          }
          if(length(cats) %% 100 != 0){
            execGRASS("r.mask", flags = c("overwrite","quiet"),
                      parameters = list(
                        raster = "rca",
                        maskcats = paste0(cats[(n*100+1):length(cats)], collapse = " "))
            )
            execGRASS("r.mapcalc", flags = c("overwrite","quiet"),
                      parameters = list(
                        expression = paste0("'temp", n+1, "=MASK'")
                      ))
            execGRASS("r.mask", flags = c("r","quiet")
            )
            count <- count + 1
          }
          m <- paste0("temp",1:count)
          m <- paste0("if(", paste0(m, collapse = "|||"),",1)")
          execGRASS("r.mapcalc", flags = c("overwrite","quiet"),
                    parameters = list(
                      expression = paste0('\"', paste0(sites_map, '_catchm_',locID), '=', m,'\"')
                    ))
          execGRASS("g.remove", flags = c("f","quiet"),
                    parameters = list(
                      type = "raster",
                      name = paste0("temp",1:count)
                    ))
          
        }
      } else {
        coord <- coordinates(d.sites[ii,])
        execGRASS("r.stream.basins",
                  flags = c("overwrite", "l", "quiet"),
                  parameters = list(direction = "dirs",
                                    coordinates = coord,
                                    basins = paste0(sites_map, "_catchm_",locID)))
        dat[i,"H2OArea"] <- round(as.numeric(as.character(strsplit(
          execGRASS("r.stats",
                    flags = c("a", "quiet"),
                    parameters = list(input = paste0(sites_map, "_catchm_",locID)),
                    intern = TRUE)[1], split = ' ')[[1]][[2]]))/1000000,round_dig[1])
      }
    }
    # calculate unviriate statistics per watershed
    # set mask to the current basin
    execGRASS("r.mask",
              flags = c("overwrite", "quiet"),
              parameters = list(
                raster = paste0(sites_map, "_catchm_",locID)))
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
          if(grepl("percent", stat[j])){
            if(st[X1=="variance",X2] == 0){  # if coded as something and NA, null(), no data value
              st2 <- execGRASS("r.univar",
                               flags = c("overwrite", "quiet","g"),
                               parameters = list(
                                 map = "MASK"), intern = TRUE)
              n <- st2[grep("^n=", st2)]
              n <- as.numeric(substring(n,3,nchar(n)))
              st <- rbindlist(list(st, data.table(X1 = "n2", X2 = n)))
              dat[i,attr_name[j]] <- round(st[X1 == "n", X2] / st[X1 == "n2", X2], round_dig[j + 1])
              # this was wrong cells and non_null_cells stell refers to the 
              # whole map; MASK only sets to null value
              # dat[i,j+1] <- round((st[X1 == "cells", X2] - st[X1 == "null_cells", X2])/
              #                     st[X1 == "cells", X2], round_dig[j+1])
            } else{  # if coded as 1 and 0, "mean" gives ratio
              dat[i,attr_name[j]] <- round(st[X1 == "mean",X2],round_dig[j+1])
            }
          }else
            dat[i,attr_name[j]] <- round(st[X1 == stat[j],X2],round_dig[j+1])
        } else
          dat[i,attr_name[j]] <- 0
      }
    }
    # Remove the mask!
    execGRASS("r.mask",
              flags = c("r", "quiet"))

    # Delete watershed raster
    if (!keep_basins) {
      execGRASS("g.remove",
                flags = c("quiet", "f"),
                parameters = list(
                  type = "raster",
                  name = paste0(sites_map, "_catchm_",locID)
                ))
    }
    pb$tick()
  }

  # Join attributes to sites attribute table
  message("Joining tables...")
  dir.create(temp_dir)
  utils::write.csv(dat, file.path(temp_dir,"sites_attributes_exact.csv"),row.names = F)
  write.table(t(gsub("numeric","Real",apply(dat,2,class))),file.path(temp_dir,"sites_attributes_exact.csvt"),quote=T,sep=",",row.names = F,col.names = F)
  execGRASS("db.in.ogr", flags = c("overwrite","quiet"),
            parameters = list(
              input = file.path(temp_dir,"sites_attributes_exact.csv"),
              output = "sites_attributes_exact"
            ),ignore.stderr = T)
  execGRASS("v.db.join", flags = "quiet",
            parameters = list(
              map = sites_map,
              column = "locID",
              other_table = "sites_attributes_exact",
              other_column = "locID"
            ))
  unlink(temp_dir, recursive = TRUE, force = TRUE)
  execGRASS("db.droptable", flags = c("quiet","f"),
            parameters = list(
              table = "sites_attributes_exact"
            ))
}
