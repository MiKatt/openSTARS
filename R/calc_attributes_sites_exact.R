#' Calculate attributes of the sites.
#'
#' For each site (observation or prediction) the total catchment area is
#' calculated ('H2OArea'). Additionally, other attributes (predictor variables)
#' can be derived based on given raster or vector maps. This function calculates
#' exact values for catchments derived with
#' \href{https://grass.osgeo.org/grass74/manuals/addons/r.stream.basins.html}{r.stream.basins}
#' and can take considerable time if there are many sites.
#' Catchment raster maps can optionally be stored as "sitename_catchm_X" (X = locID).

#' @import progress
#'
#' @param sites_map character; name of the sites (observation or prediction) 
#' attributes shall be calculated for. "sites" (default) refers to the observation sites.
#' @param input_raster character vector (optional); name of additional raster
#'   maps to calculate attributes from.
#' @param stat_rast character vector (optional); statistics to be calculated, one of:
#'   min, max, mean, stddev, variance, sum, median, percant, area or percentile_X (where X
#'   gives the desired percentile e.g. 25 for the first). Must be provided if 
#'   \code{input_raster} are given.
#' @param attr_name_rast character vector (optional); column name for the attributes
#'   to be calculated. Attribute names must not be longer than 10 characters.
#'   Must be provided if \code{input_raster} are given.
#' @param input_vector character string vector (optional); name of additional vector
#'   maps to calculate attributes from.
#' @param stat_vect character string vector (optional); statistics to be calculated, 
#'  one of: percentage, area or count. Must be provided if \code{input_vector} is given.
#' @param attr_name_vect character string vector (optional); column name(s) in 
#'  the vector file provided to calculate the attributes from (if \code{input_vector}
#'  is a polygon map and stat_vect is 'percent') or giving the new name attributes
#'  to calculate (if \code{input_vector} is a point map and stat_vect is 'count'.
#'  Must be provided if \code{input_vector} is given.
#' @param round_dig integer; number of digits to round results to. Can be a vector
#'   of different values or just one value for all attributes.
#' @param calc_basin_area boolean; shall the catchment area be calculated? (Useful
#'  to set to FALSE if the function has been called before with \code{keep_basins = TRUE}.)
#' @param keep_basins boolean; shall raster maps of all the watersheds be kept?
#'
#' @return Nothing. The function appends new columns to the \code{sites_map} attribute table
#' \itemize{
#'  \item{'H2OArea':} {Total watershed area of the watershed upstream of each site.}
#'  \item{attr_name_rast:} {Additional optional attributes calculated based on input_raster maps.}
#' }
#' Please note that for sampling points that lie in the same DEM raster cell 
#'  along a stream identical values are calculated because identical watersheds
#'  are derived.
#'
#' @note \code{\link{import_data}}, \code{\link{derive_streams}},
#'   \code{\link{calc_edges}} and \code{\link{calc_sites}} or
#'   \code{\link{calc_prediction_sites}} must be run before.
#'   
#' If \code{calc_basin_area = F} but there are no raster maps called 'sitename_catchm_x' 
#' with x = locID of all sites the catchments (and their area) are derived. 
#'   
#' @author Mira Kattwinkel, \email{mira.kattwinkel@@gmx.net}, 
#'   Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' 
#' @examples
#' \donttest{
#' # Initiate GRASS session
#' if(.Platform$OS.type == "windows"){
#'   gisbase = "c:/Program Files/GRASS GIS 7.4.0"
#' } else {
#'   gisbase = "/usr/lib/grass74/"
#' }
#' initGRASS(gisBase = gisbase,
#'      home = tempdir(),
#'      override = TRUE)
#' 
#' # Load files into GRASS
#' dem_path <- system.file("extdata", "nc", "elev_ned_30m.tif", package = "openSTARS")
#' sites_path <- system.file("extdata", "nc", "sites_nc.shp", package = "openSTARS")
#' setup_grass_environment(dem = dem_path)
#' import_data(dem = dem_path, sites = sites_path)
#' gmeta()
#' 
#' # Derive streams from DEM
#' derive_streams(burn = 0, accum_threshold = 700, condition = TRUE, clean = TRUE)
#' 
#' # Prepare edges
#' calc_edges()
#' execGRASS("r.slope.aspect", flags = c("overwrite","quiet"),
#'           parameters = list(
#'             elevation = "dem",
#'             slope = "slope"
#'           ))
#' calc_attributes_edges(input_raster = "slope", stat_rast = "max", attr_name_rast = "maxSlo")
#' 
#' # Prepare sites
#' calc_sites()
#' calc_attributes_sites_approx(input_attr_name = "maxSlo", output_attr_name = "maxSloA", stat = "max")
#' calc_attributes_sites_exact(input_raster = "slope", attr_name_rast = "maxSloE", stat_rast = "max")
#' 
#' # Plot data
#' dem <- readRAST('dem', ignore.stderr = TRUE)
#' edges <- readVECT('edges', ignore.stderr = TRUE)
#' sites <- readVECT('sites', ignore.stderr = TRUE)
#' plot(dem, col = gray(seq(0,1,length.out=20)))
#' mm <- range(c(edges$maxSlo_e, sites$maxSloA, sites$maxSloE))
#' b <- seq(from = mm[1], to = mm[2] + diff(mm) * 0.01, length.out = 10)
#' c_ramp <- colorRampPalette(c("white", "blue", "orange", "red"))
#' cols <- c_ramp(length(b))[as.numeric(cut(edges$maxSlo_e, breaks = b, right = FALSE))]
#' plot(edges,col = cols, lwd = 2, add = TRUE)
#' cols <- c_ramp(length(b))[as.numeric(cut(sites$maxSloA,breaks = b,right = FALSE))]
#' plot(sites, pch = 19, col = cols, cex = 2, add = TRUE)
#' cols <- c_ramp(length(b))[as.numeric(cut(sites$maxSloE,breaks = b,right = FALSE))]
#' plot(sites, pch = 21, bg = cols, cex = 1.1, add = TRUE)
#' # Some points in the lower centre of the map indicate a difference in max slope between
#' # approximate and exact calculation (different colors for inner and outer points)
#' }

calc_attributes_sites_exact <- function(sites_map = "sites",
                                        input_raster = NULL,
                                        stat_rast = NULL,
                                        attr_name_rast = NULL,
                                        input_vector = NULL,
                                        stat_vect = NULL,
                                        attr_name_vect = NULL,
                                        round_dig = 2,
                                        calc_basin_area = TRUE,
                                        keep_basins = FALSE){
  
  if(length(input_raster) != length(stat_rast) | length(input_raster) != length(attr_name_rast) | length(attr_name_rast) != length(stat_rast))
    stop(paste0("There must be the same number of input raster files (",length(input_raster), "), statistics to calculate (",
                length(stat_rast), ") and attribute names (", length(attr_name_rast),")."))

  if(length(input_vector) != length(stat_vect))
    stop(paste0("There must be the same number of input vector files (",length(input_vector), ")
                and statistics to calculate (", length(stat_vect), ")."))
  
  if(!is.null(stat_rast) & any(stat_rast %in% c("min","max", "mean", "stddev","variance","sum","median", "percent","area") + grepl("percentile", stat_rast)) == 0) # TRUE = 1, FALSE = 0
    stop('Statistisc to calculate from raster data must be one of "min","max", "mean", "stddev","variance","sum", "median", "percentile_X", "percent" or "area".')

  if(any(!stat_vect %in% c("percent", "count", "area")))
    stop('statistics to calculate from vector data must be one of "count", "percent" or "area".')
  
  # 1 for area, 2 for points
  vtype <- rep(1, length(stat_vect))
  if(!is.null(stat_vect)){
    for(i in 1:length(stat_vect)){
      a <- execGRASS("v.info", flags = "t",
                     parameters = list(
                       map = input_vector[i]
                     ), intern = T)
      a <- do.call(rbind,strsplit(a, "="))
      k <- which(a[,1] == "points")
      if(as.numeric(a[k,2]) != 0){
        vtype[i] <- 2
        if(stat_vect[i] != "count"){
          stop('If an input vector is of type point the statistic to calculate must be "count".')
        }
      }
    }
  }
  
  if(is.null(stat_rast) & is.null(stat_vect) & !calc_basin_area)
    stop("Either the catchment areas are calculated or a statistic to calculate must be provided.")

  temp_dir <- tempdir()

  if(length(round_dig) == 1)
    round_dig <- rep(round_dig, length(stat_rast)+1)
  if(length(round_dig) == length(stat_rast))
    round_dig <- c(round_dig[1], round_dig)

  rast <- execGRASS("g.list",
                    parameters = list(
                      type = "rast"
                    ),
                    intern = TRUE)
  if ("MASK" %in% rast)
    execGRASS("r.mask",flags = c("r", "quiet"))
  
  if(!all(input_raster %in% rast)){
    if(length(input_raster)>1){
      i <- which(input_raster %in% rast)
      mes <- input_raster[-i]
    } else {
      mes <- input_raster
    }
    stop(paste0("Missing input raster data ", paste0("'", mes, "'", collapse = ", "), 
                ". Please give valid raster names. \nAvailable raster are ",  
                paste0("'", rast, "'", collapse = ", ")))
  }

  vect <- execGRASS("g.list",
                    parameters = list(
                      type = "vector"
                    ),
                    intern = TRUE)
  
  if(!all(input_vector %in% vect)){
    if(length(input_vector)>1){
      i <- which(input_vector %in% vect)
      mes <- input_vector[-i]
    } else {
      mes <- input_vector
    }
    stop(paste0("Missing input vector data ", paste0("'", mes, "'", collapse = ", "), 
                ". Please give valid vector file names. \nAvailable vector files are ",  
                paste0("'", vect, "'", collapse = ", ")))
  }
  
  d.sites <- readVECT(sites_map, ignore.stderr = TRUE)
  
  if(!all(paste0(sites_map, "_catchm_", d.sites@data$locID) %in% rast)){
    calc_basin_area <- TRUE
  }
  if(any(d.sites@data$ratio == 0) & calc_basin_area){
    d.edges <- readVECT("edges", ignore.stderr = TRUE)
    dt.edges <- setDT(d.edges@data)
    dt.edges[, colnames(dt.edges)[-which(colnames(dt.edges) %in% c("cat", "stream","prev_str01","prev_str02","rid","H2OArea"))] := NULL]
    rm(d.edges)
  }

  locID <- unique(d.sites@data$locID)
  dat.h2o <- dat.rast <- dat.vect <- NULL
  if(calc_basin_area){
    dat.h2o <- matrix(nrow = length(locID), ncol = 1, data = 0)
    colnames(dat.h2o) <- c("H2OArea")
  } 
  if(!is.null(input_raster)){
    vals.rast <- lapply(1:length(input_raster), function(x) ifelse(stat_rast[x] == "percent", list(get_all_raster_values(input_raster[x])), 1))
    ndat <- length(unlist(vals.rast))
    dat.rast <- matrix(nrow = length(locID), ncol = ndat, data = 0)
    cnames.rast <- lapply(1:length(attr_name_rast), function(x) ifelse(stat_rast[x] %in% c("percent", "area"),
                                                                       list(paste(attr_name_rast[x], unlist(vals.rast[[x]]), sep = "_")),
                                                                       attr_name_rast[x]))
    colnames(dat.rast) <- unlist(cnames.rast)
  }
  if(!is.null(input_vector)){
    attribute_cats <- NULL
    for(i in 1:length(input_vector)){
      if(vtype[i] == 1){
        cnames <- execGRASS("db.columns", flags = "quiet",
                            parameters = list(
                              table = input_vector[i]
                            ), intern = T)
        if(!attr_name_vect[i] %in% cnames)
          stop(paste0("Invalid vector attribute name ", paste0("'", attr_name_vect[i], "'", collapse = ", "), 
                      ". Please give a valid column name. \nAvailable columns are ",  
                      paste0("'", cnames, "'", collapse = ", ")))
        attribute_cats <- c(attribute_cats, 
                           unique(execGRASS("db.select", flags = c("c"),
                                            parameters = list(
                                              sql = paste0("select ", attr_name_vect[i], " from ",input_vector[i])
                                            ), intern = T))
        )
      } else {
        attribute_cats <- c(attribute_cats, attr_name_vect[i])
      }
      if(length(round_dig) < length(attribute_cats) + length(input_raster) + calc_basin_area)
        round_dig <- c(round_dig, rep(round_dig[1], length(attribute_cats)))
    }
    dat.vect <- matrix(ncol = length(attribute_cats), nrow = length(locID), data = 0)
    colnames(dat.vect) <- c(attribute_cats)
  }
  
  cellsize <- NULL
  if(any(stat_rast == "area")){
    cellsize <- execGRASS("g.region", flags = "p",intern=T)
    cellsize <- as.numeric(do.call(rbind,strsplit(cellsize[grep("res",cellsize)],split=":"))[,2])
    cellsize <- prod(cellsize)
  }
  
  cnames_sites <- execGRASS("db.columns", flags = "quiet",
                             parameters = list(
                               table = "sites"
                             ), intern = T)
  
  message("Intersecting attributes for ", nrow(d.sites@data)," sites ...")
  # progress bar
  pb <- progress::progress_bar$new(total = nrow(d.sites@data))
  
  for (i in seq_along(locID)) {
    #message(i)
    id <- locID[i]
    #dat[i,"locID"]  <-  locID
    # get first entry from d.sites@data with locID
    ii <- which(d.sites@data$locID == id)[1] 

    # calculate drainage area in km^2
    if(calc_basin_area){
      # If the distance ratio is 0, the site lies within the outflow cell of
      # the edge; then, r.stream.basins will extract a too large basin including
      # the second tributary of the confluence
      if(d.sites@data$ratio[ii] == 0){
        j <- which(dt.edges[, "rid"] == d.sites@data$rid[ii])
        # use $ here to get single value from data.table to match dimensions of dat
        dat.h2o[i,"H2OArea"] <- round(dt.edges$H2OArea[j], round_dig[1])
        streams <- get_streams_edges_in_catchment(dt.edges, dt.edges$stream[j])
        ## new, simpler approach
        m <- paste0("if(", paste0("rca == ", streams, collapse =  " || "), ", 1, 0)")
        execGRASS("r.mapcalc", flags = c("overwrite","quiet"),
                  parameters = list(
                    expression = paste0(sites_map, "_catchm_", id,  " = ", m)
                  ))
        
        ## old approach
        # ## more than 106 categories in r.mask crashes (i.e. no MASK is created)
        # ## workaround:
        # n <- length(cats) %/% 100
        # if(n == 0){
        #   execGRASS("r.mask", flags = c("overwrite","quiet"),
        #             parameters = list(
        #               raster = "rca",
        #               maskcats = paste0(cats, collapse = " "))
        #   )
        #   execGRASS("r.mapcalc", flags = c("overwrite","quiet"),
        #             parameters = list(
        #               expression = paste0("'", paste0(sites_map, "_catchm_",locID), "=MASK'")
        #             ))
        #   execGRASS("r.mask", flags = c("r","quiet")
        #   )
        # } else {
        #   count <- 0
        #   for(j in 1:n){
        #     execGRASS("r.mask", flags = c("overwrite","quiet"),
        #               parameters = list(
        #                 raster = "rca",
        #                 maskcats = paste0(cats[((j-1)*100+1):(j*100)], collapse = " "))
        #     )
        #     execGRASS("r.mapcalc", flags = c("overwrite","quiet"),
        #               parameters = list(
        #                 expression = paste0("'temp", j, "=MASK'")
        #               ))
        #     execGRASS("r.mask", flags = c("r","quiet")
        #     )
        #     count <- count + 1
        #   }
        #   if(length(cats) %% 100 != 0){
        #     execGRASS("r.mask", flags = c("overwrite","quiet"),
        #               parameters = list(
        #                 raster = "rca",
        #                 maskcats = paste0(cats[(n*100+1):length(cats)], collapse = " "))
        #     )
        #     execGRASS("r.mapcalc", flags = c("overwrite","quiet"),
        #               parameters = list(
        #                 expression = paste0("'temp", n+1, "=MASK'")
        #               ))
        #     execGRASS("r.mask", flags = c("r","quiet")
        #     )
        #     count <- count + 1
        #   }
        #   m <- paste0("temp",1:count)
        #   m <- paste0("if(", paste0(m, collapse = "|||"),",1)")
        #   execGRASS("r.mapcalc", flags = c("overwrite","quiet"),
        #             parameters = list(
        #               expression = paste0('\"', paste0(sites_map, '_catchm_',locID), '=', m,'\"')
        #             ))
        #   execGRASS("g.remove", flags = c("f","quiet"),
        #             parameters = list(
        #               type = "raster",
        #               name = paste0("temp",1:count)
        #             ))
        #}
      } else {
        coord <- coordinates(d.sites[ii,])
        execGRASS("r.stream.basins",
                  flags = c("overwrite", "l", "quiet"),
                  parameters = list(direction = "dirs",
                                    coordinates = coord,
                                    basins = paste0(sites_map, "_catchm_",id)))
        dat.h2o[i,"H2OArea"] <- round(as.numeric(as.character(strsplit(
          execGRASS("r.stats",
                    flags = c("a", "quiet"),
                    parameters = list(input = paste0(sites_map, "_catchm_",id)),
                    intern = TRUE)[1], split = ' ')[[1]][[2]]))/1000000,round_dig[1])
      }
    }
    
    ##########################
    
    # raster data
    # calculate unviriate statistics per watershed
    if(length(stat_rast) > 0){
      # set mask to the current basin
      execGRASS("r.mask",
                flags = c("overwrite", "quiet"),
                parameters = list(
                  raster = paste0(sites_map, "_catchm_",id)))
      st <- execGRASS("r.univar",
                      flags = c("overwrite", "quiet", "t"),
                      parameters = list(
                        map = "MASK",
                        separator = ","),
                      intern = TRUE)
      st <- data.frame(do.call(rbind,strsplit(st,",")), stringsAsFactors = FALSE)
      non_null_cells <- as.numeric(st[2,which(st[1,] == "non_null_cells")])
      for(j in 1:length(stat_rast)){
        if(stat_rast[j] == "percent" | stat_rast[j] == "area"){
          st <- execGRASS("r.stats", flags = c("c"),
                          parameters = list(
                            input =   input_raster[j], #paste0("rca,", input_raster[j]),
                            separator = "comma"
                          ), intern = TRUE)
          st <- data.frame(do.call(rbind,strsplit(st,",")), stringsAsFactors = FALSE)
          colnames(st) <- c("class", "cellcount")
          st$cellcount <- as.numeric(st$cellcount)
          #suppressWarnings(st$zone <- as.numeric(st$zone))
          setDT(st)
          #st[, `:=`(names(st), lapply(.SD, as.numeric))]
          #st <- dcast(st, "zone  ~ class", value.var = "cellcount")
          #st[, all_cells := rowSums(.SD, na.rm = TRUE), .SDcols = 2:ncol(st)]
          #st[, "*" := NULL]
          #st[, 2:(ncol(st)-1) := lapply(.SD, function(x) x / st$all_cells), .SDcols = 2:(ncol(st)-1)]
          #st[, all_cells := NULL]
          #colnames(st)[-1] <- paste(attr_name_rast[j], colnames(st)[-1], sep = "_")
          #rca_cell_count <- merge(rca_cell_count, st, by = "zone", all.x = TRUE)
          #stat_r <- c(stat_r, rep("percent_class", ncol(st) - 1))
          if(length(ind <- which(st$class == "*")) > 0)
            st <- st[-ind,, drop = FALSE]
          if(nrow(st) > 0){
            if(stat_rast[j] == "percent"){
              dat.rast[i, paste(attr_name_rast[j], st$class, sep = "_")] <- st$cellcount / non_null_cells
            } else { #"area
              dat.rast[i, paste(attr_name_rast[j], st$class, sep = "_")] <- st$cellcount * cellsize
            }
          }
        } else {
          if(stat_rast[j] == "median"){
            st <- execGRASS("r.univar",
                            flags = c("overwrite", "quiet","g", "e"),
                            parameters = list(
                              map = input_raster[j]), intern = TRUE)
          } else if(grepl("percentile",stat_rast[j])){
            p <- as.numeric(as.character(unlist(strsplit(stat_rast[j],"_"))[2]))
            st <- execGRASS("r.univar",
                            flags = c("overwrite", "quiet","g", "e"),
                            parameters = list(
                              map = input_raster[j],
                              percentile = p), intern = TRUE)
          } else {
            st <- execGRASS("r.univar",
                            flags = c("overwrite", "quiet","g"),
                            parameters = list(
                              map = input_raster[j]), intern = TRUE)
          } 
          st <- setDT(data.frame(do.call(rbind,strsplit(st,"="))))
          if(nrow(st) > 0){
            st[,X2 := as.numeric(as.character(X2))]
            if(grepl("percent", stat_rast[j])){
              if(st[X1 == "variance", X2] == 0){  # if coded as something and NA, null(), no data valu
                dat.rast[i, cnames.rast[[j]]] <- round(st[X1 == "n", X2] /non_null_cells, round_dig[j + 1])
              } else{  # if coded as 1 and 0, "mean" gives ratio
                dat.rast[i, cnames.rast[[j]]] <- round(st[X1 == "mean", X2], round_dig[j + 1])
              }
            } else
              dat.rast[i, cnames.rast[[j]]] <- round(st[X1 == stat_rast[j], X2], round_dig[j + 1])
          } else
            dat.rast[i, cnames.rast[[j]]] <- 0
        }
      }
      # Remove the mask!
      execGRASS("r.mask",
                flags = c("r", "quiet"))
    }
    
    ###################################################
    # vector data
    if(length(input_vector) > 0){
      # convert raster catchment to vector
      rname <- paste0(sites_map, "_catchm_",id)
      vname <- paste0(sites_map, "_catchm_",id, "_v")
      execGRASS("r.to.vect", flags = c("overwrite","v", "quiet"),
                parameters = list(
                  input = rname,
                  output = vname,
                  type = "area"
                ))
      # calculate area
      execGRASS("v.db.addcolumn",
                parameters = list(
                  map = vname,
                  columns = "area double precision"
                ))
      execGRASS("v.to.db",flags = "quiet",
                parameters = list(
                  map = vname,
                  option = "area",
                  columns = "area"
                ))
      carea <- sum(as.numeric(execGRASS("v.db.select",flags = "quiet",
                                        parameters = list(
                                          map = vname,
                                          columns = "area"
                                        ), intern = T)[-1]))
      j.count <- 1 + calc_basin_area * length(input_raster)
      for(j in 1:length(input_vector)){
        # if this is no point vector
        if(vtype[j] == 1){
          # intersect with catchment
          execGRASS("v.overlay", flags = c("overwrite","quiet"),
                    parameters = list(
                      ainput = input_vector[j],
                      binput = vname,
                      operator = "and",
                      output = "intersect_out",
                      olayer = "1,0,0"
                    ), ignore.stderr = T, intern = T)
          # calculate area of all features
          execGRASS("v.db.addcolumn",
                    parameters = list(
                      map = "intersect_out",
                      columns = "area double precision"
                    ))
          execGRASS("v.to.db",flags = "quiet",
                    parameters = list(
                      map = "intersect_out",
                      option = "area",
                      columns = "area"
                    ))
          # get the areas per value of the attribute
          a <- execGRASS("db.select",flags = "c",
                         parameters = list(
                           sql = paste0("select a_", attr_name_vect[j],",sum(area) from intersect_out group by a_", attr_name_vect[j])
                         ), intern = T)
          if(length(a) > 0){
            a <- do.call(rbind,strsplit(a,split = '\\|'))
            a <- data.frame(a,  stringsAsFactors = F)
            if(stat_v[j] == "percent")  # do not divide by area if calculating total area 
              a[,2] <- round(as.numeric(a[,2]) / carea, round_dig[j.count])
            dat.vect[i, a[,1]] <- a[,2]
          }
        } else { # if this is a point vector
          dat.vect[i, attr_name_vect[j]] <- as.numeric(unlist(strsplit(
            execGRASS("v.vect.stats", flags = c("p", "quiet"),
                    parameters = list(
                      points = input_vector[j],
                      areas = vname,
                      separator = ","
                    ), intern = T)[2], split = ","))[2])
          
        }
        j.count <- j.count + 1
      }
    }
    ###################################################

    # Delete watershed raster
    if (!keep_basins) {
      rast <- execGRASS("g.list",
                        parameters = list(
                          type = "rast"
                        ),
                        intern = TRUE)
      if(paste0(sites_map, "_catchm_",id) %in% rast){
        execGRASS("g.remove",
                  flags = c("quiet", "f"),
                  parameters = list(
                    type = "raster",
                    name = paste0(sites_map, "_catchm_",id)
                  ))
      }
      vect <- execGRASS("g.list",
                        parameters = list(
                          type = "vector"
                        ),
                        intern = TRUE)
      if(paste0(sites_map, "_catchm_",id, "_v") %in% vect){
      execGRASS("g.remove",
                flags = c("quiet", "f"),
                parameters = list(
                  type = "vector",
                  name = paste0(sites_map, "_catchm_",id, "_v")
                ), ignore.stderr = TRUE)
      }
    }
    pb$tick()
  }
  
  # change column names if there is just one raster class (and NULL)
  cnames.rast <- lapply(1:length(attr_name_rast), function(x) ifelse(stat_rast[x] %in% c("percent", "area") & length(unlist(vals.rast[[x]])) > 1, 
                                                                     list(paste(attr_name_rast[x], unlist(vals.rast[[x]]), sep = "_")),
                                                                     attr_name_rast[x]))
  colnames(dat.rast) <- unlist(cnames.rast)
  
  dat <- cbind(dat.h2o, dat.rast, dat.vect, locID)
  
  # Join attributes to sites attribute table
  message("Joining new attributes to attribute table ...")
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
  execGRASS("db.droptable", flags = c("quiet","f"),
            parameters = list(
              table = "sites_attributes_exact"
            ))
  invisible(file.remove(file.path(temp_dir,"sites_attributes_exact.csv")))
  invisible(file.remove(file.path(temp_dir,"sites_attributes_exact.csvt")))
  
  cnames_sites2 <- execGRASS("db.columns", flags = "quiet",
                             parameters = list(
                               table = sites_map
                             ), intern = T)
  cnames_sites2 <- cnames_sites2[-(which(cnames_sites2 %in% cnames_sites))]
  message(paste0("\nNew attributes values are stored as ", paste("'", cnames_sites2, "'", sep = "", collapse = ", "), " in 'sites'."))
}
