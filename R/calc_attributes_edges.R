#' Calculate attributes of the edges.
#'
#' For each edge (i.e. stream segment) additional attributes (potential predictor 
#' variables) are derived based on given raster or vector maps.
#'
#' @param input_raster name(s) of raster map(s) to calculate attributes from.
#' @param stat_rast name(s) giving the statistics to be calculated,
#'   from the raster maps, must be one of: "min", "max", "mean", "sum", "percent", "area"
#'   for each \code{input_raster}.
#' @param attr_name_rast of new column name(s) for the attribute(s)
#'   to be calculated. Attribute names must not be longer than 8 characters as ESRI shapefiles
#'   cannot have colum names with more than 10 characters. See notes.
#' @param input_vector name(s) of vector map(s) to calculate attributes from.
#' @param stat_vect name(s) giving the statistics to be calculated
#'   from the vector maps, must be one of: "count" (for point data), "percent" or "area"
#'   (for polygon data) for each \code{input_vector}.
#' @param attr_name_vect name(s) of attribute column(s), case sensitive. For polygon data, this is
#'   the column to calculate the statistics from; the results column names are 
#'   created by the content of this column. For point data, a column will be created
#'   with this name to hold the counts. See notes.
#' @param round_dig integer; number of digits to round results to. Can be a vector
#'   of different values or just one value for all attributes.
#' #@param clean logical; should intermediate files be deleted

#' @return Nothing. The function appends new columns to the 'edges' attribute
#'   table with column names given in \code{attr_name_rast} and derived from the attribute classes for 
#'   vector data. For each attribute, two columns are appended: one giving the attribute for the rca of the edge
#'   ("attribute_name_e") and one for the attribute of the total catchment of
#'   the edge ("attribute_name_c").
#'
#'@note Column names for the results are created as follows:
#' Raster data - the column names given in \code{attr_name_rast} are used. The user should
#' take care to use unique, clear names. For \code{stat_rast} = 'percentage' or 'area', 
#' the output column name will be concatenated 'p' or 'a', repectively.
#' For vector data, column names are constructed from the entries in in the column 
#' \code{attr_name_vect}. For counts of points, the new column name containing the counts
#' is just the given name. For polygon data ('percentage' or 'area'), the names are constructed using
#' the unique entries of the column with a concatenated 'p' or 'a', repectively. If, for instance, 
#' for a landuse vector containing the classes 'urban' and 'arable' percentages would be calculated,
#' edges would contain two new columns 'urbanp' and 'arablep'.
#'
#'@details First, the reach contributing areas (= subcatchments) for all edges are calculated. 
#' Then these are intersected with the given raster and/or vector maps and the desired 
#' statistics are computed. 
#' This function must be run before computing approximate attribute values for 
#' sites \code{\link{calc_attributes_sites_approx}}.
#'
#'For \code{stat_rast} = "percent" or "area" the \code{input_raster} can be either coded as 1 and 0
#'  (e.g., cells occupied by the land use under consideration and not) or as different classes. 
#'  The percentage or area of each class in the catchment is calculated. If
#'  the \code{input_raster} consists of percentages per cell (e.g., proportional land
#'  use of a certain type per cell) \code{stat_rast} = "mean" gives the overall proportion
#'  of this land use in the catchment.
#'
#' For \code{stat_vect} = "percent" or "area" \code{input_vector} must contain polygons of
#' e.g. different land use types. The column \code{attr_name_vect} would then 
#' give the code for the different land uses. Then, the percentage for each land
#' use type in the catchment of the edge is calculated and given in separate columns
#' with column names resampling the different categories given in column 
#' \code{attr_name_vect}
#' 
#' For \code{stat_vect} = "count" \code{input_vector} must contain points of
#' e.g. waste water treatment plants. The column \code{attr_name_vect} gives the 
#' name of the column to hold the count value, e.g. nWWTP. 
#' 
#' Both raster and vector maps to be used must be read in to the GRASS session, 
#' either in \code{\link{import_data}} or using the GRASS function r.in.rast or
#' v.in.ogr (see examples).
#'
#'
#' @note \code{\link{setup_grass_environment}}, \code{\link{import_data}},
#' \code{\link{derive_streams}} and \code{\link{calc_edges}} must be run before.
#'
#' @author Mira Kattwinkel, \email{mira.kattwinkel@@gmx.net}
#' @export
#' 
#' @examples
#' \donttest{
#' if(.Platform$OS.type == "windows"){
#'   gisbase = "c:/Program Files/GRASS GIS 7.6"
#'   } else {
#'   gisbase = "/usr/lib/grass74/"
#'   }
#' initGRASS(gisBase = gisbase,
#'     home = tempdir(),
#'     override = TRUE)
#' 
#' # Load files into GRASS
#' dem_path <- system.file("extdata", "nc", "elev_ned_30m.tif", package = "openSTARS")
#' sites_path <- system.file("extdata", "nc", "sites_nc.shp", package = "openSTARS")
#' streams_path <- system.file("extdata", "nc", "streams.shp", package = "openSTARS")
#' preds_v_path <- system.file("extdata", "nc", "pointsources.shp", package = "openSTARS")
#' preds_r_path <- system.file("extdata", "nc", "landuse_r.tif", package = "openSTARS")
#'                  
#' 
#' setup_grass_environment(dem = dem_path)
#' import_data(dem = dem_path, sites = sites_path, streams = streams_path,
#'             predictor_vector = preds_v_path, predictor_raster = preds_r_path)
#' gmeta()
#' 
#' # Derive streams from DEM
#' # burn in 'streams' 10 meters
#' derive_streams(burn = 10, accum_threshold = 700, condition = TRUE, clean = TRUE)
#' 
#' # Check and correct complex confluences (there are no complex confluences in this
#' # example date set; set accum_threshold in derive_streams to a smaller value
#' # to create complex confluences)
#' cj <- check_compl_confluences()
#' if(cj){
#'   correct_compl_confluences()
#' }
#' 
#' # calculate slope as potential predictor
#' execGRASS("r.slope.aspect", flags = c("overwrite","quiet"),
#' parameters = list(
#'   elevation = "dem",
#'     slope = "slope"
#'     ))
#' 
#' 
#' # Prepare edges
#' calc_edges()
#' calc_attributes_edges(input_raster = c("slope", "landuse_r"), 
#'                       stat_rast = c("max", "percent"), 
#'                       attr_name_rast = c("maxSlo", "luse"),
#'                       input_vector = "pointsources", stat_vect = "count",
#'                       attr_name_vect = "psource")
#'                       
#' # Plot eges with percentage of forest in the catchment (luse_5) as line width
#' edges <- readVECT('edges', ignore.stderr = TRUE)
#' head(edges@data)
#' lu <- readRAST("landuse_r", ignore.stderr = TRUE)
#' 
#'  # plot landuse data
#' library(raster)
#' op <- par()
#' par(xpd = FALSE)
#' plot(raster(lu), legend = FALSE, xaxt = "n", yaxt = "n", bty = "n",
#' col = adjustcolor(c("red", "goldenrod", "green", "forestgreen",
#' "darkgreen", "blue", "lightblue"), alpha.f = 0.7))
#' par(xpd = TRUE)
#' legend("bottom", cex = 0.75,
#'   legend = c("developed", "agriculture", "herbaceous", "shrubland", "forest", "water", "sediment"),
#'   fill = c("red", "goldenrod", "green", "forestgreen","darkgreen", "blue", "lightblue"),
#'   horiz = TRUE, inset = -0.175)
#' plot(edges, lwd = edges$luse_5_c * 10, add = TRUE)    
#' par <- op
#' }

calc_attributes_edges <- function(input_raster = NULL, stat_rast = NULL, attr_name_rast = NULL,
                                  input_vector = NULL, stat_vect = NULL, attr_name_vect = NULL,
                                  round_dig = 2){
  
  if(length(input_raster) != length(stat_rast) | length(input_raster) != length(attr_name_rast) | length(attr_name_rast) != length(stat_rast))
    stop(paste0("There must be the same number of input raster files (",length(input_raster), "), statistics to calculate (",
                length(stat_rast), ") and attribute names (", length(attr_name_rast),")."))
  
  if(any(!stat_rast %in% c("min","max", "mean", "sum", "percent", "area")))
    stop('statistics to calculate must be one of "min","max", "mean", "sum", "percent" or "area".')
  
  if(any(sapply(attr_name_rast, nchar) > 8))
    stop("Attribute names must not be longer than eight characters.")
  
  if(length(input_vector) != length(stat_vect) | length(input_vector) != length(attr_name_vect) | length(attr_name_vect) != length(stat_vect))
    stop(paste0("There must be the same number of input raster files (",length(input_vector), "), statistics to calculate (",
                length(stat_vect), ") and attribute names (", length(attr_name_vect),")."))
  
  if(any(!stat_vect %in% c("percent", "count", "area")))
    stop('statistics to calculate must be one of "count", "percent" or "area".')
  
  i <- which(stat_vect == "count")
  if(length(i) > 0){
    for(j in i){
      a <- execGRASS("v.info", flags = "t",
                     parameters = list(
                       map = input_vector[j]
                     ), ignore.stderr = TRUE, intern = TRUE)
      a <- do.call(rbind,strsplit(a, "="))
      k <- which(a[,1] == "points")
      if(as.numeric(a[k,2]) == 0)
        stop('If statistic to calculate is "count" the respective input vector must be of type point.')
    }
  }
  
  # 1 for area, 2 for points
  vtype <- rep(1, length(stat_vect))
  if(!is.null(stat_vect)){
    for(i in 1:length(stat_vect)){
      a <- execGRASS("v.info", flags = "t",
                     parameters = list(
                       map = input_vector[i]
                     ), ignore.stderr = TRUE, intern = TRUE)
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
  
  # if(any(sapply(attr_name_vect, nchar) > 8))
  #   stop("Attribute names must not be longer than eight characters.")

  if(is.null(input_raster) & is.null(input_vector))
    stop("At least one raster or vector file name must be given for intersection.")
  
  cnames_edges <- execGRASS("db.columns", flags = "quiet",
                            parameters = list(
                              table = "edges"
                            ), ignore.stderr = TRUE, intern = TRUE)
  
  rast <- execGRASS("g.list",
                    parameters = list(
                      type = "rast"
                    ),
                    intern = TRUE)
  if (!"rca" %in% rast)
    stop("Missing data. Did you run calc_edges()?")
  
  if ("MASK" %in% rast)
    execGRASS("r.mask",flags = c("r", "quiet"))
  
  if(!all(input_raster %in% rast)){
    if(length(input_raster)>1){
      i <- which(input_raster %in% rast)
      mes <- input_raster[-i]
    } else {
      mes <- input_raster
    }
    stop(paste0("Missing input raster data ", paste0("'",mes,"'", collapse = ", "), 
                ". Please give valid raster names. \nAvailable raster are ",  
                paste0("'",rast,"'", collapse = ", ")))
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
    stop(paste0("Missing input vector data ", paste0("'",mes,"'", collapse = ", "), 
                ". Please give valid vector file names. \nAvailable vector files are ",  
                paste0("'",vect,"'", collapse = ", ")))
  }
  
  if(length(round_dig) == 1)
    round_dig <- rep(round_dig, length(stat_rast) + length(stat_vect))
  
  cellsize <- NULL
  if(any(stat_rast == "area")){
    cellsize <- execGRASS("g.region", flags = "p", ignore.stderr = TRUE, intern=T)
    cellsize <- as.numeric(do.call(rbind,strsplit(cellsize[grep("res",cellsize)],split=":"))[,2])
    cellsize <- prod(cellsize)
  }
  
  temp_dir <- tempdir()

  if(!is.null(input_raster)){
    message("Intersecting raster maps ...")
    rca_cell_count <- execGRASS("r.univar",
                                flags = c("overwrite", "quiet","t"),
                                parameters = list(
                                  map = "rca",
                                  zones = "rca",
                                  separator = "comma"),
                                ignore.stderr = TRUE,
                                intern = TRUE)
    rca_cell_count <- do.call(rbind,strsplit(rca_cell_count, ","))
    colnames(rca_cell_count) <- rca_cell_count[1,]
    rca_cell_count <- rca_cell_count[-1, c("zone","non_null_cells")]
    rca_cell_count <- data.frame(apply(rca_cell_count,2,as.numeric))
    setDT(rca_cell_count)
    
    stat_r <- NULL
    
    for(j in 1:length(stat_rast)){
      # TODO: check what happens with 0/1 or NA/1 coded
      if(stat_rast[j] == "percent" | stat_rast[j] == "area"){#} & get_n_val_raster(input_raster[j]) > 2){
        # MiKatt 20190417: ignore.stderr = T prevents output of "100%" at the end on Windows!
        # solves problem with strange column names
        st <- execGRASS("r.stats", flags = c("c"),
                        parameters = list(
                          input =  paste0("rca,", input_raster[j]),
                          separator = "comma"
                        ), ignore.stderr = TRUE, intern = TRUE)
        st <- data.frame(do.call(rbind,strsplit(st,",")), stringsAsFactors = FALSE)
        colnames(st) <- c("zone", "class", "cellcount")
        st$cellcount <- as.numeric(st$cellcount)
        suppressWarnings(st$zone <- as.numeric(st$zone))
        setDT(st)
        
        # # MiKatt 20190416: to catch strange %-row on Windows
        # i <- which(!sapply(st, is.numeric))
        # if(length(i) > 0){
        #   k <- unlist(sapply(i, function(x) which(grepl("%", unlist(st[,..x])))))
        #   k <- k[is.numeric(k)]
        #   if(length(k) > 0 & ! is.na(k))
        #     st <- st[-k,]
        # }
        
        #st[, `:=`(names(st), lapply(.SD, as.numeric))]
        st <- dcast(st, "zone  ~ class", value.var = "cellcount")
        if(stat_rast[j] == "percent"){
          st[, all_cells := rowSums(.SD, na.rm = TRUE), .SDcols = 2:ncol(st)]
          st[, "*" := NULL]
          st[, 2:(ncol(st)-1) := lapply(.SD, function(x) x / st$all_cells), .SDcols = 2:(ncol(st)-1)]
          st[, all_cells := NULL]
          stat_r <- c(stat_r, rep("percent_class", ncol(st) - 1))
        } else {
          st[, "*" := NULL]
          st[,  2:ncol(st) := lapply(.SD, function(x) x * cellsize), .SDcol  =  2:ncol(st)]
          stat_r <- c(stat_r, rep("area_class", ncol(st) - 1))
        }
        if(length(colnames(st)) > 2){
          colnames(st)[-1] <- paste(paste0(attr_name_rast[j], substr(stat_rast[j],1,1)), colnames(st)[-1], sep = "_")
        } else{
          colnames(st)[-1] <- paste0(attr_name_rast[j], substr(stat_rast[j],1,1))
        }
        rca_cell_count <- merge(rca_cell_count, st, by = "zone", all.x = TRUE)
      } else {
        st <- execGRASS("r.univar",
                        flags = c("overwrite", "quiet","t"),
                        parameters = list(
                          map = input_raster[j],
                          zones = "rca",
                          separator = "comma"),
                        ignore.stderr = TRUE,
                        intern = TRUE)
        st <- do.call(rbind,strsplit(st,","))
        colnames(st) <- st[1,]
        st <- st[-1,, drop = FALSE]
        st <- data.frame(apply(st,2,as.numeric))
        setDT(st)
        if(nrow(st) > 0){
          if(stat_rast[j] %in% names(st)){
            st <- st[, c("zone", stat_rast[j]), with = FALSE]
          } else if(any(st[,"variance", with = F] != 0)) { # if(stat_rast == "percent") and coded as 0 and 1, mean gives the ratio
            st <- st[, c("zone", "mean"), with = FALSE]  
          } else{  # if coded as something and NA, null(), no data value
            st[, "all_cells" := sum(.SD), .SDcols = c("non_null_cells", "null_cells"), by = "zone"]
            st <- data.table(data.frame(st[,"zone"],st[, "non_null_cells"] /st[,"all_cells"]))
          }
          names(st)[2] <- attr_name_rast[j]
          st[, attr_name_rast[j] := round(st[, attr_name_rast[j], with = FALSE], round_dig[j])]
          rca_cell_count <- merge(rca_cell_count, st, by = "zone", all.x = TRUE)
        } else{
          rca_cell_count[,attr_name_rast[j] := 0]
        }
        stat_r <- c(stat_r, stat_rast[j])
      }
    }
    attr_name_rast <- colnames(rca_cell_count)[-c(1:2)]
    stat_rast <- stat_r
    
    dt.streams <- do.call(rbind,strsplit(
      execGRASS("db.select",
                parameters = list(
                  sql = "select stream, next_str, prev_str01, prev_str02, netID from edges"
                ), ignore.stderr = TRUE, intern = TRUE),
      split = '\\|'))
    colnames(dt.streams) <- dt.streams[1,]
    dt.streams <- data.table(dt.streams[-1,,drop = FALSE])
    dt.streams[, names(dt.streams) := lapply(.SD, as.numeric)]
    dt.streams <- merge(dt.streams, rca_cell_count, by.x = "stream", by.y = "zone", all.x = TRUE)
    dt.streams[, cumsum_cells := 0]
    ind <- which(!stat_rast %in% c("min", "max"))
    if(length(ind) > 0){
      for(i in ind)
        dt.streams[which(is.na(dt.streams[, attr_name_rast[i], with = FALSE])), attr_name_rast[i] := 0]
    }
    dt.streams[is.na(non_null_cells), non_null_cells := 0]
    
    # This can take long
    # Calculate attributes for total catchments
    calc_catchment_attributes_rast(dt.streams, stat_rast, attr_name_rast, round_dig)
    
    # Delete unneeded columns
    dt.streams[, c("next_str", "prev_str01", "prev_str02", "netID", "non_null_cells","cumsum_cells") := NULL]
    setnames(dt.streams, attr_name_rast, paste0(attr_name_rast, "_e"))
    
    # Join attributes to edges attribute table
    message("Joining table raster attributes ...")
    utils::write.csv(dt.streams, file.path(temp_dir,"edge_attributes.csv"),row.names = F)
    write.table(t(gsub("numeric","Real",sapply(dt.streams,class))),file.path(temp_dir,"edge_attributes.csvt"),quote=T,sep=",",row.names = F,col.names = F)
    execGRASS("db.in.ogr", flags = c("overwrite","quiet"),
              parameters = list(
                input = file.path(temp_dir,"edge_attributes.csv"),
                output = "edge_attributes"
              ),ignore.stderr = T)
    execGRASS("v.db.join", flags = "quiet",
              parameters = list(
                map = "edges",
                column = "stream",
                other_table = "edge_attributes",
                other_column = "stream"
              ))
  }
  
  if(!is.null(input_vector)){
    message("Intersecting vector maps ...")
    
    # convert raster rca to vector
    execGRASS("r.to.vect", flags = c("overwrite", "quiet"),
              parameters = list(
                input = "rca",
                output = "rca_v",
                type = "area",
                column = "stream"
              ), ignore.stderr = TRUE, intern = TRUE)
    
    dt.streams <- do.call(rbind,strsplit(
      execGRASS("db.select",
                parameters = list(
                  sql = "select stream, next_str, prev_str01, prev_str02, netID, rcaArea, H2OArea from edges"
                ), ignore.stderr = TRUE, intern = TRUE),
      split = '\\|'))
    colnames(dt.streams) <- dt.streams[1,]
    dt.streams <- data.table(dt.streams[-1,,drop = FALSE])
    dt.streams[, names(dt.streams) := lapply(.SD, as.numeric)]
    
    anames <- NULL
    nanames <- vector(length = length(stat_vect))
    
    for(j in 1:length(stat_vect)){
      if(vtype[j] == 1){ # if polygon data
        
        execGRASS("v.overlay", flags = c("overwrite", "quiet"),
                  parameters = list(
                    ainput = "rca_v",
                    atype = "area",
                    binput = input_vector[j],
                    btype = "area",
                    operator = "and",
                    output = "temp_inters"
                  ), ignore.stderr = T)
        execGRASS("v.db.addcolumn", flags = "quiet",
                  parameters = list(
                    map =  "temp_inters",
                    columns = "area double"
                  ), ignore.stderr = TRUE)
        execGRASS("v.to.db", flags = c("quiet"),
                  parameters = list(
                    map =  "temp_inters",
                    option = "area",
                    columns = "area"
                  ), ignore.stderr = TRUE)
        cname <- paste0("b_", attr_name_vect[j])
        dt.dat <- do.call(rbind,strsplit(
          execGRASS("db.select",
                    parameters = list(
                      sql = paste0('select a_stream, area, ', cname, ' from temp_inters')
                    ), ignore.stderr = TRUE, intern = TRUE),
          split = '\\|'))
        colnames(dt.dat) <- dt.dat[1,]
        dt.dat <- as.data.frame(dt.dat[-1,], stringsAsFactors = FALSE)
        setDT(dt.dat)
        setnames(dt.dat, "a_stream", "stream")
        dt.dat[, c("stream", "area") := lapply(.SD, as.numeric), .SDcols =  c("stream", "area")]
        dt.dat <- dt.dat[, .(area = sum(area)), c("stream", cname)] 
        dt.dat <- dcast(dt.dat, paste0("stream  ~ b_", attr_name_vect[j]), value.var = "area")
        #setnames(dt.dat, names(dt.dat)[-1], paste0("p", names(dt.dat)[-1]))
      } else { # if point data
        execGRASS("v.vect.stats", flags = "quiet", 
                  parameters = list(
                    points = input_vector[j],
                    areas = "rca_v",
                    count_column = attr_name_vect[j]
                  ))
        dt.dat <- do.call(rbind,strsplit(
          execGRASS("db.select",
                    parameters = list(
                      sql = paste0('select stream, ', attr_name_vect[j], ' from rca_v')
                    ), ignore.stderr = TRUE, intern = TRUE),
          split = '\\|'))
        colnames(dt.dat) <- dt.dat[1,]
        dt.dat <- as.data.frame(dt.dat[-1,], stringsAsFactors = FALSE)
        setDT(dt.dat)
        dt.dat[, names(dt.dat) := lapply(.SD, as.numeric)]
        dt.dat <- dt.dat[, lapply(.SD, sum), by = stream, .SDcols = attr_name_vect[j]]
        # MK 01.052018: Why did I set the names starting with "s"?
        #setnames(dt.dat, attr_name_vect[j], paste0("s", attr_name_vect[j]))
      }
      # MiKatt 20190627 switch on 
      names(dt.dat)[-1] <- paste0(names(dt.dat)[-1], substr(stat_vect[j],1,1))
      anames <- c(anames, names(dt.dat)[-1])
      nanames[j] <- ncol(dt.dat) - 1
      dt.streams <- merge(dt.streams, dt.dat, by = "stream", all.x = TRUE)
    }
    for (k in anames){
      set(dt.streams, which(is.na(dt.streams[[k]])),k , 0)
    }
    stat_vect2 <- c(unlist(sapply(1:length(nanames), function(x) rep(stat_vect[x], each=nanames[x]))))
    round_dig2 <- c(unlist(sapply(1:length(nanames), function(x) rep(round_dig[(1+length(input_raster)):length(round_dig)][x], each=nanames[x]))))
    
    #dt <- copy(dt.streams)
    calc_catchment_attributes_vect(dt.streams, stat_vect2, anames, round_dig2)
    
    # Delete unneeded columns
    dt.streams[, c("next_str", "prev_str01", "prev_str02", "netID", "H2OArea", "rcaArea") := NULL]
    # truncate column names
    n <- nchar(anames)
    nn <- which(n > 9)
    anames2 <- anames
    if(length(nn) > 0){
      anames2[nn] <- paste0(substr(anames[nn], start = 1, stop = 7), substr(anames[nn], start = nchar(anames[nn]), stop = nchar(anames[nn])))     
    }
    setnames(dt.streams, anames, paste0(anames2, "_e"))

    # Join attributes to edges attribute table
    message("Joining table vector attributes ...")
    utils::write.csv(dt.streams, file.path(temp_dir,"edge_attributes.csv"),row.names = F)
    write.table(t(gsub("numeric","Real",sapply(dt.streams,class))),file.path(temp_dir,"edge_attributes.csvt"),quote=T,sep=",",row.names = F,col.names = F)
    execGRASS("db.in.ogr", flags = c("overwrite","quiet"),
              parameters = list(
                input = file.path(temp_dir,"edge_attributes.csv"),
                output = "edge_attributes"
              ),ignore.stderr = TRUE, intern = TRUE)
    
    execGRASS("v.db.join", flags = "quiet",
              parameters = list(
                map = "edges",
                column = "stream",
                other_table = "edge_attributes",
                other_column = "stream"
              ))
    
    # remove temporary files
    execGRASS("g.remove",
              flags = c("quiet", "f"),
              parameters = list(
                type = "vector",
                name = "temp_inters"
              ), ignore.stderr = TRUE)
  }
  
  cnames_edges2 <- execGRASS("db.columns", flags = "quiet",
                             parameters = list(
                               table = "edges"
                             ), ignore.stderr = TRUE, intern = TRUE)
  cnames_edges2 <- cnames_edges2[-(which(cnames_edges2 %in% cnames_edges))]
  cnames_edges2 <- unique(gsub("_c|_e$", "", cnames_edges2))
  i <- which(cnames_edges2 == "cat_")
  if(length(i) > 0)
    cnames_edges2 <- cnames_edges2[-i]
  #message(writeLines(strwrap(paste0("\nNew attributes values are stored as ", paste("'", cnames_edges2, "'", collapse = ", ", sep = ""), " in 'edges'."),
  #        width = 80)))
  message(paste0("\nNew attributes values are stored as ", paste("'", cnames_edges2, "'", collapse = ", ", sep = ""), " in 'edges'."))
  
  # remove temporary files
  execGRASS("db.droptable", flags = c("quiet","f"),
            parameters = list(
              table = "edge_attributes"
            ))
  invisible(file.remove(file.path(temp_dir,"edge_attributes.csv")))
  invisible(file.remove(file.path(temp_dir,"edge_attributes.csvt")))
}

#' calc_catchment_attributes_rast
#' Aggregate attributes for the total catchment of each stream segment.
#'
#' This function aggregates the attributes of each segment for the total
#' catchment of each stream segment. It is called within \code{\link{calc_attributes_edges}}
#' and should not be called by the user.
#'
#' @param dt data.table of stream topology and attributes per segment.
#' @param stat_rast name or character vector giving the statistics to be calculated,
#'   must be one of: min, max, mean, percent, sum.
#' @param attr_name_rast name or character vector of column names for the attribute(s)
#'   to be calculated.
#' @param round_dig integer; number of digits to round results to. Can be a vector
#'   of different values or just one value for all attributes.
#'
#' @return Nothing. The function changes the values of the columns attr_name_rast in dt.

#dt <- copy(dt.streams)
calc_catchment_attributes_rast <- function(dt, stat_rast, attr_name_rast, round_dig){
  outlets <- dt[next_str == -1, stream]
  dt[, paste0(attr_name_rast,"_c") := dt[, attr_name_rast, with = FALSE]]
  for(i in outlets){
    calc_catchment_attributes_rast_rec(dt, id=i, stat_rast, paste0(attr_name_rast,"_c"))
  }
  # for "mean" and "percent", calc_catchment_attributes_rast_rec gives the cummulative sum of mean value * non_null_cells
  # => divide here by total number of cells
  ind <-  which(stat_rast %in% c("mean", "percent", "percent_class"))# c(grep("mean", stat_rast), grep("percent", stat_rast))
  if(length(ind) > 0){
    dt[cumsum_cells > 0, paste0(attr_name_rast[ind], "_c") := lapply(.SD, function(x) x / cumsum_cells), .SDcols =  paste0(attr_name_rast[ind], "_c")]
    #dt[cumsum_cells <= 0, paste0(attr_name_rast[ind], "_c1") := lapply(.SD, function(x) x ), .SDcols =  paste0(attr_name_rast[ind], "_c")]
  }
  newcols <- paste0(rep(attr_name_rast, each = 2), c("", "_c"))
  setcolorder(dt, c(colnames(dt)[!colnames(dt) %in% newcols], newcols))
}


#' calc_catchment_attributes_rast_rec
#' Aggregate attributes for the total catchment of each stream segment.
#'
#' @description Recursive function to calculate the catchment attributes of each stream
#' segment. It is called by \code{\link{calc_catchment_attributes_rast}} for each
#' outlet and should not be called by the user.
#'
#' @param dt data.table of stream topology and attributes per segment.
#' @param id integer; 'stream' of outlet segment to start the calculation from.
#' @param stat name or character vector giving the statistics to be calculated,
#'   must be one of: min, max, mean, percent.
#' @param attr_name name or character vector of column names for the attribute(s)
#'   to be calculated.
#'
#' @return One row data.table with the cumulative number of cells of the total
#'  catchment of each segment and the values for each attribute and changes the
#'  values in dt.
#'
#' @note The values for stats "mean" and "percent" need to be divided by the cumulative
#'  number of cells of the total catchment in a subsequent step.

calc_catchment_attributes_rast_rec <- function(dt, id, stat, attr_name){
  if(dt[stream == id, prev_str01,] == 0){  # check only one of prev01 and prev02 because they are always both 0
    dt[stream == id, cumsum_cells := non_null_cells]
    for(j in 1:length(stat)){
      if(stat[j] %in% c("mean", "percent", "percent_class"))
        dt[stream == id, attr_name[j] := dt[stream == id, attr_name[j], with = FALSE] * dt[stream == id, non_null_cells]]
    } # else: do nothing (value = value)
  }  else {
    d1 <- calc_catchment_attributes_rast_rec(dt, dt[stream == id, prev_str01], stat, attr_name)
    d2 <- calc_catchment_attributes_rast_rec(dt, dt[stream == id, prev_str02], stat, attr_name)
    dt[stream == id, cumsum_cells := d1[,cumsum_cells] + d2[, cumsum_cells] + dt[stream == id, non_null_cells]]
    for(j in 1:length(stat)){
      if(stat[j] %in% c("min", "max", "sum")){
        dt[stream == id, attr_name[j] :=
             eval(call(stat[j],unlist(c(d1[,attr_name[j], with = FALSE],
                                        d2[,attr_name[j], with = FALSE],
                                        dt[stream == id, attr_name[j], with = FALSE])), na.rm = TRUE))]
      } else if(stat[j] %in% c("percent", "mean", "percent_class")){# for percent and mean sum values (relative to cell number)
        dt[stream == id, attr_name[j] := dt[stream == id, attr_name[j], with = FALSE] * dt[stream == id, non_null_cells] +
             d1[, attr_name[j], with = FALSE] + d2[, attr_name[j], with = FALSE]]
      } else { # for area sum values
        dt[stream == id, attr_name[j] := dt[stream == id, attr_name[j], with = FALSE] +
             d1[, attr_name[j], with = FALSE] + d2[, attr_name[j], with = FALSE]]
      }
    }
  }
  return(dt[stream == id,c("cumsum_cells", attr_name), with = FALSE])
}

#' calc_catchment_attributes_vect
#' Aggregate attributes for the total catchment of each stream segment.
#'
#' This function aggregates the attributes of each segment for the total
#' catchment of each stream segment. It is called within \code{\link{calc_attributes_edges}}
#' and should not be called by the user.
#'
#' @param dt data.table of stream topology and attributes per segment.
#' @param stat_vect name or character vector giving the statistics to be calculated,
#'   must be one of: percent, sum.
#' @param attr_name_vect name or character vector of column names for the attribute(s)
#'   to be calculated.
#' @param round_dig integer; number of digits to round results to. Can be a vector
#'   of different values or just one value for all attributes.
#'
#' @return Nothing. The function changes the values of the columns attr_name_vect in dt.


calc_catchment_attributes_vect <- function(dt, stat_vect, attr_name_vect, round_dig){
  outlets <- dt[next_str == -1, stream]
  dt[, paste0(attr_name_vect, "_c") := dt[, attr_name_vect, with = FALSE]]
  for(i in outlets){
    calc_catchment_attributes_vect_rec(dt, id=i, stat_vect, paste0(attr_name_vect,"_c"))
  }
  ind <- grep("percent", stat_vect)
  if(length(ind) > 0){
    #dt[H2OArea > 0, paste0(attr_name_vect[ind], "_c") := dt[H2OArea > 0,paste0(attr_name_vect[ind],"_c"), with = FALSE] / (dt[H2OArea > 0, H2OArea] * 1000000)]
    #sapply(ind, function(x) round(dt[, paste0(attr_name_vect[x], "_c"), with = FALSE], round_dig[x]))
    dt[H2OArea > 0, paste0(attr_name_vect[ind], "_c") := round(dt[H2OArea > 0,paste0(attr_name_vect[ind],"_c"), with = FALSE] / (dt[H2OArea > 0, H2OArea] * 1000000), round_dig[ind])]
    dt[rcaArea > 0, attr_name_vect[ind] := round(dt[rcaArea > 0,attr_name_vect[ind], with = FALSE] / (dt[rcaArea > 0, rcaArea] * 1000000), round_dig[ind])]
    # TODO: correct, check, why it did not work with this!!
    # for(j in 1:length(ind)){
    #   dt[paste0(attr_name_vect[ind[j]], "_c") > 1, paste0(attr_name_vect[ind[j]], "_c") := 1]
    #   dt[attr_name_vect[ind[j]] > 1,attr_name_vect[ind[j]] := 1]
    # }
  }
  newcols <- paste0(rep(attr_name_vect, each = 2), c("", "_c"))
  setcolorder(dt, c(colnames(dt)[!colnames(dt) %in% newcols], newcols))
}

#' calc_catchment_attributes_vect_rec
#' Aggregate attributes for the total catchment of each stream segment.
#'
#' @description Recursive function to calculate the catchment attributes of each stream
#' segment. It is called by \code{\link{calc_catchment_attributes_vect}} for each
#' outlet and should not be called by the user.
#'
#' @param dt data.table of stream topology and attributes per segment.
#' @param id integer; 'stream' of outlet segment to start the calculation from.
#' @param stat_vect name or character vector giving the statistics to be calculated,
#'   must be one of: min, max, mean, percent.
#' @param attr_name_vect name or character vector of column names for the attribute(s)
#'   to be calculated.
#' @keywords internal
#'
#' @return One row data.table with the cumulative number of cells of the total
#'  catchment of each segment and the values for each attribute and changes the
#'  values in dt.

calc_catchment_attributes_vect_rec <- function(dt, id, stat_vect, attr_name_vect){
  if(dt[stream == id, prev_str01,] != 0){  # check only one of prev01 and prev02 because they are always both 0
    d1 <- calc_catchment_attributes_vect_rec(dt, dt[stream == id, prev_str01], stat_vect, attr_name_vect)
    d2 <- calc_catchment_attributes_vect_rec(dt, dt[stream == id, prev_str02], stat_vect, attr_name_vect)
    for(j in 1:length(stat_vect)){ 
      dt[stream == id, attr_name_vect[j] := dt[stream == id, attr_name_vect[j], with = FALSE] +
           d1[, attr_name_vect[j], with = FALSE] + d2[, attr_name_vect[j], with = FALSE]]
    }
  }
  return(dt[stream == id, attr_name_vect, with = FALSE])
}



#' get_n_val_raster
#' Returns the number of differnt values in the raster.
#'
#' @description Returns the number of different values in the input raster.
#'
#' @param raster_name name of the raster map
#' @keywords internal
#'
#' @return The range of values in the raster map.
#' 
#' @note This function is sensitive to MASKs, i.e. if a MASK is present,
#'  only the part or the raster is processed within the MASK; 
#'  \href{https://grass.osgeo.org/grass75/manuals/r.mask.html}{r.mask}.

get_n_val_raster <- function(raster_name){
  return(length(get_all_raster_values(raster_name)))
}

#' get_all_raster_values
#' Returns all unuque values in the raster
#' 
#' @description Returns the number of different values in the input raster.
#'
#' @param raster_name name of the raster map
#' @keywords internal
#'
#' @return a vector of all values in the raster
#' 
#' @note This function is sensitive to MASKs, i.e. if a MASK is present,
#'  only the part or the raster is processed within the MASK; 
#'  \href{https://grass.osgeo.org/grass75/manuals/r.mask.html}{r.mask}.

get_all_raster_values <- function(raster_name){
  r <- execGRASS("r.stats", flags = c("l","n"),
                 parameters = list(
                   input = raster_name
                 ), ignore.stderr = TRUE,
                 intern = TRUE)
  r <- gsub(" $", "", r)
  return(r)
}
