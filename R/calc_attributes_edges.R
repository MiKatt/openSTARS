#' Calculate attributes of the edges.
#'
#' For each edge (i.e. stream segment) additional attributes (predictor variables)
#' are derived based on given raster maps.
#'
#' @param input_raster name(s) of names of the raster map(s)
#'   to calculate attributes from.
#' @param stat_rast name(s) giving the statistics to be calculated,
#'   from the raster maps, must be one of: "min", "max", "mean", "sum", "percent".
#' @param attr_name_rast name(s) of new column names for the attribute(s)
#'   to be calculated. Attribute names must not be longer than 8 characters.
#' @param input_vector name(s) of names of the vector map(s)
#'   to calculate attributes from.
#' @param stat_vect name(s) giving the statistics to be calculated
#'   from the vector maps, must be one of: "count" (for point data), "percent"
#'   (for polygon data).
#' @param attr_name_vect name(s) of attribute column(s) to calculate the 
#'   statistics from. For point data, results columns will have the same name, for
#'   polygon data, the results column names are determined by the content of 
#'   this column.
#' @param round_dig integer; number of digits to round results to. Can be a vector
#'   of different values or just one value for all attributes.
#' @param clean logical; should intermediate files be deleted

#' @return Nothing. The function appends new columns to the 'edges' attribute
#'   table with column names given in \code{attr_name_rast}. For each attribute, two
#'   columns are appended: one giving the attribute for the rca of the edge
#'   ("attribute_name_e") and one for the attribute of the total catchment of
#'   the edge ("attribute_name_c").
#'
#'@details First, the subcatchments for all edges are calculated. Then these are
#' intersected with the given raster maps and the desired statistics are computed.
#' This is needed to compute approximate attribute values for sites \code{\link{calc_attributes_sites_approx}}.
#'
#'For \code{stat_rast} = "percent" the \code{input_raster} must be coded as 1 and 0
#'  (e.g., cells occupied by the land use under consideration and not). If
#'   the \code{input_raster} consists of percentages per cell (e.g., proportional land
#'   use of a certain type per cell) \code{stat_rast} = "mean" gives the overall proportion
#'   of this land use.
#'
#' @note \code{\link{setup_grass_environment}}, \code{\link{import_data}},
#' \code{\link{derive_streams}} and \code{\link{calc_edges}} must be run before.
#'
#' @author Mira Kattwinkel, \email{mira.kattwinkel@@gmx.net}
#' @export
#' 
#' @examples
#' \donttest{
#' # Initiate GRASS session
#' if(.Platform$OS.type == "windows"){
#'   gisbase = "c:/Program Files/GRASS GIS 7.2.0"
#'   } else {
#'   gisbase = "/usr/lib/grass72/"
#'   }
#' initGRASS(gisBase = gisbase,
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
#' # Check and correct complex junctions (there are no complex juctions in this 
#' # example date set)
#' cj <- check_compl_junctions()
#' if(cj){
#'   correct_compl_junctions()
#' }
#' 
#' # Prepare edges
#' calc_edges()
#' 
#' # Plot data
#' dem <- readRAST('dem', ignore.stderr = TRUE)
#' edges <- readVECT('edges', ignore.stderr = TRUE)
#' plot(dem, col = terrain.colors(20))
#' lines(edges, col = "blue", lwd = 2)
#' }
#'

calc_attributes_edges <- function(input_raster = NULL, stat_rast = NULL, attr_name_rast = NULL,
                                  input_vector = NULL, stat_vect = NULL, attr_name_vect = NULL,
                                  round_dig = 2, clean = FALSE){
  
  if(length(input_raster) != length(stat_rast) | length(input_raster) != length(attr_name_rast) | length(attr_name_rast) != length(stat_rast))
    stop(paste0("There must be the same number of input raster files (",length(input_raster), "), statistics to calculate (",
                length(stat_rast), ") and attribute names (", length(attr_name_rast),")."))
  
  if(any(!stat_rast %in% c("min","max", "mean", "sum", "percent")))
    stop('statistics to calculate must be one of "min","max", "mean", "sum" or "percent".')
  
  if(any(sapply(attr_name_rast, nchar) > 8))
    stop("Attribute names must not be longer than eight characters.")
  
  if(length(input_vector) != length(stat_vect) | length(input_vector) != length(attr_name_vect) | length(attr_name_vect) != length(stat_vect))
    stop(paste0("There must be the same number of input raster files (",length(input_vector), "), statistics to calculate (",
                length(stat_vect), ") and attribute names (", length(attr_name_vect),")."))
  
  if(any(!stat_vect %in% c("percent", "count")))
    stop('statistics to calculate must be one of "count" or "percent".')
  
  i <- which(stat_vect == "count")
  if(length(i) > 0){
    for(j in i){
      a <- execGRASS("v.info", flags = "t",
                     parameters = list(
                       map = input_vector[j]
                     ), intern = T)
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
  
  if(any(sapply(attr_name_vect, nchar) > 8))
    stop("Attribute names must not be longer than eight characters.")
  
  if(is.null(input_raster) & is.null(input_vector))
    stop("At least one raster or vector file name must be given for intersection.")
  
  temp_dir <- tempdir()
  
  rast <- execGRASS("g.list",
                    parameters = list(
                      type = "rast"
                    ),
                    intern = TRUE)
  if (!"streams_r" %in% rast | !"dirs" %in% rast)
    stop("Missing data. Did you run derive_streams()?")
  
  if ("MASK" %in% rast)
    execGRASS("r.mask",flags = c("r", "quiet"))
  
  if(length(round_dig) == 1)
    round_dig <- rep(round_dig, length(stat_rast) + length(stat_vect))
  
  ## Calculate reach contributing area (= sub chatchment) for each segment).
  message("Calculating reach contributing area (RCA)...\n")
  execGRASS("r.stream.basins",
            flags = c("overwrite", "quiet"),
            parameters = list(direction = "dirs",
                              stream_rast = "streams_r",
                              basins = "rca"))
  
  if(!is.null(input_raster)){
    message("Intersecting raster attributes...")
    rca_cell_count <- execGRASS("r.univar",
                                flags = c("overwrite", "quiet","t"),
                                parameters = list(
                                  map = "rca",
                                  zones = "rca",
                                  separator = "comma"),
                                intern = TRUE)
    rca_cell_count <- do.call(rbind,strsplit(rca_cell_count, ","))
    colnames(rca_cell_count) <- rca_cell_count[1,]
    rca_cell_count <- rca_cell_count[-1, c("zone","non_null_cells")]
    rca_cell_count <- data.frame(apply(rca_cell_count,2,as.numeric))
    setDT(rca_cell_count)
    
    for(j in 1:length(stat_rast)){
      st <- execGRASS("r.univar",
                      flags = c("overwrite", "quiet","t"),
                      parameters = list(
                        map = input_raster[j],
                        zones = "rca",
                        separator = "comma"),
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
    }
    
    dt.streams <- do.call(rbind,strsplit(
      execGRASS("db.select",
                parameters = list(
                  sql = "select cat, stream, next_str, prev_str01, prev_str02, netID from edges"
                ),intern = T),
      split = '\\|'))
    colnames(dt.streams) <- dt.streams[1,]
    dt.streams <- data.table(dt.streams[-1,,drop = FALSE])
    dt.streams[, names(dt.streams) := lapply(.SD, as.numeric)]
    dt.streams <- merge(dt.streams, rca_cell_count, by.x = "cat", by.y = "zone", all.x = TRUE)
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
    dt.streams[, c("stream", "next_str", "prev_str01", "prev_str02", "netID", "non_null_cells","cumsum_cells") := NULL]
    setnames(dt.streams, attr_name_rast, paste0(attr_name_rast, "_e"))
    
    # Join attributes to edges attribute table
    message("Joining table raster attributes...")
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
                column = "cat",
                other_table = "edge_attributes",
                other_column = "cat_"
              ))
  }
  
  if(!is.null(input_vector)){
    message("Intersecting vector attributes...")
    
    # convert raster rca to vector
    execGRASS("r.to.vect", flags = c("overwrite", "quiet"),
              parameters = list(
                input = "rca",
                output = "rca_v",
                type = "area",
                column = "edge_cat"
              ))
    
    dt.streams <- do.call(rbind,strsplit(
      execGRASS("db.select",
                parameters = list(
                  sql = "select cat, stream, next_str, prev_str01, prev_str02, netID, rcaArea, H2OArea from edges"
                ),intern = T),
      split = '\\|'))
    colnames(dt.streams) <- dt.streams[1,]
    dt.streams <- data.table(dt.streams[-1,,drop = FALSE])
    dt.streams[, names(dt.streams) := lapply(.SD, as.numeric)]
    
    anames <- NULL
    nanames <- vector(length = length(stat_vect))
    
    for(j in 1:length(stat_vect)){
      if(vtype[j] == 1){ # if vector data
        
        execGRASS("v.overlay", flags = c("overwrite", "quiet"),
                  parameters = list(
                    ainput = "rca_v",
                    binput = input_vector[j],
                    operator = "and",
                    output = "temp_inters"
                  ))
        execGRASS("v.db.addcolumn", flags = "quiet",
                  parameters = list(
                    map =  "temp_inters",
                    columns = "area double"
                  ))
        execGRASS("v.to.db", flags = c("quiet"),
                  parameters = list(
                    map =  "temp_inters",
                    option = "area",
                    columns = "area"
                  ))
        cname <- paste0("b_", attr_name_vect[j])
        dt.dat <- do.call(rbind,strsplit(
          execGRASS("db.select",
                    parameters = list(
                      sql = paste0('select a_edge_cat, area, ', cname, ' from temp_inters')
                    ),intern = T),
          split = '\\|'))
        colnames(dt.dat) <- dt.dat[1,]
        dt.dat <- as.data.frame(dt.dat[-1,], stringsAsFactors = F)
        setDT(dt.dat)
        setnames(dt.dat, "a_edge_cat", "edge_cat")
        dt.dat[, c("edge_cat", "area") := lapply(.SD, as.numeric), .SDcols =  c("edge_cat", "area")]
        dt.dat <- dt.dat[, .(area = sum(area)), c("edge_cat", cname)] 
        dt.dat <- dcast(dt.dat, paste0("edge_cat  ~ b_", attr_name_vect[j]), value.var = "area")
        setnames(dt.dat, names(dt.dat)[-1], paste0("p", names(dt.dat)[-1]))
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
                      sql = paste0('select edge_cat, ', attr_name_vect[j], ' from rca_v')
                    ),intern = T),
          split = '\\|'))
        colnames(dt.dat) <- dt.dat[1,]
        dt.dat <- as.data.frame(dt.dat[-1,], stringsAsFactors = F)
        setDT(dt.dat)
        dt.dat[, names(dt.dat) := lapply(.SD, as.numeric)]
        dt.dat <- dt.dat[, lapply(.SD, sum), by = edge_cat, .SDcols = attr_name_vect[j]]
        setnames(dt.dat, attr_name_vect[j], paste0("s", attr_name_vect[j]))
      }
      nanames[j] <- ncol(dt.dat) - 1
      anames <- c(anames, names(dt.dat)[-1])
      dt.streams <- merge(dt.streams, dt.dat, by.x = "cat", by.y = "edge_cat", all.x = TRUE)
    }
    for (k in anames){
      set(dt.streams, which(is.na(dt.streams[[k]])),k , 0)
    }
    stat_vect2 <- c(unlist(sapply(1:length(nanames), function(x) rep(stat_vect[x], each=nanames[x]))))
    round_dig2 <- c(unlist(sapply(1:length(nanames), function(x) rep(round_dig[(1+length(stat_rast)):length(round_dig)][x], each=nanames[x]))))
    calc_catchment_attributes_vect(dt.streams, stat_vect2, anames, round_dig2)
    
    # Delete unneeded columns
    dt.streams[, c("stream", "next_str", "prev_str01", "prev_str02", "netID", "H2OArea", "rcaArea") := NULL]
    setnames(dt.streams, anames, paste0(anames, "_e"))
    
    # Join attributes to edges attribute table
    message("Joining table raster attributes...")
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
                column = "cat",
                other_table = "edge_attributes",
                other_column = "cat_"
              ))
  }
  
  if (clean) {
    execGRASS("g.remove",
              flags = c("quiet", "f"),
              parameters = list(
                type = "raster",
                name = "rca"
              ))
    execGRASS("db.droptable", flags = c("quiet","f"),
              parameters = list(
                table = "edge_attributes"
              ))
  }
}

#' calc_catchment_attributes_rast
#' Aggregate attributes for the total catchment of each stream segment.
#'
#' This function aggregates the attributes of each segment for the total
#' catchment of each stream segment. It is called within \code{\link{calc_attributes_edges}}
#' and should not be called by the user.
#'
#' @param dt data.table of stream topology and attributes per segment.
#' @param stat_rast name or character vector giving the stastistics to be calculated,
#'   must be one of: min, max, mean, percent, sum.
#' @param attr_name_rast name or character vector of column names for the attribute(s)
#'   to be calculated.
#' @param round_dig integer; number of digits to round results to. Can be a vector
#'   of different values or just one value for all attributes.
#'
#' @return Nothing. The function changes the values of the columns attr_name_rast in dt.

calc_catchment_attributes_rast <- function(dt, stat_rast, attr_name_rast, round_dig){
  outlets <- dt[next_str == -1, stream]
  dt[, paste0(attr_name_rast,"_c") := dt[, attr_name_rast, with = FALSE]]
  for(i in outlets){
    calc_catchment_attributes_rast_rec(dt, id=i, stat_rast, paste0(attr_name_rast,"_c"))
  }
  # for "mean" and "percent", calc_catchment_attributes_rast_rec gives the cmmulative sum of mean value * non_null_cells
  # => divide here by total number of cells
  ind <- c(grep("mean", stat_rast), grep("percent", stat_rast))
  if(length(ind) > 0)
    dt[cumsum_cells > 0, paste0(attr_name_rast[ind], "_c") := round(dt[cumsum_cells > 0, paste0(attr_name_rast[ind],"_c"), with = FALSE] / dt[cumsum_cells > 0, cumsum_cells], round_dig[ind])]

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
      if(stat[j] %in% c("mean","percent"))
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
      } else{# for percent and mean sum values (relative to cell number)
        dt[stream == id, attr_name[j] := dt[stream == id, attr_name[j], with = FALSE] * dt[stream == id, non_null_cells] +
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
#' @param stat_vect name or character vector giving the stastistics to be calculated,
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
    dt[H2OArea > 0, paste0(attr_name_vect[ind], "_c") := round(dt[H2OArea > 0,paste0(attr_name_vect[ind],"_c"), with = FALSE] / (dt[H2OArea > 0, H2OArea] * 1000000), round_dig[ind])]
    dt[rcaArea > 0, attr_name_vect[ind] := round(dt[rcaArea > 0,attr_name_vect[ind], with = FALSE] / (dt[rcaArea > 0, rcaArea] * 1000000), round_dig[ind])]
    dt[paste0(attr_name_vect[ind], "_c") > 1, paste0(attr_name_vect[ind], "_c") := 1]
    dt[attr_name_vect[ind] > 1,attr_name_vect[ind] := 1]
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
