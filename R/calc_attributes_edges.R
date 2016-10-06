#' Calcuate attributes of the edges
#'
#' For each edge, additional attributes (predictor variables) are derived based
#' on given raster maps.
#'
#' @param input_raster name or character vector of names of the raster map(s)
#'   to calculate attributes from.
#' @param stat name or character vector giving the statistics to be calulated,
#'   must be one of: min, max, mean, percent.
#' @param attr_name name or character vector of column names for the attribute(s)
#'   to be caculated. Attribute names must not be longer than 8 characters.
#' @param round_dig integer; number of digits to round results to. Can be a vector
#'   of different values or just one value for all attributes.
#' @param clean logical; should intermediate files be deleted

#' @return Nothing. The function appends new columns to the 'edges' attribute
#'   table with column names given in \code{attr_name}. For each attribute, tow
#'   columns are appended: one giving the attribute for the rca of the edge
#'   ("attribute_name_e") and one for the attribute of the total catchment of
#'   the edge ("attribute_name_c").
#'
#'@details For \code{stat} = "percent" the \code{input_raster} must be coded as 1 and 0
#'  (e.g., cells occupied by the land use under consideration and not). If
#'   the \code{input_raster} consists of percentages per cell (e.g., proportional land
#'   use of a certain type per cell) \code{stat} = "mean" gives the overall proportion
#'   of this land use.
#'
#' @note \code{\link{import_data}}, \code{\link{derive_streams}} and
#'   \code{\link{calc_edges}} must be run before.
#'
#' @author Mira Kattwinkel, \email{mira.kattwinkel@@gmx.net}
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
#' calc_attributes_edges(input_raster = rep("slope",3),
#'   stat = c("mean", "min","max"), attr_name = paste0(c("mean", "min","max"),"Slope"))
#' }

calc_attributes_edges <- function(input_raster, stat, attr_name, round_dig = 2, clean = TRUE){

  if(length(input_raster) != length(stat) | length(input_raster) != length(attr_name) | length(attr_name) != length(stat))
    stop(paste0("There must be the same number of input raster files (",length(input_raster), "), statistics to calculate (",
                length(stat), ") and attribute names (", length(attr_name),")."))

  if(any(!stat %in% c("min","max", "mean", "percent")))
    stop('Statistics to calculate must be one of "min","max", "mean", "percent".')

  rast <- execGRASS("g.list",
                    parameters = list(
                      type = 'rast'
                    ),
                    intern = TRUE)
  if (!'streams_r' %in% rast | !'dirs' %in% rast)
    stop('Missing data. Did you run derive_streams()?')

  if ("MASK" %in% rast)
    execGRASS("r.mask",flags = c("r", "quiet"))

  if(length(round_dig) == 1)
    round_dig <- rep(round_dig, length(stat)+1)

  ## Calculate reach contributing area (=sub chatchment) for each segment.
  message('Calculating reach contributing area (RCA)...\n')
  execGRASS("r.stream.basins",
            flags = c("overwrite", "quiet"),
            parameters = list(direction = 'dirs',
                              stream_rast = 'streams_r',
                              basins = "rca"))

  message("Intersecting attributes...")
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

  for(j in 1:length(stat)){
    st <- execGRASS("r.univar",
                    flags = c("overwrite", "quiet","t"),
                    parameters = list(
                      map = input_raster[j],
                      zones = "rca",
                      separator = "comma"),
                    intern = TRUE)
    st <- do.call(rbind,strsplit(st,","))
    colnames(st) <- st[1,]
    st <- st[-1,]
    st <- data.frame(apply(st,2,as.numeric))
    setDT(st)

    if(stat[j] %in% names(st)){
      st <- st[, c("zone", stat[j]), with = FALSE]
    } else { # if(stat == "percent")
      st <- st[, c("zone", "mean"), with = FALSE]  # if e.g. coded as 0 and 1, mean gives the ratio
    }
    names(st)[2] <- attr_name[j]
    st[, attr_name[j] := round(st[, attr_name[j], with = FALSE], round_dig[j])]
    rca_cell_count <- merge(rca_cell_count, st, by = "zone", all.x = TRUE)
  }

  dt.streams <- do.call(rbind,strsplit(
    execGRASS("db.select",
              parameters = list(
                sql = "select cat, stream, next_stream, prev_str01,prev_str02,netID from edges"
              ),intern = T),
    split = '\\|'))
  colnames(dt.streams) <- dt.streams[1,]
  dt.streams <- data.table(dt.streams[-1,])
  dt.streams[, names(dt.streams) := lapply(.SD, as.numeric)]
  dt.streams <- merge(dt.streams, rca_cell_count, by.x = "cat", by.y = "zone", all.x = TRUE)
  dt.streams[, cumsum_cells := 0]
  ind <- which(!stat %in% c("min", "max"))
  if(length(ind) > 0){
    for(i in ind)
      dt.streams[which(is.na(dt.streams[, attr_name[i], with = FALSE])),attr_name[i] := 0, with = FALSE]
  }
  dt.streams[is.na(non_null_cells), non_null_cells := 0]

  # This can take long
  # Calculate attributes
  calc_catchment_attributes(dt.streams, stat, attr_name, round_dig)


  # Delete unneeded columns
  dt.streams[, c("stream", "next_stream", "prev_str01", "prev_str02", "netID", "non_null_cells","cumsum_cells") := NULL]
  setnames(dt.streams, attr_name, paste0(attr_name, "_e"))

  # Join attributes to edges attribute table
  message("Joining tables...")
  dir.create("temp")
  write.csv(dt.streams, file.path("temp","edge_attributes.csv"),row.names = F)
  write.table(t(gsub("numeric","Real",sapply(dt.streams,class))),file.path("temp","edge_attributes.csvt"),quote=T,sep=",",row.names = F,col.names = F)
  execGRASS("db.in.ogr", flags = c("overwrite","quiet"),
            parameters = list(
              input = file.path("temp","edge_attributes.csv"),
              output = "edge_attributes"
            ),ignore.stderr = T)
  execGRASS("v.db.join", flags = "quiet",
            parameters = list(
              map = "edges",
              column = "cat",
              other_table = "edge_attributes",
              other_column = "cat_"
            ))

  if (clean) {
    execGRASS("g.remove",
              flags = c('quiet', 'f'),
              parameters = list(
                type = 'raster',
                name = 'rca'
              ))
    execGRASS("db.droptable", flags = c("quiet","f"),
              parameters = list(
                table = "edge_attributes"
              ))
    unlink("temp", recursive = TRUE, force = TRUE)
  }
}

#' Aggregate attributes for the total catchment of each stream segment.
#'
#' This function aggregates the attributes of each segment for the total
#' catchment of each stream segment. It is called within \code{\link{calc_attributes_edges}}
#' and should not be called by the user.
#'
#' @param dt data.table of stream topology and attributes per segment.
#' @param stat name or character vector giving the statistics to be calulated,
#'   must be one of: min, max, mean, percent.
#' @param attr_name name or character vector of column names for the attribute(s)
#'   to be caculated.
#' @param round_dig integer; number of digits to round results to. Can be a vector
#'   of different values or just one value for all attributes.
#'
#' @return Nothing. The function changes the values of the columns attr_name in dt.

calc_catchment_attributes <- function(dt, stat, attr_name, round_dig){
  outlets <- dt[next_stream == -1, stream]
  dt[, paste0(attr_name,"_c") := dt[, attr_name, with = FALSE]]
  for(i in outlets){
    calc_catchment_attributes_rec(dt, id=i, stat, paste0(attr_name,"_c"))
  }
  # for "mean" and "percent", calc_catchment_attributes_rec gives the cummulative sum of mean value * non_null_cells
  # => devide here by total number of cells
  ind <- c(grep("mean", stat), grep("percent", stat))
  if(length(ind) > 0)
    dt[, paste0(attr_name[ind], "_c") := round(dt[,paste0(attr_name[ind],"_c"), with = FALSE] / dt[,cumsum_cells], round_dig[ind])]

  newcols <- paste0(rep(attr_name, each = 2), c("", "_c"))
  setcolorder(dt, c(colnames(dt)[!colnames(dt) %in% newcols], newcols))
}

#' Aggregate attributes for the total catchment of each stream segment.
#'
#' @description Recursive function to calculate the catchment attributes of each stream
#' segment. It is called by \code{\link{calc_catchment_attributes}} for each
#' outlet and should not be called by the user.
#'
#' @param dt data.table of stream topology and attributes per segment.
#' @param id integer; 'stream' of outlet segment to start the calculation from.
#' @param stat name or character vector giving the statistics to be calulated,
#'   must be one of: min, max, mean, percent.
#' @param attr_name name or character vector of column names for the attribute(s)
#'   to be caculated.
#'
#' @return One row data.table with the cummulative number of cells of the total
#'  catchment of each segment and the values for each attribute.
#'
#' @note The values for stas "mean" and "percent" need to be devided by the cummulative
#'  number of cells of the total catchment in a subsequent step.
#'
calc_catchment_attributes_rec <- function(dt, id, stat, attr_name){
  if(dt[stream == id, prev_str01,] == 0){  # check only one of prev01 and prev02 because they are always both 0
    dt[stream == id, cumsum_cells := non_null_cells]
    for(j in 1:length(stat)){
      if(stat[j] %in% c("mean","percent"))
        dt[stream == id, attr_name[j] := dt[stream == id, attr_name[j], with = FALSE] * dt[stream == id, non_null_cells]]
    } # else: do nothing (value = value)
  }  else {
    d1 <- calc_catchment_attributes_rec(dt, dt[stream == id, prev_str01], stat, attr_name)
    d2 <- calc_catchment_attributes_rec(dt, dt[stream == id, prev_str02], stat, attr_name)
    dt[stream == id, cumsum_cells := d1[,cumsum_cells] + d2[, cumsum_cells] + dt[stream == id, non_null_cells]]
    for(j in 1:length(stat)){
      if(stat[j] %in% c("min", "max")){
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
