#' Export .ssn object
#'
#' This function exports the calculated sites, edges and binary IDs
#' to a folder which then can be read using the SSN package.
#'
#' @import rgrass7
#' @importFrom utils write.table
#'
#' @param path character; path to write .ssn object to.
#' @param predictions name(s) of prediction map(s) (optional).
#' @param delete_directory boolean; shall the ssn directory and all files be
#' deleted before export in case it already exists? See details.
#'
#' @return Nothing. Files are written to the specified folder
#'
#' @details First it is checked if one of the column names is longer than 10
#' characters (which cannot be exported to ESRI shape files as required by SSN).
#'
#' \code{delete_directory = TRUE} is useful if the same directory name has been
#' used before and the existing data shall be overwritten.
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}, Mira Kattwinkel,\email{mira.kattwinkel@gmx.net}
#' @export
#' 
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
#' # Prepare site
#' calc_sites()
#'
#' # Write data to SSN Folder
#' ssn_dir <- file.path(tempdir(), 'nc.ssn')
#' export_ssn(ssn_dir)
#' list.files(ssn_dir)
#' }
#' 
export_ssn <- function(path, predictions = NULL, delete_directory = FALSE){
  if(file.exists(path) & delete_directory == FALSE)
    stop(paste(path, "already exists. To delete it use 'delete_directory = TRUE'."))
  if(file.exists(path) & delete_directory == TRUE)
    unlink(path, recursive = T)
  cnames<-execGRASS("db.columns",
                    parameters = list(
                      table = "edges"
                    ), intern=T)

  cnames<-c(cnames,execGRASS("db.columns",
                    parameters = list(
                      table = "sites"
                    ), intern=T))

  if(!is.null(predictions)){
    cnames <- NULL
    for(i in 1:length(predictions)){
    cnames<-c(cnames,c(cnames,execGRASS("db.columns",
                               parameters = list(
                                 table = predictions[i]
                               ), intern=T)))
    }
  }

  if(any(unlist(lapply(cnames,nchar)) > 10))
    stop(paste("Some column names are longer than 10 characters and cannot be exported
         to shape files; please correct. Nothing exported.\n",
               paste(cnames[unlist(lapply(cnames,nchar)) > 10], collapse = ", ")))

  message("Exporting to ", path)
  # write edges
  # MiKatt first copy edges and drop attributes not needed for ssn
  execGRASS("g.copy",
            flags = c("overwrite", "quiet"),
            parameters = list(
              vector = "edges,edges2"))
  execGRASS("v.db.dropcolumn",
            flags = "quiet",
            parameters = list(
              map = "edges2",
              columns = "stream,next_str,prev_str01,prev_str02"
            ))
  execGRASS("v.out.ogr",
            flags = c("overwrite", "quiet"),
            parameters = list(
              input = "edges2",
              type = "line",
              output = path,
              output_layer = "edges"
            ))
  execGRASS("g.remove",
            flags = c("quiet", "f"),
            parameters = list(
              type = "vector",
              name = "edges2"
            ))

  # write sites
  execGRASS("v.out.ogr",
            c("overwrite", "quiet"),
            parameters = list(
              input = "sites",
              type = "point",
              output = path,
              output_layer = "sites"
            ))

  # write preds
  if(!is.null(predictions)){
    for(i in seq_along(predictions))
      execGRASS("v.out.ogr",
                c("overwrite", "quiet"),
                parameters = list(
                  input = predictions[i],
                  type = "point",
                  output = path,
                  output_layer = predictions[i]
                ))
  }

  # create binary
  binary <- calc_binary()

  # write binary files
  # temporarly turn off scientific notation
  old_scipen <-  options("scipen")$scipen
  options(scipen = 999)
  # write files
  lapply(names(binary),
         function(y) {
           write.table(binary[[y]],
                       file.path(path, paste0("netID", y, ".dat")),
                       sep = ",",
                       row.names = FALSE)
         })
  # restore
  options(scipen = old_scipen)
}

#' Calculate binary IDs for stream networks.
#'
#' Calculate binary IDs for each stream network built up by '0' and '1'.
#' This function is called by \code{\link{export_ssn}} and there is no need for it
#' be called by the users.
#'
#' @import data.table
#'
#' @return A list with one slot for each network id containing a data frame
#' with 'rid' and 'binaryID' for each segment belonging to this network.
#'
#' @note \code{\link{import_data}}, \code{\link{derive_streams}},
#'   \code{\link{calc_edges}} and \code{\link{calc_sites}} must be run before.
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}; Mira Kattwinkel, \email{mira.kattwinkel@@gmx.net}
#'
#'  @export
#'
calc_binary <- function(){
  vect <- execGRASS("g.list",
                    parameters = list(
                      type = "vect"
                    ),
                    intern = TRUE)
  if (!"sites_o" %in% vect)
    stop("Sites not found. Did you run import_data()?")
  if (!"edges" %in% vect)
    stop("edges not found. Did you run calc_edges()?")
  if (!"sites" %in% vect)
    stop("sites not found. Did you run calc_sites()?")

  dt.streams<-execGRASS("db.select",
                        flags = "c",
                        parameters = list(
                          sql = "select rid,stream,next_str,prev_str01,prev_str02,netID from edges",
                          separator = ","
                        ), intern = TRUE)

  dt.streams<-do.call(rbind,strsplit(dt.streams,split=","))
  dt.streams<-apply(dt.streams,2,as.numeric)
  colnames(dt.streams)<-c("rid","stream","next_str","prev_str01","prev_str02","netID")
  dt.streams <- data.frame(dt.streams)
  setDT(dt.streams)
  dt.streams[, binaryID := "0"]
  outlets <- dt.streams[next_str == -1, stream]

  for(i in outlets){
    assign_binIDs(dt = dt.streams, id=i, 1, NULL)
  }

  bins<-lapply(outlets, function(x) dt.streams[netID == dt.streams[stream == x, netID], list(rid,binaryID)])
  names(bins)<-  dt.streams[stream %in% outlets, netID]
  return(bins)
}

#' Recursive function to assign binary id to stream segments.
#'
#' This function is run for all outlets in the network ( = most downstream segments)
#' and fills the binID for all segments. It is called by \code{calc_binary} in
#' \code{export_ssn} and should not be called by the user.
#'
#' @param id: stream segment
#' @param binID: binary ID
#' @param lastbit: last char to be added (0 or 1)
#' @keywords internal
#'
assign_binIDs <- function(dt, id, binID, lastbit){
  if(dt[stream == id, prev_str01 ] == 0){  # check only one of prev01 and prev02 because they are always both 0
    dt[stream == id, binaryID := paste0(binID, lastbit)]
  } else {
    dt[stream == id, binaryID := paste0(binID,lastbit)]
    assign_binIDs(dt, dt[stream == id, prev_str01], dt[stream == id, binaryID], 0)
    assign_binIDs(dt, dt[stream == id, prev_str02], dt[stream == id, binaryID], 1)
  }
}
