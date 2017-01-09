#' Export .ssn object
#'
#' This function exports the calculated sites, edges and binary IDs
#' to a folder which then can be read using the SSN package.
#'
#' @import rgrass7
#' @importFrom utils write.table
#'
#' @param path character; path to write .ssn object to.
#' @param binary object of type \code{binary}, as created by \code{\link{calc_binary}}.
#' @param predictions name(s) of prediction map(s) (optional).
#'
#' @return Nothing. Files are written to the specified folder
#'
#' @details First it is checked if one of the column names is longer than 10
#' characters (which cannot be exported to shape).
#'
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}, Mira Kattwinkel,\email{mira.kattwinkel@gmx.net}
#' @export
#' @examples
#' \donttest{
#'
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
#' cj <- check_compl_junctions()
#' if(cj)
#'   correct_compl_junctions()
#' calc_edges()
#' calc_sites()
#' binaries <- calc_binary()
#' ssn_dir <- file.path(tempdir(), 'nc.ssn')
#' export_ssn(ssn_dir, binary = binaries)
#' list.files(ssn_dir)
#' }
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

  message('Exporting to ', path)
  # write edges
  # MiKatt first copy edges and drop attributes not needed for ssn
  execGRASS("g.copy",
            flags = c('overwrite', 'quiet'),
            parameters = list(
              vector = 'edges,edges2'))
  # execGRASS('v.db.dropcolumn',
  #           flags = 'quiet',
  #           parameters = list(
  #             map = 'edges2',
  #             columns = 'stream,next_str,prev_str01,prev_str02'
  #           ))

  execGRASS('v.out.ogr',
            c('overwrite', 'quiet'),
            parameters = list(
              input = 'edges2',
              type = 'line',
              output = path,
              output_layer = 'edges'
            ))
  execGRASS("g.remove",
            flags = c('quiet', 'f'),
            parameters = list(
              type = 'vector',
              name = 'edges2'
            ))

  # write sites
  execGRASS('v.out.ogr',
            c('overwrite', 'quiet'),
            parameters = list(
              input = 'sites',
              type = 'point',
              output = path,
              output_layer = 'sites'
            ))

  # write preds
  if(!is.null(predictions)){
    for(i in seq_along(predictions))
      execGRASS('v.out.ogr',
                c('overwrite', 'quiet'),
                parameters = list(
                  input = predictions[i],
                  type = 'point',
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
                       file.path(path, paste0('netID', y, '.dat')),
                       sep = ',',
                       row.names = FALSE)
         })
  # restore
  options(scipen = old_scipen)
}
