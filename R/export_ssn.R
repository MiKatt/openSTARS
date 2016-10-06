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
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
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
export_ssn <- function(path, binary, predictions = NULL){
  message('Exporting to ', path)
  # write edges
  # MiKatt first copy edges and drop attributes not needed for ssn
  execGRASS("g.copy",
            flags = c('overwrite', 'quiet'),
            parameters = list(
              vector = 'edges,edges2'))
  execGRASS('v.db.dropcolumn',
            flags = 'quiet',
            parameters = list(
              map = 'edges2',
              columns = 'stream,next_stream,prev_str01,prev_str02'
            ))
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
  if(length(predictions) > 0){
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
