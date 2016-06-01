#' Export .ssnb object
#'
#' This function takes the calculated sites, edges and binary IDs and exports them
#' to a folder which then can be read using the SSN package.
#'
#' @return Nothing. Files are written to the specified folder
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
#' @examples
#' library(rgrass7)
#' initGRASS(gisBase = "/usr/lib/grass70/",
#'   home = tempdir())
#' gmeta()
#' dem_path <- system.file("extdata", "nc", "elev_ned_30m.tif", package = "openSTARS")
#' sites_path <- system.file("extdata", "nc", "sites_nc.shp", package = "openSTARS")
#' import_data(dem = dem_path, sites = sites_path)
#' derive_streams()
#' calc_edges()
#' calc_sites()
#' binaries <- calc_binary()
#' ssn_dir <- file.path(tempdir(), 'nc.ssn')
#' export_ssn(ssn_dir, binary = binaries)
#' list.files(ssn_dir)
export_ssn <- function(path, binary){
  message('Exporting to ', path)
  # write edges
  execGRASS('v.out.ogr',
            c('overwrite', 'quiet'),
            parameters = list(
              input = 'edges',
              type = 'line',
              output = path,
              output_layer = 'edges'
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