#' Create prediction sites for .ssn object
#' @importFrom sp spsample
#' @importFrom rgdal writeOGR
#' @param path character; path to .ssn folder
#' @param name character; name of prediction shapefile
#' @param n numeric; (approximate) sample size
#' @param type character; "random" for completely spatial random;
#' "regular" for regular (systematically aligned) sampling;
#' @param export logical; should the prediction sites also be exported to the .ssn folder?
#' @return SpatialPointsDataFrame with prediction sites
#' @seealso \code{\link[sp]{spsample}}
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
#' @export
create_pred <- function(path, name, n = 1000, type = c('random', 'regular'),
                        export = TRUE, ...) {
  type <- match.arg(type)
  edges <- readOGR(path, 'edges', verbose = FALSE)
  sites <- readOGR(path, 'sites', verbose = FALSE)
  out <- spsample(edges, n, type, ...)
  # change data=... to pred variables here

  # add locID (other then already in sites)
  locID <- seq(max(sites$locID + 1), max(sites$locID + 1) + length(out))
  # pid same as locID
  pid <- locID

  # rid and netID and upDist must be added
  #! maybe import to GRASS

  out <- SpatialPointsDataFrame(out, data = data.frame(dummy = seq_len(length(out))))
  # add attributes
  # ...
  if (export) {
    writeOGR(out, path, name, driver = "ESRI Shapefile")
  }
  return(out)
}