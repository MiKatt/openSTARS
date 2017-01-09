#' Calcuate attributes of the sites.
#'
#' For each site (observations or predictions) attributes (predictor variables)
#' are derived based on the values caluclated for the edge the site lies on.
#' This fuction calculates approximate values for site catchments as described
#' in Peterson & Ver Hoef, 2014: STARS: An ARCGIS Toolset Used to Calculate the
#' Spatial Information Needed to Fit Spatial Statistical Models to Stream
#' Network Data. J. Stat. Softw., 56 (2).
#'
#' @param sites_map character; name of the sites the attributes shall be
#' calculated for. "sites" refers to the observation or prediction sites.
#' @param input_attr_name character vector; input column name in the edges
#'   attribute data table.
#' @param output_attr_name character vector (optional); output column name appended
#'  to the site attribute data table. If not provided it is set to
#'  \code{input_attr_name}. Attribute names must not be longer than 10 characters.
#' @param stat name or character vector giving the statistics to be calulated. If
#' it is one of min, max, mean or percent the function returns the same value for
#' the site as for the edge it lies on. Otherwise, it returns the sum of all
#' catchments upstream of the last junction upstream of the site, and a proportional
#' value of the edge it lies on based on distRatio.
#' @param round_dig integer; number of digits to round results to.
#'
#' @return Nothing. The function appends new columns to the \code{sites_map}
#'  attribute table
#'
#' @details If \code{stat} is one of "min", "max", "mean" or "percent" simply the
#' value of the edge is assigned. Otherwise, the value is calculated as the sum
#' of the upstream catchments and the proportional value of the edge the site lies
#' on (based on distRatio); this is usefull e.g. for counts of dams or waste
#' water treatment plant.
#'
#' @note \code{\link{import_data}}, \code{\link{derive_streams}},
#'   \code{\link{calc_edges}}, \code{\link{calc_sites}} or
#'   \code{\link{calc_prediction_sites}} and
#'   \code{\link{calc_attributes_edges}} must be run before.

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
#' calc_sites_attributes(input_raster = "slope",  stat = "mean", attr_name = "avgSlopeP")
#' }

calc_attributes_sites_approx <- function(sites_map = "sites",
                                         input_attr_name,
                                         output_attr_name = NULL,
                                         stat,
                                         round_dig = 2){
  if(is.null(output_attr_name))
    output_attr_name <- input_attr_name

  if(length(round_dig) == 1)
    round_dig <- rep(round_dig, length(output_attr_name))

  if(length(input_attr_name) != length(output_attr_name))
    stop("There must be the same number of input and output attribute names.")

  if(length(input_attr_name) != length(stat) | length(input_attr_name) != length(output_attr_name) | length(output_attr_name) != length(stat))
    stop(paste0("There must be the same number of input input attribute names (",length(input_attr_name), "),
                output attribute names (", length(output_attr_name), ") and
                statistics to calculate  (", length(stat),")."))

  execGRASS("v.db.addcolumn",
            flags = c('quiet'),
            parameters = list(
              map = sites_map,
              columns = paste0(output_attr_name,' double precision', collapse = ", ")
            ))

  for(i in seq_along(input_attr_name)){
    if(stat[i] %in% c("min", "max", "mean", "percent")){
      execGRASS('db.execute',
                parameters = list(
                  sql = paste0("UPDATE ", sites_map," SET ", output_attr_name[i], "=",
                               "(SELECT ", paste0(input_attr_name[i],"_c"),
                               " FROM edges WHERE edges.cat=", sites_map,".cat_edge)")
                ))
    } else {
      # calculate site attribute as attribute of the two previous edges +
      # (1-distRatio) * contribution of edge to total edge attribute
      # Usefull e.g. for total numbers (no of WWTP per catchment) or total area
      ecat_prev1 <-  paste0("(SELECT cat FROM edges WHERE edges.stream=(SELECT prev_str01 FROM edges WHERE edges.cat=",sites_map,".cat_edge))")
      ecat_prev2 <-  paste0("(SELECT cat FROM edges WHERE edges.stream=(SELECT prev_str02 FROM edges WHERE edges.cat=",sites_map,".cat_edge))")
      sql_str <-  paste0("UPDATE ", sites_map, " SET ", output_attr_name[i], "=ROUND(",
                         "1-(SELECT distRatio FROM ",sites_map,")*(",
                         "(SELECT ", paste0(input_attr_name[i],"_e"), " FROM edges WHERE edges.cat=",sites_map,".cat_edge))+",
                         "(SELECT ", paste0(input_attr_name[i],"_c"), " FROM edges WHERE edges.cat=",ecat_prev1,")+",
                         "(SELECT ", paste0(input_attr_name[i],"_c"), " FROM edges WHERE edges.cat=",ecat_prev2, "),",
                         round_dig[i], ")")
      execGRASS('db.execute',
                parameters = list(
                  sql = sql_str
                ))
      # correct for those segments that do not have previous streams
      sql_str <-  paste0("UPDATE ", sites_map," SET ",output_attr_name[i],"=ROUND(",
                         "1-(SELECT distRatio FROM ",sites_map,")*(",
                         "(SELECT ", paste0(input_attr_name[i],"_e"), " FROM edges WHERE edges.cat=",sites_map,".cat_edge AND edges.prev_str01=0)",
                         "),", round_dig[i],")")
      execGRASS('db.execute',
                parameters = list(
                  sql = sql_str
                ))
    }
  }
}
