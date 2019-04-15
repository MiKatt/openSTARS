#' Calculate attributes of the sites.
#'
#' For each site (observations or predictions) attributes (potential predictor variables)
#' are derived based on the values calculated for the edge the site lies on.
#' This function calculates approximate values for site catchments as described
#' in Peterson & Ver Hoef, 2014: STARS: An ArcGIS Toolset Used to Calculate the
#' Spatial Information Needed to Fit Spatial Statistical Models to Stream
#' Network Data. J. Stat. Softw., 56 (2).
#'
#' @param sites_map character; name of the sites the attributes shall be
#'   calculated for. "sites" refers to the observation sites.
#' @param input_attr_name character vector; input column name in the edges
#'   attribute table.
#' @param output_attr_name character vector (optional); output column name
#'   appended to the site attribute data table. If not provided it is set to
#'   \code{input_attr_name}. Attribute names must not be longer than 10
#'   characters.
#' @param stat name or character vector giving the statistics to be calculated.
#'   See details below.
#' @param round_dig integer; number of digits to round results to.
#' @param calc_basin_area boolean; shall the catchment area be calculated? (Useful
#'  to set to FALSE if the function has been called before.)
#'
#' @return Nothing. The function appends new columns to the \code{sites_map}
#'   attribute table
#' \itemize{
#'  \item{'H2OAreaA':} {Total watershed area of the watershed upstream of each site.}
#'  \item{attr_name:} {Additional optional attributes calculated based on \code{input_attr_name}.}
#' }
#'
#' @details The approximate total catchment area (H2OAreaA) is always calculated
#' if \code{calc_basin_area} is TRUE. If \code{stat} is one of "min", "max", "mean" or "percent" the
#'   function assigns the value of the edge the site lies on. Otherwise, the
#'   value is calculated as the sum of all edges upstream of the previous
#'   junction and the proportional value of the edge the site lies on (based on
#'   the distance ratio 'ratio'); this is useful e.g. for counts of dams or waste water
#'   treatment plant or total catchment area.
#'
#' @note \code{\link{import_data}}, \code{\link{derive_streams}},
#'   \code{\link{calc_edges}}, \code{\link{calc_sites}} or
#'   \code{\link{calc_prediction_sites}} and \code{\link{calc_attributes_edges}}
#'   must be run before.
#'
#' @author Mira Kattwinkel, \email{mira.kattwinkel@@gmx.net}
#' @export
#' 
#' @examples 
#' \donttest{
#' # Initiate GRASS session
#' if(.Platform$OS.type == "windows"){
#'   gisbase = "c:/Program Files/GRASS GIS 7.4.0"
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
#' pred_path <- system.file("extdata", "nc", "geology.shp", package = "openSTARS")
#' setup_grass_environment(dem = dem_path)
#' import_data(dem = dem_path, sites = sites_path,
#'  predictor_vector = pred_path)
#' gmeta()
#'
#' # Derive streams from DEM
#' derive_streams(burn = 0, accum_threshold = 700, condition = TRUE, clean = TRUE)
#'
#' # Check and correct complex confluences (there are no complex confluences in this
#' # example date set; set accum_threshold in derive_streams to a smaller value
#' # to create complex confluences)
#' cj <- check_compl_confluences()
#' if(cj){
#'   correct_compl_confluences()
#' }
#' 
#' # Prepare edges
#' calc_edges()
#' 
#' # Derive slope from the DEM as an example raster map to calculate attributes from
#' execGRASS("r.slope.aspect", flags = c("overwrite","quiet"),
#' parameters = list(
#'   elevation = "dem",
#'     slope = "slope"
#'     ))
#' calc_attributes_edges(input_raster = "slope", stat_rast = "max", attr_name_rast = "maxSlo",
#'                      input_vector = "geology", stat_vect = "percent", attr_name_vect = "GEO_NAME")
#' 
#' calc_sites() 
#'  
#' # approximate potential predictor variables for each site based on edge values
#' calc_attributes_sites_approx(
#' input_attr_name = c('maxSlo', 'CZam', 'CZbg', 'CZfg', 'CZg', 'CZig', 'CZlg', 'CZve', 'Km'), 
#'   stat = c("max", rep("percent", 8)))
#' 
#' # plot share of a certain geology in the sampling point's catchment as
#' # point size
#' edges <- readVECT('edges', ignore.stderr = TRUE)
#' sites <- readVECT('sites', ignore.stderr = TRUE)
#' geo <- readVECT("geology", ignore.stderr = TRUE)#' 
#' plot(geo, col = adjustcolor(1:8, alpha.f = 0.5)[as.factor(geo$GEO_NAME)])
#' plot(edges, col = "blue", add = TRUE)
#' plot(sites, col = 1, add = TRUE, pch = 19, cex = (sites$CZbg + 0.15) * 2)
#' legend("left", col = adjustcolor(1:8, alpha.f = 0.5), bty = "n",
#' legend = unique(geo$GEO_NAME), pch = 15, title = "geology")
#' legend("right", col = 1, pch = 19, legend = seq(0, 1, 0.2), bty = "n",
#' title = "share CZbg\nin catchment", pt.cex =  (seq(0, 1, 0.2) + 0.15) * 2)
#'}

calc_attributes_sites_approx <- function(sites_map = "sites",
                                         input_attr_name = NULL,
                                         output_attr_name = NULL,
                                         stat = NULL,
                                         round_dig = 2,
                                         calc_basin_area = TRUE){

  if(is.null(output_attr_name))
    output_attr_name <- input_attr_name
  if(calc_basin_area == TRUE){
    output_attr_name <- c("H2OAreaA", output_attr_name)
    input_attr_name <- c("H2OArea", input_attr_name)
    stat <- c("totalArea", stat)
    
    cnames <- execGRASS("db.columns", 
                        parameters = list(
                          table = sites_map), 
                        intern = T)
    if("H2OAreaA" %in% cnames){
      execGRASS("v.db.dropcolumn", flags = "quiet",
                parameters = list(
                  map = sites_map,
                  columns = "H2OAreaA"
                ))
    }
  }
  if(calc_basin_area == FALSE & is.null(input_attr_name)){
    stop("Either input attribute name(s) must be given or calc_basin_area must be TRUE.")
  }

  if(length(input_attr_name) != length(output_attr_name))
    stop("There must be the same number of input and output attribute names.")

  if(any(nchar(output_attr_name)) > 10)
    stop("Attribute names must not be longer than ten characters.")

   if(length(round_dig) == 1)
    round_dig <- rep(round_dig, length(output_attr_name))

  if(length(round_dig) < length(output_attr_name))
    round_dig <- c(max(round_dig), round_dig)

  if(length(unique(c(length(input_attr_name), length(stat),length(output_attr_name)))) > 1)
    stop(paste0("There must be the same number of input attribute names (",length(input_attr_name), "),
                output attribute names (", length(output_attr_name), ") and
                statistics to calculate  (", length(stat),")."))

  execGRASS("v.db.addcolumn",
            flags = c("quiet"),
            parameters = list(
              map = sites_map,
              columns = paste0(output_attr_name," double precision", collapse = ", ")
            ))

  for(i in seq_along(input_attr_name)){
    if(stat[i] %in% c("min", "max", "mean", "percent")){
      execGRASS("db.execute",
                parameters = list(
                  sql = paste0("UPDATE ", sites_map," SET ", output_attr_name[i], "=",
                               "(SELECT ", paste0(input_attr_name[i],"_c"),
                               " FROM edges WHERE edges.stream=", sites_map,".str_edge)")
                ))
    } else {
      # calculate site attribute as attribute of the two previous edges +
      # (1-ratio) * contribution of edge to total edge attribute
      # for H2O Area or e.g. for total numbers (no of WWTP per catchment)
      # e.g. calculated with stat = "sum" in calc_attributes_edges
      stream_prev1 <-  paste0("(SELECT prev_str01 FROM edges WHERE edges.stream=",sites_map,".str_edge)")
      stream_prev2 <-  paste0("(SELECT prev_str02 FROM edges WHERE edges.stream=",sites_map,".str_edge)")
      if(input_attr_name[i] == "H2OArea"){
        sql_str <-paste0("UPDATE ", sites_map," SET ",output_attr_name[i],
                         " = ROUND(((1-ratio)*",
                         "(SELECT rcaArea FROM edges WHERE ", sites_map,".str_edge = edges.stream) +",
                         "(SELECT H2OArea FROM edges WHERE edges.stream=",stream_prev1,") +",
                         "(SELECT H2OArea FROM edges WHERE edges.stream=",stream_prev2,")),",round_dig[i],")")
      } else {
        sql_str <-paste0("UPDATE ", sites_map," SET ",output_attr_name[i],
                         " = ROUND(((1-ratio)*",
                         "(SELECT ", paste0(input_attr_name[i],"_e"), " FROM edges WHERE ", sites_map,".str_edge = edges.stream) +",
                         "(SELECT ", paste0(input_attr_name[i],"_c"), " FROM edges WHERE edges.stream=",stream_prev1,") +",
                         "(SELECT ", paste0(input_attr_name[i],"_c"), " FROM edges WHERE edges.stream=",stream_prev2,")),",round_dig[i],")")
      }
      execGRASS("db.execute",
                parameters = list(
                  sql = sql_str
                ))
      # correct for those segments that do not have previous streams
      if(input_attr_name[i] == "H2OArea"){
        sql_str <- paste0("UPDATE ", sites_map," SET ",output_attr_name[i],
                          " = (1-ratio)*(SELECT rcaArea FROM edges WHERE ",
                          sites_map,".str_edge = edges.stream) WHERE str_edge IN ",
                          "(SELECT stream FROM edges WHERE prev_str01=0)")
      } else {
        sql_str <- paste0("UPDATE ", sites_map," SET ",output_attr_name[i],
                          " = (1-ratio)*(SELECT ", paste0(input_attr_name[i],"_e"),
                          " FROM edges WHERE ", sites_map,".str_edge = edges.stream) WHERE str_edge IN ",
                          "(SELECT stream FROM edges WHERE prev_str01=0)")
      }
      execGRASS("db.execute",
                parameters = list(
                  sql = sql_str
                ))
      # ROUND does not work with WHERE ... IN ...
      execGRASS("db.execute",
                parameters = list(
                  sql = paste0("UPDATE ",sites_map, " SET ",output_attr_name[i],
                               "= ROUND(",output_attr_name[i],",",round_dig[i],")")
                ))
    }
  }
  cnames2 <- execGRASS("db.columns", flags = "quiet",
                             parameters = list(
                               table = sites_map
                             ), intern = T)
  cnames2 <- cnames2[-(which(cnames2 %in% cnames))]
  message(writeLines(strwrap(paste0("\nNew attributes values are stored as ", paste("'", cnames2, "'", sep = "", collapse = ", "), " in 'sites'."),
          width = 80)))
}
