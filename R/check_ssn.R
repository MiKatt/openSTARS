#' Checking SSN object.
#'
#' This function roughly checks
#'
#' @importFrom rgdal readOGR
#' @import SSN
#'
#' @param path character; path to .ssn object.

#' @return TRUE or FALSE depending if checks pass.
#' @author Eduard Szoecs, \email{eduardszoecs@@gmail.com}
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
#'
#' # Check if all files are ok
#' library(SSN)
#' check_ssn(ssn_dir)
#' }

# # Windows:
# initGRASS(gisBase = "c:/Program Files/GRASS GIS 7.0.3", #"/usr/lib/grass70/",
#           home = tempdir(), #"f:/Landau/02_openSTARS/openSTARS-master_MK/Grass/test",
#           gisDbase = "f:/Landau/02_openSTARS/Grass/GRASSDB",
#           location = "testdata_Edi2",
#           remove_GISRC = T,
#           override = TRUE)
#
# dem_path <-  "f:/Landau/02_openSTARS/openSTARS-master_MK/inst/extdata/nc/elev_ned_30m.tif"
# sites_path <- "f:/Landau/02_openSTARS/openSTARS-master_MK/inst/extdata/nc/sites_nc.shp"
# streams = NULL
#
# import_data(dem = dem_path, sites = sites_path)
# dem <- readRAST('dem')
# plot(dem)
#
# derive_streams(burn=5, at=700, condition=TRUE, clean = TRUE)
# calc_edges()
# streams<-readVECT('edges')
# plot(streams,add=T,col="green")
#
# calc_sites()
# streams<-readVECT('sites')
# plot(streams,add=T,col="red")
# streams<-readVECT('sites_o')
# plot(streams,add=T,col="yellow")
#
# binaries <- calc_binary()
# ssn_dir <- file.path(tempdir(), 'nc.ssn')
# export_ssn(ssn_dir, binary = binaries)
# require('rgdal')
# require('SSN')
# check_ssn(ssn_dir)


check_ssn <- function(path) {
  out <- TRUE

# neccesary Files ---------------------------------------------------------
  message("Checking necessary files...")
  if (file.exists(file.path(path, "edges.shp"))) {
    message("\tedges.shp...OK")
  } else {
    out <- out & FALSE
    message("\tedges.shp...FAIL!")
  }
  if (file.exists(file.path(path, "sites.shp"))) {
    message("\tsites.shp...OK")
  } else {
    out <- out & FALSE
    message("\tsites.shp...FAIL!")
  }
  edges <- readOGR(path, "edges", verbose = FALSE)
  netIDs <- unique(edges$netID)
  bin_files <- list.files(path, pattern = "*.dat")
  if (all(bin_files %in% paste0("netID", netIDs, ".dat"))) {
    message("\tbinary files...OK")
  } else {
    out <- out & FALSE
    message("\tbinary files...FAIL!")
  }

# edges -------------------------------------------------------------------
  message("Checking edges.shp...")
  obl_cols <- c("rid", "netID", "OBJECTID", "upDist", "Length", "H2OArea", "rcaArea")
  if (all(obl_cols %in% names(edges@data))) {
    message("\tColumns...OK")
  } else {
    out <- out & FALSE
    message("\tColumns...FAIL!
            \tMissing columns: ",  obl_cols[!obl_cols %in% names(edges@data)])
  }
  if (length(unique(edges$rid)) == nrow(edges)) {
    message("\tUnique rids...OK")
  } else {
    out <- out & FALSE
    message("\tUnique rids...FAIL!")
  }

  if (max(edges$rid) == nrow(edges) - 1) {
    message("\tMax rid...OK")
  } else {
    out <- out & FALSE
    message("\tMax rid...FAIL!")
  }

  if (all(edges$upDist > 0)) {
    message("\tupDist > 0...OK")
  } else {
    out <- out & FALSE
    message("\tupDist > 0...FAIL!")
  }

  if (!any(is.na(edges$netID))) {
    message("\tnetID...OK")
  } else {
    out <- out & FALSE
    message("\tnetID > 0...FAIL!")
  }

  ssn <- importSSN(path)
  ssn <- additive.function(ssn, "H2OArea", "afv_computed")
  r_afv <- range(ssn@data$afv_computed)

  if (all(r_afv >= 0 & r_afv <= 1)) {
    message("\tadditive function value range...OK")
  } else {
    out <- out & FALSE
    message("\tadditive function value range...FAIL!")
  }

  #message("\tsegment PI value range...not implemented!")


  # ggplot(edges@data, aes(x = topo_dim, y = upDist-Length)) +
  #   geom_point() +
  #   facet_wrap(~netID)


  # columns

# Obs. sites -------------------------------------------------------------------
  #message("Checking sites.shp...")



# Prediction sites -------------------------------------------------------------------
  #message("Checking Prediction sites...")



# Binary ids --------------------------------------------------------------
  message("Checking Binary files...")
  if (all(as.numeric(gsub("netID(.*)\\.dat", "\\1", bin_files)) %in% unique(edges@data$netID))) {
    message("\tBinary files...OK")
  } else {
    out <- out & FALSE
    message("\tBinary files...FAIL!")
  }



  # preds same in edges and sites
  # per network id net*.dat same as rid
  return(out)
}
