#' Checking 'SSN' object.
#'
#' This function roughly checks the 'SSN' object. It returns FALSE if some 
#' essential columns are missing or values have illegal values.
#'
#' @importFrom rgdal readOGR
#' @import SSN
#'
#' @param path character; path to .ssn object.
#' @param predictions name(s) of prediction map(s) (optional).
#' 
#' @return TRUE or FALSE depending if checks pass.
#' @author Mira Kattwinkel, \email{mira.kattwinkel@@gmx.net},
#'   Eduard Szoecs, \email{eduardszoecs@@gmail.com}
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
#' setup_grass_environment(dem = dem_path)
#' import_data(dem = dem_path, sites = sites_path)
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
#' # Prepare site
#' calc_sites()
#' # Calculate H2OArea
#' calc_attributes_sites_exact()
#'
#' # Plot data
#' dem <- readRAST('dem', ignore.stderr = TRUE)
#' sites <- readVECT('sites', ignore.stderr = TRUE)
#' sites_orig <-  readVECT('sites_o', ignore.stderr = TRUE)
#' edges <- readVECT('edges', ignore.stderr = TRUE)
#' plot(dem, col = terrain.colors(20))
#' lines(edges, col = 'blue')
#' points(sites_orig, pch = 4)
#' cols <- colorRampPalette(c("blue", 'red'))(length(sites$H2OArea))[rank(sites$H2OArea)]
#' points(sites, pch = 16, col = cols)
#'
#' # Write data to SSN Folder
#' ssn_dir <- file.path(tempdir(), 'nc.ssn')
#' export_ssn(ssn_dir, delete_directory = TRUE)
#'
#' # Check if all files are ok
#' library(SSN)
#' check_ssn(ssn_dir)
#' }



check_ssn <- function(path, predictions = NULL) {
  out <- TRUE
  
  # neccesary Files ---------------------------------------------------------
  message("Checking necessary files...")
  if(file.exists(file.path(path, "edges.shp"))) {
    message("\tedges.shp...OK")
  } else {
    out <- out & FALSE
    message("\tedges.shp...FAIL!")
  }
  if(file.exists(file.path(path, "sites.shp"))) {
    message("\tsites.shp...OK")
  } else {
    out <- out & FALSE
    message("\tsites.shp...FAIL!")
  }
  if(!is.null(predictions)){
    for(i in 1:length(predictions)){
      if(file.exists(file.path(path, predictions[i], ".shp"))) {
        message(paste0("\t", predictions[i], ".shp...OK"))
      } else {
        out <- out & FALSE
        message(paste0("\t", predictions[i], ".shp...FAIL!"))
      }
    }
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
  
  if (max(edges$rid) >= nrow(edges) - 1) {
    message("\tMax rid...OK")
  } else {
    out <- out & FALSE
    message("\tMax rid...FAIL!")
  }
  
  # if (all(edges$upDist > 0)) {
  #   message("\tupDist > 0...OK")
  # } else {
  #   out <- out & FALSE
  #   message("\tupDist > 0...FAIL!")
  # }
  
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
  # columns
  
  # Obs. sites -------------------------------------------------------------------
  message("Checking sites.shp...")
  sites <- readOGR(path, "sites", verbose = FALSE)
  obl_cols <- c("rid", "pid", "locID", "netID", "upDist")
  obl_cols2 <- "H2OArea"
  if (all(obl_cols %in% names(sites@data)) & any(grepl(obl_cols2, names(sites@data)))) {
    message("\tColumns...OK")
  } else {
    out <- out & FALSE
    message("\tColumns...FAIL!
            \tMissing columns: ",  obl_cols[!obl_cols %in% names(sites@data)])
  }
  
  if(any(sites$ratio > 1) | any(sites$ratio < 0)) {
    out <- out & FALSE
    message("\tratio > 0 and < 1 ...FAIL!")
  } else {
    message("\tratio...OK")
  }
  
  if (!any(is.na(sites$netID))) {
    message("\tnetID...OK")
  } else {
    out <- out & FALSE
    message("\tnetID > 0...FAIL!")
  }
  if (!any(is.na(sites$rid))) {
    message("\trid...OK")
  } else {
    out <- out & FALSE
    message("\trid > 0...FAIL!")
  }
  
  
  # Prediction sites -------------------------------------------------------------------
  if(!is.null(predictions)){
    message("Checking Prediction sites...")
    for(i in 1:length(predictions)){
      preds <- readOGR(path, predictions[i], verbose = FALSE)
      obl_cols <- c("rid", "pid", "locID", "netID", "upDist", "H2OArea")
      if (all(obl_cols %in% names(preds@data))) {
        message("\tColumns...OK")
      } else {
        out <- out & FALSE
        message("\tColumns...FAIL!
            \tMissing columns: ",  obl_cols[!obl_cols %in% names(preds@data)])
      }
      
      if (any(preds$ratio > 1) | any (preds$ratio < 0)) {
        message("\tratio...OK")
      } else {
        out <- out & FALSE
        message("\tratio > 0 and < 1 ...FAIL!")
      }
      
      if (!any(is.na(preds$netID))) {
        message("\tnetID...OK")
      } else {
        out <- out & FALSE
        message("\tnetID > 0...FAIL!")
      }
      if (!any(is.na(preds$rid))) {
        message("\trid...OK")
      } else {
        out <- out & FALSE
        message("\trid > 0...FAIL!")
      }
    }
  }
  
  # Binary ids --------------------------------------------------------------
  message("Checking Binary files...")
  if (all(as.numeric(gsub("netID(.*)\\.dat", "\\1", bin_files)) %in% unique(edges@data$netID)) &
      all(unique(edges@data$netID) %in% as.numeric(gsub("netID(.*)\\.dat", "\\1", bin_files)))) {
    message("\tBinary files...OK")
  } else {
    out <- out & FALSE
    message("\tBinary files...FAIL!")
  }
  
  netids <- as.numeric(gsub("netID(.*)\\.dat", "\\1", bin_files))
  rid_ok <- TRUE
  for(i in netids){
    rids_bin <- utils::read.table(file.path(path, paste0("netID", i, ".dat")), header = T, sep = ",")[,1]
    rids_edge <- edges$rid[edges$netID == i]
    if(! all(rids_bin %in% rids_bin) & all(rids_edge %in% rids_bin)){
      rid_ok <- rid_ok & FALSE
    }
  }
  if(rid_ok){
    message("\trids in binary files...OK")
  } else {
   out <- out & FALSE
   message("\trids in binary files...FAIL!")
 }

# preds same in edges and sites
# per network id net*.dat same as rid
return(out)
}
