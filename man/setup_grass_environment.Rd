% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setup_grass_environment.R
\name{setup_grass_environment}
\alias{setup_grass_environment}
\title{Setup 'GRASS' environment.}
\usage{
setup_grass_environment(dem, gisBase, epsg = NULL, sites = NULL, ...)
}
\arguments{
\item{dem}{character; path to DEM.}

\item{gisBase}{character; the directory path to GRASS binaries and libraries, containing 
bin and lib subdirectories among others (see details).}

\item{epsg}{integer (deprecated); not used any more. Only included for compatibility with previous version.}

\item{sites}{(deprecated); not used any more. Only included for compatibility with previous version.}

\item{...}{Optional arguments to be passed to \code{\link[rgrass7]{initGRASS}} (see details).}
}
\value{
Nothing. A GRASS session is initiated and the 'GRASS' mapset is set to PERMANENT. 
 The geographical projection, geographical extension, number of columns and number of 
 rows for the data and the resolution are defined by the dem. They are stored the DEFAULT_WIND file.
}
\description{
This function sets the 'GRASS' mapset to PERMANENT and sets its projection and extension.
}
\details{
A GRASS session is initiated using \code{\link[rgrass7]{initGRASS}}. The path to 
  the GRASS program must be provided as \code{gisBase}. For Linux, this might look like 
  "/usr/lib/grass78/" and for Windows "c:/Program Files/GRASS GIS 7.8".
  Optional arguments are for example 
  * \code{gisDbase}: the GRASS GISBASE directory for this session; defaults to tempdir()
  * \code{location}: name of the location for this session; defaults to tempfile()
  * \code{override}: TRUE for allowing to override an existing location.
}
\note{
It is no longer required to initiate a GRASS session before using \code{\link[rgrass7]{initGRASS}}!
}
\examples{
\donttest{
# path to GRASS
if(.Platform$OS.type == "windows"){
  gisbase = "c:/Program Files/GRASS GIS 7.6"
  } else {
  gisbase = "/usr/lib/grass78/"
  }
# path to the dem   
dem_path <- system.file("extdata", "nc", "elev_ned_30m.tif", package = "openSTARS")
setup_grass_environment(dem = dem_path, 
                        gisBase = gisbase, 
                        location = "nc_example_location",
                        override = TRUE)
gmeta()
}
}
\author{
Mira Kattwinkel, \email{mira.kattwinkel@gmx.net}
}
