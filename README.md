---
output:
  html_document:
    keep_md: yes
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

openSTARS
=============



## Introduction
`openSTARS` is an open source implementation of the STARS toolbox (Peterson & Ver Hoef, 2014) using R and GRASS GIS.
It prepares the .ssn object needed for the SSN package.
A digital elevation model (DEM) is used to derive stream networks (in contrast to STARS that can clean an existing stream network). The reason for this is that existing stream networks (e.g. obtained as shape files) very often contain loops and dead ends that hinder building a valid topology for them.

For more information on STARS and SSN, see [their web page](http://www.fs.fed.us/rm/boise/AWAE/projects/SpatialStreamNetworks.shtml).

Peterson, E. E., & Ver Hoef, J. M. (2014). STARS: An ArcGIS Toolset Used to Calculate the Spatial Information Needed to Fit Spatial Statistical Models to Stream Network Data. J Stat Softw, 56(2), 1–17.

## Installation and loading
A functional installation of [GRASS GIS (>=7.0)](https://grass.osgeo.org/#) with installed add-ons [r.stream.basins](https://grass.osgeo.org/grass74/manuals/addons/r.stream.basins.html), [r.stream.distance](https://grass.osgeo.org/grass74/manuals/addons/r.stream.distance.html), [r.stream.order](https://grass.osgeo.org/grass74/manuals/addons/r.stream.order.html) and 
[r.hydrodem](https://grass.osgeo.org/grass74/manuals/addons/r.hydrodem.html) is needed.
These add-ons can be installed from within GRASS using the console and g.extension or in the GUI under 'Settings'/'Addons extensions'/'Install extensions from add-ons' under 'raster'.

Installation from CRAN repository:

```r
install.packages("openSTARS")
library("openSTARS")
```

For the lastest github version of openSTARS (carefull, might be experimental):

```r
# install.packages("devtools")
devtools::install_github("MiKatt/openSTARS")
library("openSTARS")
```

## Step by step usage

### Initiate an ephemeral GRASS session
First, a GRASS session must be initiated:

```r
library(openSTARS)
initGRASS(gisBase = "/usr/lib/grass74/",
          home = tempdir(),
          override = TRUE)
```

Alternatively, the path to a specific GRASS database directory and a Location name can be provided.

```r
library(openSTARS)
initGRASS(gisBase = "/usr/lib/grass74/",
          home = tempdir(),
          gisDbase = file.path(tempdir(),"GRASSDB"),
          location = "test_openSTARS",
          remove_GISRC = T)
```

On Windows, this might look like this:

```r
library(openSTARS)
initGRASS(gisBase = "c:/Program Files/GRASS GIS 7.4.0", 
          home = tempdir(),
          location = "test_openSTARS",
          remove_GISRC = T)
#> gisdbase    C:/Users/Mira_2/AppData/Local/Temp/RtmpQhlYuA 
#> location    test_openSTARS 
#> mapset      file34386a953400 
#> rows        1 
#> columns     1 
#> north       1 
#> south       0 
#> west        0 
#> east        1 
#> nsres       1 
#> ewres       1 
#> projection  NA
```


### Setup GRASS and load data into GRASS
The path to the digital elevation model (DEM) and the observation sites must be
provided. Additionally, the path to a stream network, which can be burnt into the
DEM before extracting the streams, can be given.

First, `setup_grass_environment` prepares the GRASS environment by setting

 * the projection to that one of the observation sites or to an epsg code provided
 * the region to the extent of the DEM.
 
For more information on the concept of GRASS Locations, Mapsets etc. see the [GRASS GIS Quickstart](https://grass.osgeo.org/grass74/manuals/helptext.html).


```r
dem_path <- system.file("extdata", "nc", "elev_ned_30m.tif", package = "openSTARS")

setup_grass_environment(dem = dem_path)
#> Setting up GRASS Environment ...

gmeta()
#> gisdbase    C:/Users/Mira_2/AppData/Local/Temp/RtmpQhlYuA 
#> location    test_openSTARS 
#> mapset      PERMANENT 
#> rows        450 
#> columns     500 
#> north       228500 
#> south       215000 
#> west        630000 
#> east        645000 
#> nsres       30 
#> ewres       30 
#> projection  +proj=lcc +lat_1=36.16666666666666 +lat_2=34.33333333333334
#> +lat_0=33.75 +lon_0=-79 +x_0=609601.22 +y_0=0 +no_defs +a=6378137
#> +rf=298.257222101 +towgs84=0.000,0.000,0.000 +to_meter=1
```

Then, use `import_data` to import all data into GRASS (DEM, observation sites and other optional data). Optional data includes a stream network to burn into the DEM (see `derive_streams`), prediction sites if they have been already created with a different program (alternatively, prediction sites can be created using `calc_prediction_sites`), and raster and vector maps of potential predictor variables for the SSN model that can later be intersected with the catchments of the sites (`calc_attributes_edges` and `calc_attributes_sites_approx`, or `calc_attributes_sites_exact`).


```r
sites_path <- system.file("extdata", "nc", "sites_nc.shp", package = "openSTARS")
preds_path <- c(system.file("extdata", "nc", "landuse.shp", package = "openSTARS"),
                 system.file("extdata", "nc", "pointsources.shp", package = "openSTARS"))
import_data(dem = dem_path, sites = sites_path, predictor_vector = preds_path, predictor_v_names = c("landuse", "psources"))
#> Loading DEM into GRASS as dem ...
#> Loading sites into GRASS as sites_o ...
#> Loading predictior varibales into GRASS as landuse, psources ...
#> No streams available, skipping.
```

The DEM is loaded into the GRASS database as a raster map named `dem`, the sites as a vector map named `sites_o` and the (optional) stream network as a vector map named `streams_o`. Predictor sites are stored under their base file name, potential predictors either using their base file names or the ones provided in 'predictor_r_names' and 'predictor_v_names', respectively.

The data looks like this:


```r
dem <- readRAST("dem", ignore.stderr = TRUE)
sites <- readVECT("sites_o", ignore.stderr = TRUE)
psources <- readVECT("psources", ignore.stderr = TRUE)
lu <- readVECT("landuse", ignore.stderr = TRUE)
plot(dem, col = gray(seq(0,1,length.out=20)))
col <- adjustcolor(c("red", "green", "blue", "yellow"), alpha.f = 0.3)
plot(lu, add = T, col = col[as.numeric(as.factor(lu$landuse))])
legend(x = par("usr")[1]*1.001, y = par("usr")[4]*0.999, col = col, pch = 15, legend = as.factor(sort(unique(lu$landuse))), 
       title = "landuse", ncol = 4)
cols <- colorRampPalette(c("blue", "red"))(length(sites$value))[rank(sites$value)]
points(sites, pch = 16, col = cols)
points(psources, pch = 19, col = 1, cex = 1.7)
legend(x = par("usr")[2]*0.991, y = par("usr")[4]*0.999, pch = c(16, 16, 19), ncol = 1, col = c(range(cols),1), legend = c(paste("value at sites:", c(range(sites$value))), "point sources"))
```

![](README_files/figure-html/plot_data1-1.png)<!-- -->

### Derive streams from DEM
Next, the streams must be derived from the DEM.

```r
derive_streams()
#> Conditioning DEM ...
#> Deriving streams from DEM ...
#> Calculating stream topology ...
```
An existing stream network (if provided to `import_data` before) can be burnt into the DEM to force the streams derived from the DEM to mapped ones. It is not possible to use a given stream network directly but it has to be derived from the DEM because otherwise it lacks topological information needed in the consecutive steps. Additional specifications on how the streams shall be created can be provided (see `?derive_streams` and the GRASS function [r.stream.extract](https://grass.osgeo.org/grass74/manuals/r.stream.extract.html) for details).


```r
derive_streams(burn = 10, accum_threshold = 1000)
```


```r
dem <- readRAST("dem", ignore.stderr = TRUE)
streams <- readVECT("streams_v", ignore.stderr = TRUE)
plot(dem, col = terrain.colors(20))
lines(streams, col = "blue")
cols <- colorRampPalette(c("blue", "red"))(length(sites$value))[rank(sites$value)]
points(sites, pch = 16, col = cols)
```

![](README_files/figure-html/plot_data2-1.png)<!-- -->

### Check and correct the network
Next, the stream network should be checked if there are stream segments with more than two inflows. These must be corrected because the .ssn object must not have such complex junctions. In the nc data set provided, there will be complex junctions only if accum_threshold is small (e.g. 150; note that this will take long to run and use a lot of memory).

<!-- uses too much disk space and takes ages

```r
derive_streams(burn = 0, accum_threshold = 100)
cp <- check_compl_junctions()
if (cp)
  correct_compl_junctions()
```


```r
dem <- readRAST('dem', ignore.stderr = TRUE)
streams <- readVECT('streams_v', ignore.stderr = TRUE)
streams_orig <- readVECT('streams_v_o', ignore.stderr = TRUE)
# zoom to a relevant part of the dem
plot(dem, col = terrain.colors(20), axes = TRUE,
  xlim = c(640050,640200), ylim = c(219700,219850))
lines(streams_orig, col = 'red', lwd = 4)
lines(streams, col = 'blue', lty = 2, lwd = 2)
legend("topright", col = c("red", "blue"), lty = c(1,2), lwd = c(4,2),
  legend = c("original", "corrected"))
```
-->

An example of a complex junction and the correction would look like this:

![Original network with complex junction. Arrows indicate flow direction.](README_files/compl_junction1.PNG)  ![Corrected network. Arrows indicate flow direction.](README_files/compl_junction2.PNG)

*Left* Original network with complex junction (i.e. three inflows to one outflow). 
*Right* Corrected network. Arrows indicate flow direction.

The end node of the inflowing segment with the smallest angle to the outflowing 
segment is moved 0.25 times the cell size of the DEM downstream. The outflowing
segment is split into tow parts at this new junction. All features are corrected
accordingly (cat, length, prev_str01, prev_str02, next_str etc.). Currently, this 
only works for three inflows to the same outflow but not more.

Other topological errors as mentioned for the ArcGIS toolbox STARS do not occur
if the stream network is derived from a DEM.

### Prepare edges
Now, information needed for the .ssn object can be derived for the streams and stored in a new vector map `edges`.


```r
calc_edges()
```


```r
edges <- readVECT("edges", ignore.stderr = TRUE)
head(edges@data, n = 4)
#>   cat stream prev_str01 prev_str02 next_str flow_accu netID rid OBJECTID
#> 1   1      1          0          0       30  903.7007    15   0        1
#> 2   2      2          0          0       15 1893.0472    15   1        2
#> 3   3      3          0          0       51 6130.5509    14   2        3
#> 4   4      4          0          0       -1 3123.5889     1   3        4
#>    Length sourceDist   upDist H2OArea rcaArea
#> 1 1268.53    1268.53 19128.73  1.0674  1.0674
#> 2 1006.69    1006.69 21908.81  1.3473  1.3473
#> 3 4298.01    4298.01 13038.00  5.5233  5.5233
#> 4 2657.06    2657.06  2657.06  2.8305  2.8305
```

`edges` now holds the derived network plus attributes needed for the .ssn object

* network identifier (netID)
* reach identifier (rid)
* stream segment length (length)
* distance from the source (sourceDist)
* upstream distance, i.e. distance from the outlet of the network to the start (upstream node) of the stream segment (upDist)
* total catchment area (H2OArea)
* reach contributing area (rcaArea)

The additional fields hold information about the network: 'next_str' is the 'stream' this segment flows into, 'prev_str01' and 'prev_str02' are the two segments that flow into this segment.

### Prepare sites
Often, survey sites do not lay exactly on the stream network (due to GPS imprecision, stream representation as lines, derivation of streams from a DEM, etc.). To assign an exact position of the sites on the network they are moved to the closest stream segment (snapped) using the GRASS function
[v.distance](https://grass.osgeo.org/grass72/manuals/v.distance.html). Additionally, attributes needed for .ssn object are assigned: 


```r
calc_sites()
```


```r
sites <- readVECT("sites", ignore.stderr = TRUE)
head(sites@data, n = 4)
#>   cat cat_ value cat_edge     dist       xm       ym locID pid netID rid
#> 1   1    1     1        5 79.90783 631046.1 226074.1     1   1    15   4
#> 2   2    2     1        5 61.15861 632011.9 225175.7     2   2    15   4
#> 3   3    3     1        2 72.04108 631203.4 224771.5     3   3    15   1
#> 4   4    4     1        2 31.22663 631787.3 224883.8     4   4    15   1
#>     upDist  distalong     ratio
#> 1 22490.34  289.70563 0.8457322
#> 2 21166.96 1613.08658 0.1410340
#> 3 21827.61   81.19927 0.9193403
#> 4 21104.67  804.14221 0.2012018
```

* point identifier (pid)
* location identifier (locID) 
* network identifier (netID)
* reach identifier of the edge segment the point lies on (rid)
* upstream distance (upDist), i.e. the distance to the network outlet calculated using [r.stream.distance](https://grass.osgeo.org/grass70/manuals/addons/r.stream.distance.html).
* distance ratio, i.e. the ratio of the distance from the outflow of the edge to the point along the edge and the total length of the edge segment (distRatio).

Additional fields hold information on the snapping: distance of the original site to the closest edge ('dist'), i.e. how far the point was moved, and the new x and y coordinates ('xm', 'ym'). The filed 'cat_edge' gives the 'cat' of the stream segment the point lies on. It is used to identify the edge the point lies on to extract the 'rid'.


```r
dem <- readRAST("dem", ignore.stderr = TRUE)
sites <- readVECT("sites", ignore.stderr = TRUE)
sites_orig <- readVECT("sites_o", ignore.stderr = TRUE)
edges <- readVECT("edges", ignore.stderr = TRUE)
plot(dem, col = terrain.colors(20))
lines(edges, col = "blue")
points(sites_orig, pch = 20, col = "black")
points(sites, pch = 21, cex=0.75, bg = "grey")
legend(x = par("usr")[1]*1.002, y = par("usr")[3]*1.01, col = 1, pt.bg = "grey", pch = c(21, 19), legend = c("snapped sites", "original sites"))
```

![](README_files/figure-html/plot_data3-1.png)<!-- -->

### Prepare prediction sites
Prediction sites can be created along the streams. Either the distance between the sites must be provided (`dist`) or the approximate number of sites that shall be created (`nsites`). Additionally, the creation can be restricted to certain networks (`netIDs`). The sites will be assigned regularly on the stream network. If prediction sites with specifec coordinates are needed, they should be created manually.

Similar as for the observation sites, attributes needed for .ssn object are assigned: 

* point identifier (pid)
* location identifier (locID) 
* network identifier (netID)
* reach identifier of the edge segment the point lies on (rid)
* upstream distance (upDist), i.e. the distance to the network outlet calculated using [r.stream.distance](https://grass.osgeo.org/grass70/manuals/addons/r.stream.distance.html).
* distance ratio, i.e. the ratio of the distance from the outflow of the edge to the point along the edge and the total length of the edge segment (distRatio).

The filed 'cat_edge' gives the 'cat' of the stream segment the point lies on (equivalent to 'rid').


```r
calc_prediction_sites(predictions = "preds", nsites = 100, netIDs = 15 )
```


```r
dem <- readRAST("dem", ignore.stderr = TRUE)
sites <- readVECT("sites", ignore.stderr = TRUE)
pred_sites <- readVECT("preds", ignore.stderr = TRUE)
edges <- readVECT("edges", ignore.stderr = TRUE)
plot(dem, col = terrain.colors(20))
lines(edges, col = "blue")
points(sites, pch = 21, cex=0.75, bg = "grey")
points(pred_sites, pch = 21, cex=0.75, bg = "royalblue")
legend(x = par("usr")[1]*1.002, y = par("usr")[3]*1.01, pt.bg = c("grey","royalblue"), pch = 21, legend = c("(snapped) observation sites","prediction sites"))
```

![](README_files/figure-html/plot_data4-1.png)<!-- -->

```r
head(pred_sites@data, n = 5)
#>   cat cat_edge         dist pid rid distalong      ratio locID netID
#> 1   1        1 0.000000e+00   1   0       676 0.46709971     1    15
#> 2   2        1 2.910383e-11   2   0        17 0.98659866     2    15
#> 3   3        2 0.000000e+00   3   1       820 0.18544934     3    15
#> 4   4        2 0.000000e+00   4   1       161 0.84006993     4    15
#> 5   5        5 2.910383e-11   5   4      1692 0.09901275     5    15
#>     upDist
#> 1 18452.73
#> 2 19111.73
#> 3 21088.81
#> 4 21747.81
#> 5 21088.05
```

### Calculate attributes from raster and vector maps
Attributes (i.e. predictor variables for the .ssn object) can be calculated for observation and prediction sites. There are two ways to calculates attributes: 

1. approximately as described in Peterson & Ver Hoef, 2014: STARS: An ARCGIS Toolset Used to Calculate the Spatial Information Needed to Fit Spatial Statistical Models to Stream Network Data. J. Stat. Softw., 56 (2).
2. exactly by intersecting the catchment of each point with raster maps;

For the approximate calculation, first attributes must be intersected with the sub-catchments of the stream segments and then they are assigned to each site based on the distance ratio of the point. Note that the sub-catchment area 'H2OArea' for each stream segment is calculated automatically in calc_edges.


```r
# calculate slope from DEM as an example attribute
execGRASS("r.slope.aspect", flags = c("overwrite","quiet"),
          parameters = list(
            elevation = "dem",
            slope = "slope"
          ))
# calculate average slope per sub-catchment of each stream segment using raster and imported vector data
calc_attributes_edges(input_raster = "slope", stat_rast = "mean",
                      attr_name_rast = "avSlo", input_vector = "landuse", 
                      stat_vect = "percent", attr_name_vect = "landuse", 
                      round_dig = 4)
#calculate approx. catchment area and average slope per catchment of each site
calc_attributes_sites_approx(sites_map = "sites", 
                             input_attr_name = c("avSlo","agri","forest","grass","urban"),
                             output_attr_name = c("avSloA","agriA","forestA","grassA","urbanA"),
                             stat = c("mean", rep("percemt", 4)))
sites <- readVECT("sites", ignore.stderr = TRUE)
head(sites@data, n = 5)
#>   cat cat_ value cat_edge      dist       xm       ym locID pid netID rid
#> 1   1    1     1        5 79.907826 631046.1 226074.1     1   1    15   4
#> 2   2    2     1        5 61.158605 632011.9 225175.7     2   2    15   4
#> 3   3    3     1        2 72.041077 631203.4 224771.5     3   3    15   1
#> 4   4    4     1        2 31.226630 631787.3 224883.8     4   4    15   1
#> 5   5    5     1       15  5.027821 632259.0 224793.9     5   5    15  14
#>     upDist  distalong     ratio H2OAreaA avSloA agriA forestA grassA
#> 1 22490.34  289.70563 0.8457322     0.38 3.2165  0.12       0      0
#> 2 21166.96 1613.08658 0.1410340     2.11 3.2165  0.66       0      0
#> 3 21827.61   81.19927 0.9193403     0.11 2.6833  0.07       0      0
#> 4 21104.67  804.14221 0.2012018     1.08 2.6833  0.71       0      0
#> 5 20531.07  371.03629 0.6322076     3.93 3.0612  2.02       0      0
#>   urbanA
#> 1      0
#> 2      0
#> 3      0
#> 4      0
#> 5      0
```

The exact calculation of attribute values for the total catchment of each point can take quite long (depending on the number of points): For each point, first the total catchment is delineated based on the DEM and then intersected with the map(s) provided. It must be decided on a case by case basis if the approximate calculation is good enough.



```r
# calculate exact catchment area and average slope per catchment of each site
calc_attributes_sites_exact(sites_map = "sites", 
                            input_raster = "slope",
                            stat_rast = "mean",
                            attr_name_rast = "avSloE", 
                            input_vector = "landuse",
                            stat_vect = "percent",
                            attr_name_vect = "landuse",
                            round_dig = 4)
sites <- readVECT("sites", ignore.stderr = TRUE)
head(sites@data, n = 5)
#>   cat cat_ value cat_edge      dist       xm       ym locID pid netID rid
#> 1   1    1     1        5 79.907826 631046.1 226074.1     1   1    15   4
#> 2   2    2     1        5 61.158605 632011.9 225175.7     2   2    15   4
#> 3   3    3     1        2 72.041077 631203.4 224771.5     3   3    15   1
#> 4   4    4     1        2 31.226630 631787.3 224883.8     4   4    15   1
#> 5   5    5     1       15  5.027821 632259.0 224793.9     5   5    15  14
#>     upDist  distalong     ratio H2OAreaA avSloA agriA forestA grassA
#> 1 22490.34  289.70563 0.8457322     0.38 3.2165  0.12       0      0
#> 2 21166.96 1613.08658 0.1410340     2.11 3.2165  0.66       0      0
#> 3 21827.61   81.19927 0.9193403     0.11 2.6833  0.07       0      0
#> 4 21104.67  804.14221 0.2012018     1.08 2.6833  0.71       0      0
#> 5 20531.07  371.03629 0.6322076     3.93 3.0612  2.02       0      0
#>   urbanA H2OArea avSloE   agri grass forest urban
#> 1      0  1.0476 2.8314 0.4528     0      0     0
#> 2      0  2.4192 3.1704 0.7624     0      0     0
#> 3      0  0.6696 2.3850 0.7755     0      0     0
#> 4      0  1.2780 2.5754 0.8824     0      0     0
#> 5      0  4.0257 3.0371 0.8199     0      0     0
```
In both alternatives, the catchment area for each site is calculated automatically ('H2OAreaA' for `calc_attributes_sites_appox` and 'H2OArea' for `calc_attributes_sites_exact`).

### Write all files to an ssn folder
All files needed (edges, sites and optionally prediction sites) are written to the file path provided and can then be read in by the SSN package.


```r
ssn_dir <- file.path(tempdir(), 'nc.ssn')
export_ssn(ssn_dir)
list.files(ssn_dir)
#>  [1] "edges.dbf"   "edges.prj"   "edges.shp"   "edges.shx"   "netID1.dat" 
#>  [6] "netID10.dat" "netID11.dat" "netID12.dat" "netID13.dat" "netID14.dat"
#> [11] "netID15.dat" "netID16.dat" "netID2.dat"  "netID3.dat"  "netID4.dat" 
#> [16] "netID5.dat"  "netID6.dat"  "netID7.dat"  "netID8.dat"  "netID9.dat" 
#> [21] "sites.dbf"   "sites.prj"   "sites.shp"   "sites.shx"
```


#### Try with SSN package

```r
library(SSN)
# import
ssn_obj <- importSSN(ssn_dir, o.write = TRUE)
plot(ssn_obj, 'value')
```

![](README_files/figure-html/ssn_test-1.png)<!-- -->

```r

# Create Distance Matrix
createDistMat(ssn_obj, o.write = TRUE)
dmats <- getStreamDistMat(ssn_obj)

ssn_obj.Torg <- Torgegram(ssn_obj, "value", nlag = 20, maxlag = 15000)
plot(ssn_obj.Torg)
```

![](README_files/figure-html/ssn_test-2.png)<!-- -->

```r

names(ssn_obj@data)
#>  [1] "cat"        "flow_accu"  "netID"      "rid"        "OBJECTID"  
#>  [6] "Length"     "sourceDist" "upDist"     "H2OArea"    "rcaArea"   
#> [11] "cat_"       "avSlo_e"    "avSlo_c"    "agri_e"     "agri_c"    
#> [16] "forest_e"   "forest_c"   "grass_e"    "grass_c"    "urban_e"   
#> [21] "urban_c"
names(ssn_obj)
#> $Obs
#>  [1] "cat"       "cat_"      "value"     "cat_edge"  "dist"     
#>  [6] "xm"        "ym"        "locID"     "pid"       "netID"    
#> [11] "rid"       "upDist"    "distalong" "ratio"     "H2OAreaA" 
#> [16] "avSloA"    "agriA"     "forestA"   "grassA"    "urbanA"   
#> [21] "H2OArea"   "avSloE"    "agri"      "grass"     "forest"   
#> [26] "urban"
ssn_obj <- additive.function(ssn_obj, "H2OArea", "computed.afv")

# non-spatial model
ssn_obj.glmssn0 <- glmssn(value ~ upDist, ssn.object = ssn_obj,
                            CorModels = NULL)
summary(ssn_obj.glmssn0)
#> 
#> Call:
#> glmssn(formula = value ~ upDist, ssn.object = ssn_obj, CorModels = NULL)
#> 
#> Residuals:
#>    Min     1Q Median     3Q    Max 
#> -4.785 -2.446 -0.059  2.355  5.198 
#> 
#> Coefficients:
#>               Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  6.455e+00  7.932e-01   8.138  < 2e-16 ***
#> upDist      -1.906e-04  7.033e-05  -2.710  0.00884 ** 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Covariance Parameters:
#>  Covariance.Model Parameter Estimate
#>            Nugget   parsill     8.61
#> 
#> Residual standard error: 2.934602
#> Generalized R-squared: 0.112384
# same as
summary(lm(value ~ upDist, getSSNdata.frame(ssn_obj)))
#> 
#> Call:
#> lm(formula = value ~ upDist, data = getSSNdata.frame(ssn_obj))
#> 
#> Residuals:
#>    Min     1Q Median     3Q    Max 
#> -4.785 -2.446 -0.059  2.355  5.198 
#> 
#> Coefficients:
#>               Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  6.455e+00  7.932e-01   8.138 3.59e-11 ***
#> upDist      -1.906e-04  7.033e-05  -2.710  0.00884 ** 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 2.935 on 58 degrees of freedom
#> Multiple R-squared:  0.1124,	Adjusted R-squared:  0.09708 
#> F-statistic: 7.344 on 1 and 58 DF,  p-value: 0.008836


# # # spatial model
ssn_obj.glmssn1 <- glmssn(value ~ upDist , ssn.object = ssn_obj,
                            CorModels = c("Exponential.taildown", "Exponential.tailup"),
                          addfunccol = "computed.afv")
summary(ssn_obj.glmssn1)
#> 
#> Call:
#> glmssn(formula = value ~ upDist, ssn.object = ssn_obj, CorModels = c("Exponential.taildown", 
#>     "Exponential.tailup"), addfunccol = "computed.afv")
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -4.3222 -2.0729  0.1705  2.8023  5.5241 
#> 
#> Coefficients:
#>               Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  5.9206730  1.2692905   4.665    2e-05 ***
#> upDist      -0.0001702  0.0001008  -1.688   0.0968 .  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Covariance Parameters:
#>      Covariance.Model Parameter      Estimate
#>    Exponential.tailup   parsill     8.0030404
#>    Exponential.tailup     range 98686.5778789
#>  Exponential.taildown   parsill     0.0000240
#>  Exponential.taildown     range 87589.6922263
#>                Nugget   parsill     2.4651680
#> 
#> Residual standard error: 3.235465
#> Generalized R-squared: 0.04682144
varcomp(ssn_obj.glmssn1)
#>                VarComp   Proportion
#> 1    Covariates (R-sq) 4.682144e-02
#> 2   Exponential.tailup 7.287120e-01
#> 3 Exponential.taildown 2.185922e-06
#> 4               Nugget 2.244644e-01
```


### Contributors

+ [Mira Kattwinkel](https://github.com/MiKatt)
+ [Eduard Szöcs](https://github.com/EDiLD)

### Want to contribute?

Checkout our [contribution guide here](https://github.com/edild/openSTARS/blob/master/CONTRIBUTING.md).

### Meta

* Please [report any issues, bugs or feature requests](https://github.com/MiKatt/openSTARS/issues).
* License: MIT
