openSTARS
=============



[![Build Status](https://travis-ci.org/ropensci/webchem.png)](https://travis-ci.org/edild/openSTARS)
[![Open Issues](https://img.shields.io/github/issues/edild/openSTARS.svg)](https://github.com/edild/openSTARS/issues)

`openSTARS` is an open source implementation of the STARS toolbox (Peterson & Ver Hoef, 2014) using R and GRASS GIS.
It prepares the .ssn object needed for the SSN package.
Currently a DEM is used to derive stream networks (in contrast to STARS, which can clean an existing stream network).

For more information on STARS and SSN, see [their webpage](http://www.fs.fed.us/rm/boise/AWAE/projects/SpatialStreamNetworks.shtml).

Peterson, E. E., & Ver Hoef, J. M. (2014). STARS: An ArcGIS Toolset Used to Calculate the Spatial Information Needed to Fit Spatial Statistical Models to Stream Network Data. J Stat Softw, 56(2), 1–17.


### Installation
A functional installation of [GRASS GIS (>=7.0)](https://grass.osgeo.org/#) with installed addons [r.stream.basins](https://grass.osgeo.org/grass70/manuals/addons/r.stream.basins.html), [r.stream.distance](https://grass.osgeo.org/grass70/manuals/addons/r.stream.distance.html) and [r.stream.order](https://grass.osgeo.org/grass70/manuals/addons/r.stream.order.html) is needed.

The openSTARS package can be installed from github using


```r
install.packages("devtools")
devtools::install_github("edild/openSTARS")
library('openSTARS')
```


### Basic usage

Initiate a ephemeral GRASS session:

```r
library(openSTARS)
initGRASS(gisBase = "/usr/lib/grass70/",
          home = tempdir(),
          override = TRUE)
#> gisdbase    /tmp/RtmptrpmL9 
#> location    file3afa6e0edc24 
#> mapset      file3afa45e7c875 
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

Load DEM and sites into GRASS:


```r
dem_path <- system.file("extdata", "nc", "elev_ned_30m.tif", package = "openSTARS")
sites_path <- system.file("extdata", "nc", "sites_nc.shp", package = "openSTARS")

import_data(dem = dem_path, sites = sites_path)
#> WARNING: Over-riding projection check
#> WARNING: Raster map <dem> already exists and will be overwritten

gmeta()
#> gisdbase    /tmp/RtmptrpmL9 
#> location    file3afa6e0edc24 
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

The dem is loaded as raster names `dem`, the sites as vector named `sites_o`.
Here's how the data looks like:


```r
dem <- readRAST('dem', ignore.stderr = TRUE)
#> Creating BIL support files...
#> Exporting raster as floating values (bytes=4)
#>    0%   3%   6%   9%  12%  15%  18%  21%  24%  27%  30%  33%  36%  39%  42%  45%  48%  51%  54%  57%  60%  63%  66%  69%  72%  75%  78%  81%  84%  87%  90%  93%  96%  99% 100%
sites <- readVECT('sites_o', ignore.stderr = TRUE)
plot(dem, col = terrain.colors(20))
cols <- colorRampPalette(c("blue", 'red'))(length(sites$value))[rank(sites$value)]
points(sites, pch = 16, col = cols)
```

![](README_files/figure-html/plot_data1-1.png)<!-- -->

Derive streams from DEM:

```r
derive_streams()
```


```r
streams <- readVECT('streams_v', ignore.stderr = TRUE)
#> WARNING: 178 points found, but not requested to be exported. Verify 'type'
#>          parameter.
plot(dem, col = terrain.colors(20))
lines(streams, col = 'blue')
points(sites, pch = 16, col = cols)
```

![](README_files/figure-html/derive_streams2-1.png)<!-- -->

Prepare edges:


```r
calc_edges()
```


```r
edges <- readVECT('edges', ignore.stderr = TRUE)
head(edges@data)
#>   cat stream_type type_code rid OBJECTID netID    upDist topo_dim
#> 1   1       start         0 114      114   162 1877.9394       26
#> 2   2       start         0 111      111   162 1268.5281       23
#> 3   3       start         0 115      115   162 1877.9394       26
#> 4   4       start         0  79       79   151 6824.7727        7
#> 5   5       start         0   8        8     5 2687.0563        1
#> 6   6       start         0 106      106   162  204.8528       18
#>   H2OAreaKm2 rcaAreaKm2
#> 1     246.15     246.15
#> 2     106.74     106.74
#> 3     134.73     134.73
#> 4     552.33     552.33
#> 5     283.05     283.05
#> 6     310.41     310.41
```

`edges` now holds the derived network plus attributes needed for SSN (segment id, network id, upstream distance, watershed area, river contributing area, toplogical dimension)


Prepare sites:


```r
calc_sites()
```


```r
sites <- readVECT('sites', ignore.stderr = TRUE)
edges <- readVECT('edges', ignore.stderr = TRUE)
plot(dem, col = terrain.colors(20))
lines(edges, col = 'blue')
points(sites, pch = 16, col = cols)
```

![](README_files/figure-html/sites-1.png)<!-- -->

```r
head(sites@data)
#>   cat cat_ value       dist       xm       ym pid locID netID rid   upDist
#> 1   1    1     1  79.907826 631046.1 226074.1   1     1   162  96 22387.92
#> 2   2    2     2  76.098623 631495.3 225849.5   2     2   162  96 21880.94
#> 3   3    3     1 112.904797 631787.3 225580.0   3     3   162  96 21511.53
#> 4   4    4     1  61.158605 632011.9 225175.7   4     4   162  96 21064.54
#> 5   5    5     1  72.041077 631203.4 224771.5   5     5   145  65 21721.53
#> 6   6    6     2   4.226159 631540.2 224883.8   6     6   145  65 21256.97
#>        H20areaKm
#> 1 2371500.000000
#> 2 2371500.000000
#> 3 2371500.000000
#> 4 2371500.000000
#> 5 2371500.000000
#> 6 2371500.000000
```

Now the sites are snapped to the network and additional attributes are appended to the sites.





### Contributors

+ [Eduard Szöcs](https://github.com/EDiLD)

### Want to contribute?

Checkout our [contribution guide here](https://github.com/edild/openSTARS/blob/master/CONTRIBUTING.md).

### Meta

* Please [report any issues, bugs or feature requests](https://github.com/edild/openSTARS/issues).
* License: MIT
