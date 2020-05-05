# openSTARS 1.1.0.999

* Renamed check_compl_junctions and correct_compl_junctions to 
  check_compl_confluences and correct_compl_confluences, respectively,
  to be in line with the STARS publications.
* correct_compl_confluences now works for up to seven inflows to one outflow
* Corrected an error in correct_compl_confluences: If there were outflows from a 
  complex junction that were also inflows to complex confluences a wrong assignment
  might have occured.
* Updated all lengths in correct_complex_confluences 
  (segement length, cum_length, out_dist)
* correct_complex_confluences now works correctly for non-sqared cells (use max
  cell dimension instead of min for calculation of flow directions at conflueces)
* New function to delete lakes from the network ('delete_lakes'). Added "lakes.shp"
  to example data.
* New function to restrict edges to certain networks based on sites (obeservation
  or prediction) or given netIDs and delete the rest ('restict_network').
* New functionality in calc_attributes_edges and calc_attributes_sites_exact: 
  It is now possible to calculate percentages from raster maps with multiple values
  (e.g. land cover classes). 
* New function to merge an external table with (repeated) measurements to the sites
  (merge_sites_measurements).
* Sites has now the column 'str_edge' (for 'stream') instead of 'cat_edge' 
  to relate the site to its edge.
* Several smaller fixes.
* Delete stream segments with zero length in derive_streams


# openSTARS 1.1.0

* Fixed error when loading (import_data) example shapes: no empty cell allowed
  in attribute table (used to work when checking).
* Bug fix in correct_compl_junctions(): if the cut outflow was also the inflow
  to another complex junction an error occured like
  "Error in `[<-.data.table`(`*tmp*`, ii, jj, value = c(12013, 12019)) :  NA in j".
* Set round rcaArea and H2OArea to 6 digits instead of 4 in calc_edges() to be 
  able to retrive squared meters from the sqared km.
* Updated calc_attributes_edges() for vector input data; it is now possible to 
  calculate percentage of a certain attribute within a catchment (polygons) or 
  count the number of features in a catchment (points).
* Changed setup_grass_environment: Extent and projection are set to the one of the
  DEM (compared to setting the projections according to the sites object, epsg 
  code or proj4 string as it was done before). The reason for this change is 
  that rather vector files should be projected than raster files (i.e. sites and 
  streams instead of DEM) and that the extent of the DEM should be larger than 
  that of the sites.
* Change import_data: Now vector files are reprojected to match the projection 
  of the DEM (i.e. the one of the current location).
* Updated calc_attributes_sites_exact() for vector input data. It is now possible
  to calculate percentages of a certain attribute class in a catchment (polygons)
  or count the numbers of features in a catchment (points)

# openSTARS 1.0.0

* First CRAN release Aug. 2017



