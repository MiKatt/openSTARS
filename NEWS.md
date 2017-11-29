# openSTARS 1.0.0.999

* Fixed error when loading (import_data) example shapes: no empty cell allowed
  in attribute table (used to work when checking).
* Bug fix in correct_compl_junctions(): if the cut outflow was also the inflow
  to another complex junction an error occured like
  "Error in `[<-.data.table`(`*tmp*`, ii, jj, value = c(12013, 12019)) :  NA in j".
* ISet round rcaArea and H2OArea to 6 digits instead of 4 in calc_edges() to be 
  able to retrive squared meters from the sqared km.
* Updated calc_attributes_edges() for vector inpunt data; it is now possible to 
  calculate percentage of a certain attribute within a catchment (polygon) or 
  count the number of features in a catchment (point).

# openSTARS 1.0.0

* First CRAN release Aug. 2017



