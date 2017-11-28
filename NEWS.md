# openSTARS 1.0.0.999

* fixed error when loading (import_data) example shapes: no empty cell allowed
  in attribute table (used to work when checking).
* bug fix in correct_compl_junctions(): if the cut outflow was also the inflow
  to another complex junction an error would occur like
  "Error in `[<-.data.table`(`*tmp*`, ii, jj, value = c(12013, 12019)) :  NA in j"
* in calc_edges round rcaArea and H2OArea to 6 digits instead of 4 to be able to
  retrive squared meters from the sqared km

# openSTARS 1.0.0

* First CRAN release Aug. 2017

# openSTARS 1.0.1

