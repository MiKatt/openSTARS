watershed_memory = function(dem = NULL) {
  # https://grass.osgeo.org/grass73/manuals/r.watershed.html
  ras = raster(dem)
  nc = ncell(ras)
  ram = 31 * nc / 1000000
  message('A maximum of ', ram, ' MB are needed to process this raster.')
}