#' Read Tile to nativeRaster
#' @param path Character. Path to a tile stored as png.
#' @return nativeRaster.
#' @noRd
.tile_to_nr <- function(path) {
  png::readPNG(path, native = TRUE)
}
