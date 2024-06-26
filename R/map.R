#' Draw the Room
#' @param room Matrix.
#' @param tiles List of nativeRasters.
#' @return Nothing. Draws to screen.
#' @noRd
.print_room <- function(room, tiles) {

  # tile dimensions (assumes all are square and same size)
  tile_dim <- dim(tiles[[1]])
  tile_pixels_x <- tile_dim[2]

  # generate canvas

  n_tiles_x <- ncol(room)
  x_pixels <- tile_pixels_x * n_tiles_x
  x_seq <- seq(0, x_pixels - tile_pixels_x, tile_pixels_x)

  n_tiles_y <- nrow(room)
  y_pixels <- tile_pixels_x * n_tiles_y
  y_seq <- seq(0, y_pixels - tile_pixels_x, tile_pixels_x)

  nr <- nara::nr_new(x_pixels, y_pixels, "black")

  # place floor tiles
  for (x in x_seq) for (y in y_seq) nara::nr_blit(nr, x, y, tiles[["grass"]])

  # TODO: place boundary tiles with collision
  # for (x in x_seq) nara::nr_blit(nr, x, 0, tiles[["tree"]])

  # place player tile
  player_loc <- which(room == "@", arr.ind = TRUE)
  player_tile_x <- player_loc[, "col"]
  player_tile_y <- player_loc[, "row"]
  player_pixels_x <- x_seq[player_tile_x]
  player_pixels_y <- y_seq[player_tile_y]
  nara::nr_blit(nr, player_pixels_x, player_pixels_y, tiles[["player"]])

  # draw
  grid::grid.newpage()
  grid::grid.raster(nr, interpolate = FALSE)

}
