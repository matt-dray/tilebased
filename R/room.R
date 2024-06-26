#' Create and Populate a Room Matrix
#' @param event List.
#' @param height Integer. Number of tiles high for the map.
#' @param width Integer. Number of tiles wide for the map.
#' #' @return A matrix.
#' @noRd
.setup_room <- function(height, width) {

  # set up room tiles
  room <- matrix(".", height, width)

  # mark edges as walls
  edge_n <- seq(1, length(room), height)
  edge_s <- seq(height, length(room), height)
  edge_w <- seq(height)
  edge_e <- seq(length(room) - (height - 1), length(room), 1)
  edge_tiles <- sort(unique(c(edge_n, edge_s, edge_e, edge_w)))
  room[edge_tiles] <- "#"

  # place player in interior
  interior <- which(room == ".")
  room[sample(interior, 1)] <- "@"  # place player

  room

}

#' Draw the Room
#' @param room Matrix.
#' @param tiles List of nativeRasters.
#' @return Nothing. Draws to screen.
#' @noRd
.draw_room <- function(room, tiles) {

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

  # place tree tiles
  tree_loc <- which(room == "#", arr.ind = TRUE)
  tree_tile_x <- tree_loc[, "col"]
  tree_tile_y <- tree_loc[, "row"]
  tree_pixels_x <- x_seq[tree_tile_x]
  tree_pixels_y <- y_seq[tree_tile_y]
  for (i in seq(nrow(tree_loc))) {
      nara::nr_blit(nr, tree_pixels_x[i], tree_pixels_y[i], tiles[["tree"]])
  }

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
