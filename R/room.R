#' Create and Populate a Board Mesh
#' @param height Integer. Number of tiles high for the board.
#' @param width Integer. Number of tiles wide for the board.
#' @return A matrix.
#' @noRd
.setup_boardmesh <- function(height, width) {
  
  # Set up a procedural board mesh
  boardmesh <- r.oguelike::generate_dungeon(n_row = height, n_col = width)
  
  # Place actors at random
  interior <- which(boardmesh == ".")
  boardmesh[sample(interior, 1)] <- "@"  # player
  interior <- which(boardmesh == ".")
  boardmesh[sample(interior, 1)] <- "e"  # enemy
  
  boardmesh
  
}

#' Draw a Board According to a Board Mesh
#' @param boardmesh Matrix.
#' @param tiles List of nativeRasters.
#' @return Nothing. Draws to a graphics device.
#' @noRd
.draw_board <- function(boardmesh, tiles) {
  
  # Assess tile dimensions (assumes all are square and same size)
  tile_dim <- dim(tiles[[1]])
  tile_pixels_x <- tile_dim[2]
  
  # Generate board mesh
  
  n_tiles_x <- ncol(boardmesh)
  n_tiles_y <- nrow(boardmesh)
  
  x_pixels <- tile_pixels_x * n_tiles_x
  y_pixels <- tile_pixels_x * n_tiles_y
  
  x_seq <- seq(0, x_pixels - tile_pixels_x, tile_pixels_x)
  y_seq <- seq(0, y_pixels - tile_pixels_x, tile_pixels_x)
  
  nr <- nara::nr_new(x_pixels, y_pixels, "black")
  
  # Place tiles
  
  for (x in x_seq) for (y in y_seq) nara::nr_blit(nr, x, y, tiles[["grass"]])
  
  for (actor in c("#", "@", "e")) {
    .place_tiles(boardmesh, nr, x_seq, y_seq, actor, tiles)
  }

  # Draw to graphics device
  grid::grid.newpage()
  grid::grid.raster(nr, interpolate = FALSE)

}

#' Place Tiles on a Board According to a Board Mesh
#' @param boardmesh Matrix.
#' @param nr nativeRaster.
#' @param x_seq Numeric. Board width in pixels.
#' @param y_seq Numeric. Board height in pixels.
#' @param actor Character. Named symbol in board mesh.
#' @return Nothing. Draws to a graphics device.
#' @noRd
.place_tiles <- function(
  boardmesh,
  nr,
  x_seq,
  y_seq,
  actor = c("#", "@", "e"),
  tiles
) {
  
  # Find tile locations on the board given board-mesh locations
  
  loc <- which(boardmesh == actor, arr.ind = TRUE)
  
  tile_x <- loc[, "col"]
  tile_y <- loc[, "row"]
  
  pixels_x <- x_seq[tile_x]
  pixels_y <- y_seq[tile_y]
  
  # Blit actor tiles to the board
  
  actor_name <- switch(
    actor, 
    "#" = "tree",
    "@" = "player", 
    "e" = "enemy"
  )
  
  is_single_tile <- length(loc) == 1
  
  if (is_single_tile) nara::nr_blit(nr, pixels_x, pixels_y, tiles[[actor_name]])
  
  if (!is_single_tile) {
    for (i in seq(nrow(loc))) {
      nara::nr_blit(nr, pixels_x[i], pixels_y[i], tiles[[actor_name]])
    }
  }
  
}