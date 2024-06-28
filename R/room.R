#' Create and Populate a Room Matrix
#' @param height Integer. Number of tiles high for the map.
#' @param width Integer. Number of tiles wide for the map.
#' #' @return A matrix.
#' @noRd
.setup_room <- function(height, width) {

  # set up room tiles
  # room <- matrix(".", height, width)
  room <- r.oguelike::generate_dungeon(n_row = height, n_col = width)

  # mark all edges as walls
  edge_n <- seq(1, length(room), height)
  edge_s <- seq(height, length(room), height)
  edge_w <- seq(height)
  edge_e <- seq(length(room) - (height - 1), length(room), 1)
  edge_tiles <- sort(unique(c(edge_n, edge_s, edge_e, edge_w)))
  room[edge_tiles] <- "#"

  # place actors
  interior <- which(room == ".")
  room[sample(interior, 1)] <- "@"  # place player
  interior <- which(room == ".")
  room[sample(interior, 1)] <- "E"  # place enemy

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
  n_tiles_y <- nrow(room)
  
  x_pixels <- tile_pixels_x * n_tiles_x
  y_pixels <- tile_pixels_x * n_tiles_y
  
  x_seq <- seq(0, x_pixels - tile_pixels_x, tile_pixels_x)
  y_seq <- seq(0, y_pixels - tile_pixels_x, tile_pixels_x)
  
  nr <- nara::nr_new(x_pixels, y_pixels, "black")

  # place tiles

  for (x in x_seq) for (y in y_seq) nara::nr_blit(nr, x, y, tiles[["grass"]])

  for (actor in c("#", "@", "E")) {
    .place_tiles(room, nr, x_seq, y_seq, actor, tiles)
  }

  # draw 
  grid::grid.newpage()
  grid::grid.raster(nr, interpolate = FALSE)

}

#' Draw Tiles in the Room
#' @param room Matrix.
#' @param nr nativeRaster.
#' @param x_seq Numeric. Canvas width in pixels.
#' @param y_seq Numeric. Canvas height in pixels.
#' @param actor Character. Named symbol in room mesh.
#' @return Nothing. Draws to screen.
#' @noRd
.place_tiles <- function(
  room,
  nr,
  x_seq,
  y_seq,
  actor = c("#", "@", "E"),
  tiles
) {

  actor_name <- switch(
    actor,
    "#" = "tree",
    "@" = "player", 
    "E" = "enemy"
  )

  loc <- which(room == actor, arr.ind = TRUE)
  tile_x <- loc[, "col"]
  tile_y <- loc[, "row"]
  pixels_x <- x_seq[tile_x]
  pixels_y <- y_seq[tile_y]

  is_single_tile <- length(loc) == 1

  if (is_single_tile) {
    nara::nr_blit(nr, pixels_x, pixels_y, tiles[[actor_name]])
  }

  if (!is_single_tile) {
    for (i in seq(nrow(loc))) {
    nara::nr_blit(nr, pixels_x[i], pixels_y[i], tiles[[actor_name]])
    }
  }

}