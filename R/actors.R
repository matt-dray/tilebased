#' Move a Player Around a Board Mesh
#' @param boardmesh Matrix.
#' @param keypress Character.
#' @return Matrix.
#' @noRd
.move_player <- function(
  boardmesh,
  keypress = c("Up", "Down", "Left", "Right")
) {
  
  board_y_max <- nrow(boardmesh)
  
  player_loc <- which(boardmesh == "@")
  boardmesh[player_loc] <- "."  # replace old board-mesh location with floor
  
  if (keypress %in% c("Up", "Down", "Left", "Right")) {
    
    # Get new tile index in the direction of the key press
    if (keypress == "Up")    move_to <- player_loc - 1
    if (keypress == "Down")  move_to <- player_loc + 1
    if (keypress == "Right") move_to <- player_loc + board_y_max
    if (keypress == "Left")  move_to <- player_loc - board_y_max
    
    # Undo the move if the new location is an obstacle.
    if (boardmesh[move_to] != "#") player_loc <- move_to
    
  }
  
  boardmesh[player_loc] <- "@"  # move player to new board-mesh location
  
  boardmesh
  
}

#' Move an Enemy Around a Board Mesh
#' @param boardmesh Matrix.
#' @param navmesh Matrix.
#' @return Matrix.
#' @noRd
.move_enemy <- function(boardmesh, navmesh) {
  
  n_rows <- nrow(boardmesh)
  
  en_loc <- which(boardmesh == "e")
  player_loc <- which(boardmesh == "@")
  
  # Get board-mesh indices of neighbour cells
  nbr_i <- c(
    n = en_loc - 1,
    s = en_loc + 1,
    e = en_loc + n_rows,
    w = en_loc - n_rows
  )
  
  # Get board-mesh content of neighbour cells
  nbr_cells <- c(
    n = boardmesh[nbr_i["n"]],
    s = boardmesh[nbr_i["s"]],
    e = boardmesh[nbr_i["e"]],
    w = boardmesh[nbr_i["w"]]
  )
  
  # Get navigation-mesh distance values of viable neighbour cells
  navmesh <- c(
    n = if (nbr_cells["n"] %in% c(".", "@")) navmesh[nbr_i["n"]],
    s = if (nbr_cells["s"] %in% c(".", "@")) navmesh[nbr_i["s"]],
    e = if (nbr_cells["e"] %in% c(".", "@")) navmesh[nbr_i["e"]],
    w = if (nbr_cells["w"] %in% c(".", "@")) navmesh[nbr_i["w"]]
  )
  
  # Move enemy to a viable cell in the board mesh
  direction <- sample(names(navmesh[navmesh == min(navmesh)]), 1)
  en_loc_new <- nbr_i[names(nbr_i) == direction]
  boardmesh[en_loc] <- "."
  boardmesh[en_loc_new] <- "e"
  
  boardmesh
  
}

#' Populate an Enemy Navigation Mesh with Distance Values
#' @param boardmesh Matrix.
#' @details The enemy will traverse cells from higher to lower distance values
#'     in the navigation mesh. The player is at distance value zero.
#' @return Matrix.
#' @noRd
.get_navmesh <- function(boardmesh) {
  
  navmesh <- .initiate_navmesh(boardmesh)
  
  start <- which(boardmesh == "@") 
  frontier <- start
  visited <- c()
  
  while (length(frontier) > 0) {
    
    current  <- frontier[1]  # set first cell of frontier as current
    frontier <- frontier[!frontier == current]  # remove current cell from frontier
    visited  <- append(visited, current)  # mark current as visited
    
    neighbours <- .get_neighbours(boardmesh, current)  # get vector of neighbour indices
    neighbours <- neighbours[!neighbours %in% visited]
    
    for (neighbour in neighbours) {
      if (!neighbour %in% visited) {  # only assign distance to unvisited neighbours
        navmesh[neighbour] <- navmesh[current] + 1  # assign distance, one more than parent
      }
    }
    
    frontier <- append(frontier, neighbours)  # add neighbour to the frontier
    
  }
  
  navmesh
  
}

#' Initiate an Enemy Navigation Mesh from a Board Mesh
#' @param boardmesh Matrix.
#' @details The enemy will travel to tiles with lower distance values according
#'     to the navigation mesh. Obstacle tiles will not be traversed because they
#'     are given a a starting value of infinity.
#' @return Matrix.
#' @noRd
.initiate_navmesh <- function(boardmesh) {
  navmesh <- boardmesh
  navmesh[which(navmesh != "#")] <- 0  # set initial value
  navmesh[which(navmesh == "#")] <- Inf  # will be completely avoided
  matrix(as.numeric(navmesh), nrow(navmesh), ncol(navmesh))
}

#' Get Board-Mesh Indices for Neighbour Tiles of a Focus Tile
#' @param boardmesh Matrix.
#' @param current Numeric.
#' @details Ignores obstacle tiles.
#' @return Matrix.
#' @noRd
.get_neighbours <- function(boardmesh, current) {
  
  n_rows <- nrow(boardmesh)
  
  c(
    if (boardmesh[current - n_rows] != "#") current - n_rows,
    if (boardmesh[current - 1]      != "#") current - 1,
    if (boardmesh[current + 1]      != "#") current + 1,
    if (boardmesh[current + n_rows] != "#") current + n_rows
  )
  
}