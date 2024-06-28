#' Move Player Around Room
#' @param room Matrix.
#' @param kp Character.
#' @return Matrix.
#' @noRd
.move_player <- function(room, kp = c("Up", "Down", "Left", "Right")) {

  player_loc <- which(room == "@")
  room_y_max <- nrow(room)
  room[player_loc] <- "."

  if (kp %in% c("Up", "Down", "Left", "Right")) {
    if (kp == "Up") move_to <- player_loc - 1
    if (kp == "Down") move_to <- player_loc + 1
    if (kp == "Right") move_to <- player_loc + room_y_max
    if (kp == "Left") move_to <- player_loc - room_y_max
    if (room[move_to] != "#") player_loc <- move_to
  }

  room[player_loc] <- "@"

  room

}

#' Move Enemy Around Room
#' @param room Matrix.
#' @param dist Matrix.
#' @return Matrix.
#' @noRd
.move_enemy <- function(room, dist) {
  
  en_loc <- which(room == "E")
  player_loc <- which(room == "@")
  n_rows <- nrow(room)
  
  ind <- c(
    n = en_loc - 1,
    s = en_loc + 1,
    e = en_loc + n_rows,
    w = en_loc - n_rows
  )
  
  tiles <- c(
    n = room[ind["n"]],
    s = room[ind["s"]],
    e = room[ind["e"]],
    w = room[ind["w"]]
  )
  
  dist <- c(
    n = if (tiles["n"] %in% c(".", "@")) dist[ind["n"]],
    s = if (tiles["s"] %in% c(".", "@")) dist[ind["s"]],
    e = if (tiles["e"] %in% c(".", "@")) dist[ind["e"]],
    w = if (tiles["w"] %in% c(".", "@")) dist[ind["w"]]
  )
  
  
  direction <- sample(names(dist[dist == min(dist)]), 1)
  en_loc_new <- ind[names(ind) == direction]
  
  room[en_loc] <- "."
  room[en_loc_new] <- "E"
  
  room
  
}

#' Create a Mesh of Distances to the Player
#' @param room Matrix.
#' @return Matrix.
#' @noRd
.get_distance_map <- function(room) {
  
  dist <- .initiate_distance_map(room)
  
  start <- which(room == "@")
  frontier <- start
  visited <- c()
  
  while (length(frontier) > 0) {
    
    current  <- frontier[1]  # set first tile of frontier as current
    frontier <- frontier[!frontier == current]  # remove current tile from frontier
    visited  <- append(visited, current)  # mark current as visited
    
    neighbours <- .get_neighbours(room, current)  # get vector of neighbour indices
    neighbours <- neighbours[!neighbours %in% visited]
    
    for (neighbour in neighbours) {
      if (!neighbour %in% visited) {  # only assign distance to unvisited neighbours
        dist[neighbour] <- dist[current] + 1  # assign distance, one more than parent
      }
    }
    
    frontier <- append(frontier, neighbours)  # add neighbour to the frontier
    
  }
  
  dist
  
}

#' Create a Basic Mesh of Distances for the Room Layout
#' @param room Matrix.
#' @return Matrix.
#' @noRd
.initiate_distance_map <- function(room) {
  
  dist <- room
  dist[which(dist != "#")] <- 0
  dist[which(dist == "#")] <- Inf
  matrix(as.numeric(dist), nrow(dist), ncol(dist))
  
}

.get_neighbours <- function(room, current) {
  
  n_rows <- nrow(room)
  
  c(
    if (room[current - n_rows] != "#") { current - n_rows },
    if (room[current - 1] != "#") { current - 1 },
    if (room[current + 1] != "#") { current + 1 },
    if (room[current + n_rows] != "#") { current + n_rows }
  )
  
}