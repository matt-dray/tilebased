#' Move Player Around Room
#' @param room Matrix.
#' @param kp Character.
#' @return Matrix.
#' @noRd
.move_player <- function(room, kp = c("Up", "Down", "Left", "Right")) {

  player_loc <- which(room == "@")
  wrap_tiles <- .find_wrap_tiles(room)
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

#' Find Tiles in Player's Row and Column that will Wrap if Exceeded
#' @param room Matrix.
#' @return Numeric vector.
#' @noRd
.find_wrap_tiles <- function(room) {

  room_n <- matrix(seq(length(room)), nrow = nrow(room), ncol = ncol(room))
  loc <- which(room == "@", arr.ind = TRUE)
  col_vals <- room_n[, loc[, "col"]]
  row_vals <- room_n[loc[, "row"], ]

  c(
    up = min(col_vals),
    down = max(col_vals),
    left = min(row_vals),
    right = max(row_vals)
  )

}
