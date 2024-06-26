#' Begin a Game
#' @param height Integer. Number of tiles high for the map.
#' @param width Integer. Number of tiles wide for the map.
#' @return Nothing. Draws to screen.
#' @export
#' @examples \dontrun{play()}
play <- function(height = 5L, width = height) {

  tiles_set <<- list(
    grass = tilebased::grass,
    tree = tilebased::tree,
    player = tilebased::player
  )

  tile_n <- height * width
  room_mat <<- matrix(rep(".", tile_n), nrow = height, ncol = width)
  room_mat[sample(seq(tile_n), 1)] <<- "@"  # place player

  eventloop::run_loop(explore_room)

}

#' Accept Input and Update Room
#' @param event List.
#' @param ... Not used. For compatibility.
#' @return Nothing. Draws to screen.
#' @noRd
explore_room <- function(event, ...) {

  .print_room(room_mat, tiles_set)

  if (!is.null(event)) {
    if (event$type == 'key_press') {
      kp <- event$str
      room_mat <<- .move_player(room_mat, kp)
      .print_room(room_mat, tiles_set)
    }
  }


}
