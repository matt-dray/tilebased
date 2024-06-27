#' Begin a Game
#' @param height Integer. Number of tiles high for the map.
#' @param width Integer. Number of tiles wide for the map.
#' @return Nothing. Draws to screen.
#' @export
#' @examples \dontrun{play()}
play <- function(height = 20L, width = 32L) {

  room <<- .setup_room(height, width)

  tiles <<- list(
    grass = tilebased::grass,
    tree = tilebased::tree,
    player = tilebased::player,
    enemy = tilebased::enemy
  )

  eventloop::run_loop(.explore_room)

}

#' Accept Input and Update Room
#' @param event List.
#' @param ... Not used. For compatibility.
#' @return Nothing. Draws to screen.
#' @noRd
.explore_room <- function(event, ...) {

  .draw_room(room, tiles)

  if (!is.null(event)) {
    if (event$type == 'key_press') {
      kp <- event$str
      room <<- .move_player(room, kp)
      .draw_room(room, tiles)
    }
  }

}
