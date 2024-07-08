#' Begin a Game
#' @param height Integer. Number of tiles high for the board.
#' @param width Integer. Number of tiles wide for the board.
#' @return Nothing. Draws to a graphics device.
#' @export
#' @examples \dontrun{play()}
play <- function(height = 20L, width = 32L) {

  if (!is.integer(height) || !is.integer(width)) {
    stop('Arguments height and width must be integers.', call. = FALSE)
  }
  
  boardmesh <<- .setup_boardmesh(height, width)

  tileset <<- list(
    grass  = tilebased::grass,
    tree   = tilebased::tree,
    player = tilebased::player,
    enemy  = tilebased::enemy
  )

  eventloop::run_loop(.explore_board)

}

#' Accept Input, Update and Draw the Board
#' @param event List.
#' @param ... Not used. For compatibility.
#' @return Nothing. Draws to a graphics device.
#' @noRd
.explore_board <- function(event, ...) {

  .draw_board(boardmesh, tileset)

  if (!is.null(event) && event$type == "key_press") {

      key_pressed <- event$str
    
      boardmesh <<- .move_player(boardmesh, key_pressed)
      navmesh   <<- .get_navmesh(boardmesh)
      boardmesh <<- .move_enemy(boardmesh, navmesh)
    
      .draw_board(boardmesh, tileset)
    
  }

}
