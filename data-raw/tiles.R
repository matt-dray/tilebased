grass <- system.file(
    "extdata",
    "kenney_tiny-town_tile_0001.png",
    package = "tilebased"
  ) |>
  .tile_to_nr()

tree <- system.file(
    "extdata",
    "kenney_tiny-town_tile_0028.png",
    package = "tilebased"
  ) |>
  .tile_to_nr()

player <- system.file(
    "extdata",
    "kenney_tiny-dungeon_tile_0085.png",
    package = "tilebased"
  ) |>
  .tile_to_nr()

enemy <- system.file(
  "extdata",
  "kenney_tiny-dungeon_tile_0124.png",
  package = "tilebased"
) |>
  .tile_to_nr()

usethis::use_data(grass, overwrite = TRUE)
usethis::use_data(tree, overwrite = TRUE)
usethis::use_data(player, overwrite = TRUE)
usethis::use_data(enemy, overwrite = TRUE)
