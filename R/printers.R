print_grid <- function(grid, cell_callback) {
  for (row in seq_len(length(grid[[1]]))) {
    for (column in seq_len(length(grid))) {
      cat(cell_callback(grid[[column]][[row]]), " ")
    }
    cat("\n")
  }
}

print_grid_object <- function(tiles, iterations, grid) {
  cat("Tiles: \n")
  for (tile in tiles) {
    cat(tile$get_hash(), "\n")
  }

  cat("Iterations: \n")
  cat(iterations, "\n")

  cat("Last Grid: \n")
  grid %>%
    print_grid(function(cell) {
      cell$get_entropy()
    })
}

print_entropy_grid <- function(grid) {
  cat("Entropy Grid: \n")
  grid %>%
    print_grid(function(cell) {
      cell$get_entropy()
    })
}

print_entropy_grid <- function(grid) {
  cat("Step Grid: \n")
  grid %>%
    print_grid(function(cell) {
      cell
    })

  invisible(self)
}