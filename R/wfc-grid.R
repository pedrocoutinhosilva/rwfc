get_neighbour_positions <- function(cell, width, height) {
  list(
    up = switch(
      cell$row - 1 > 0,
      list(row = cell$row - 1, column = cell$column),
      NULL
    ),
    right = switch(
      cell$column + 1 <= width,
      list(row = cell$row, column = cell$column + 1),
      NULL
    ),
    down = switch(
      cell$row + 1 <= height,
      list(row = cell$row + 1, column = cell$column),
      NULL
    ),
    left = switch(
      cell$column - 1 > 0,
      list(row = cell$row, column = cell$column - 1),
      NULL
    )
  )
}

next_collapse_target = function(width, height, grid, tiles) {
  entropy_grid <- unlist(lapply(seq_len(width), function(column) {
    lapply(seq_len(height), function(row) {
      grid[[column]][[row]]$get_entropy()
    })
  }), recursive = FALSE)

  entropy_grid <- entropy_grid[! entropy_grid %in% c(1)]

  if (identical(length(entropy_grid), 0)) {
    return(FALSE)
  }

  smallest_entropy <- sort(unique(unlist(entropy_grid)))[1]

  if (identical(smallest_entropy, length(tiles))) {
    return(
      list(
          column = sample(seq_len(width), 1),
          row = sample(seq_len(height), 1)
        )
    )
  }

  next_cell <- NULL
  for (column in seq_len(width)) {
    for (row in seq_len(height)) {
      if (identical(
        grid[[column]][[row]]$get_entropy(),
        smallest_entropy)
      ) {
        next_cell <- list(
          row = row,
          column = column
        )
        return(next_cell)
      }
    }
  }

  return(FALSE)
}

new_grid <- function(width, height, tiles) {
  new_grid <- vector("list", width)

  for (index in seq_len(width)) {
    new_grid[[index]] <- vector("list", height)
  }

  for (column in seq_len(width)) {
    for (row in seq_len(height)) {
      new_grid[[column]][[row]] <- wfc_cell(tiles, column, row)
    }
  }

  new_grid
}

generate_rotation_tiles <- function(tiles) {
  temp_tiles <- tiles

  for (tile in tiles) {
    if (!is.null(tile$get_rotations())) {
      for (rotation in seq_len(length(tile$get_rotations()))) {
        temp_tiles <- append(
          temp_tiles,
          c(tile$rotate(rotation, tile$get_rotations()[rotation]))
        )
      }
    }
  }

  temp_tiles
}

#' @importFrom R6 R6Class
#' @importFrom purrr keep
#' @importFrom stringi stri_reverse
wfc_grid_class <- R6Class("wcfGrid",
  private = list(
    height = NULL,
    width = NULL,
    grid = NULL,

    step_grid = NULL,

    iterations = 0,

    tiles = NULL,

    add_iteration = function() {
      private$iterations <- private$iterations + 1
    },

    remove_iteration = function() {
      private$iterations <- private$iterations - 1
    },

    get_neighbour_positions = function(cell) {
      get_neighbour_positions(cell, private$width, private$height)
    },

    next_collapse_target = function() {
      next_collapse_target(
        private$width,
        private$height,
        private$grid,
        private$tiles
      )
    },

    new_grid = function() {
      new_grid(private$width, private$height, private$tiles)
    }
  ),

  public = list(
    initialize = function(width, height, tiles, rotations = TRUE) {
      private$width <- width
      private$height <- height

      private$tiles <- tiles

      # generate rotation tiles
      if (rotations) {
        private$tiles <- generate_rotation_tiles(tiles)
      }

      self$reset()
    },

    reset = function() {
      private$iterations <- 0
      private$grid <- private$new_grid()
      private$add_iteration()
    },

    get_width = function() {
      private$width
    },

    get_height = function() {
      private$height
    },

    get_grid = function() {
      private$grid
    },

    get_tiles = function() {
      private$tiles
    },

    # TODO
    step_back = function() {
      private$remove_iteration()
    },

    reset_step_grid = function(width = private$width, height = private$height) {
      new_grid <- vector("list", width)

      for (index in seq_len(width)) {
        new_grid[[index]] <- vector("list", height)
      }

      for (column in seq_len(width)) {
        for (row in seq_len(height)) {
          new_grid[[column]][[row]] <- ifelse(
            private$grid[[column]][[row]]$get_entropy() == 1,
            "x",
            0
          )
        }
      }

      private$step_grid <- new_grid
    },

    step_ripple = function(ripple, next_ripple_cells) {
      if (length(next_ripple_cells) == 0 ||
          !0 %in% unique(unlist(private$step_grid)) ||
          ripple == 4) {
        return()
      }

      temp_next_ripple_cells <- list()

      for (next_ripple_cell in next_ripple_cells) {
        for (next_cell in private$get_neighbour_positions(next_ripple_cell)) {
          if (!is.null(next_cell)) {
            column <- next_cell$column
            row <- next_cell$row
            if (identical(private$step_grid[[column]][[row]], 0)) {
              private$step_grid[[column]][[row]] <- ripple + 1

              self$recalculate_cell_entropy(column, row)

              temp_next_ripple_cells <- append(
                temp_next_ripple_cells,
                list(next_cell)
              )
            }
          }
        }
      }

      self$step_ripple(ripple + 1, temp_next_ripple_cells)
    },

    step = function() {
      # get random lowest entropy random
      starter_cell <- private$next_collapse_target()

      # No more starter cells. Its collapsed
      if (identical(starter_cell, FALSE)) {
        return(FALSE)
      }

      self$reset_step_grid()

      private$grid[[starter_cell$column]][[starter_cell$row]]$collapse()
      private$step_grid[[starter_cell$column]][[starter_cell$row]] <- 1

      self$step_ripple(1, list(starter_cell))

      private$add_iteration()

      print("step done")
      return(TRUE)
    },

    process_neighbour_tile = function(cell,
                                        neighbour_cell,
                                        direction_index,
                                        oposite_direction_index) {

      # neighbour has not been at least partially collapsed
      # all options are possible
      if (identical(neighbour_cell$get_entropy(), length(private$tiles))) { # nolint
        return(FALSE)
      }

      # sockets that the neighbour_cell allows
      neighbour_sockets <- neighbour_cell$get_possible_tiles() %>%
        purrr::map(function(tile) {
          tile$get_sockets()[[oposite_direction_index]]
        }) %>%
        unique() %>%
        stringi::stri_reverse()

      # keep tiles of the current cell that fit the neighbour_cell
      filtered_tiles <- cell$get_possible_tiles() %>%
        purrr::keep(
          function(tile) {
            tile$get_sockets()[[direction_index]] %in% neighbour_sockets
          }
        )

      cell$set_possible_tiles(filtered_tiles)

      return(TRUE)
    },

    recalculate_cell_entropy = function(column, row) {
      # get cell
      cell <- private$grid[[column]][[row]]

      # get neighbours
      neighbours <- list(column = column, row = row) %>%
        private$get_neighbour_positions()

      # calculate tiles based on that info
      if (!is.null(neighbours$up)) {
        position <- neighbours$up
        neighbour_cell <- private$grid[[position$column]][[position$row]]

        self$process_neighbour_tile(cell, neighbour_cell, 1, 3)
      }
      if (!is.null(neighbours$right)) {
        position <- neighbours$right
        neighbour_cell <- private$grid[[position$column]][[position$row]]

        self$process_neighbour_tile(cell, neighbour_cell, 2, 4)
      }
      if (!is.null(neighbours$down)) {
        position <- neighbours$down
        neighbour_cell <- private$grid[[position$column]][[position$row]]

        self$process_neighbour_tile(cell, neighbour_cell, 3, 1)
      }
      if (!is.null(neighbours$left)) {
        position <- neighbours$left
        neighbour_cell <- private$grid[[position$column]][[position$row]]

        self$process_neighbour_tile(cell, neighbour_cell, 4, 2)
      }
    },

    solve = function() {
      start_time <- Sys.time()

      while (TRUE) {
        step <- self$step()

        if (identical(step, FALSE)) {
          break
        }
      }

      return(paste("Solved in:", Sys.time() - start_time))
    },

    print = function(...) {
      print_grid_object(private$tiles, private$iterations, private$grid)
      invisible(self)
    },

    print_step_grid = function() {
      print_step_grid(private$step_grid)
      invisible(self)
    },

    print_entropy_grid = function() {
      print_entropy_grid(private$grid)
      invisible(self)
    }
  )
)

#' @export
wfc_grid <- function(...) {
  wfc_grid_class$new(...)
}
