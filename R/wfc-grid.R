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

    get_neighbour_cells = function(cell) {
      up <- list(
        row = cell$row - 1,
        column = cell$column
      )
      if (up$row < 1) {
        up <- NULL
      }

      right <- list(
        row = cell$row,
        column = cell$column + 1
      )
      if (right$column > private$width) {
        right <- NULL
      }

      down <- list(
        row = cell$row + 1,
        column = cell$column
      )
      if (down$row > private$height) {
        down <- NULL
      }

      left <- list(
        row = cell$row,
        column = cell$column - 1
      )
      if (left$column < 1) {
        left <- NULL
      }

      list(
        up = up,
        right = right,
        down = down,
        left = left
      )
    },

    next_collapse_target = function() {
      entropy_grid <- unlist(lapply(seq_len(private$width), function(column) {
        lapply(seq_len(private$height), function(row) {
          private$grid[[column]][[row]]$get_entropy()
        })
      }), recursive = FALSE)

      entropy_grid <- entropy_grid[! entropy_grid %in% c(1)]

      if (identical(length(entropy_grid), 0)) {
        return(FALSE)
      }

      smallest_entropy <- sort(unique(unlist(entropy_grid)))[1]

      if (identical(smallest_entropy, length(private$tiles))) {
        return(
          list(
              column = sample(seq_len(private$width), 1),
              row = sample(seq_len(private$height), 1)
            )
        )
      }

      next_cell <- NULL
      for (column in seq_len(private$width)) {
        for (row in seq_len(private$height)) {
          if (identical(
            private$grid[[column]][[row]]$get_entropy(),
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
    },

    new_grid = function() {
      new_grid <- vector("list", private$width)

      for (index in seq_len(private$width)) {
        new_grid[[index]] <- vector("list", private$height)
      }

      for (column in seq_len(private$width)) {
        for (row in seq_len(private$height)) {
          new_grid[[column]][[row]] <- wfc_cell(private$tiles, column, row)
        }
      }

      new_grid
    }
  ),

  public = list(
    initialize = function(width, height, tiles, rotations = TRUE) {
      private$width <- width
      private$height <- height

      private$tiles <- tiles

      # generate rotation tiles
      if (rotations) {
        for (tile in tiles) {
          if (!is.null(tile$get_rotations())) {
            for (rotation in seq_len(length(tile$get_rotations()))) {
              private$tiles <- append(
                private$tiles,
                c(tile$rotate(rotation, tile$get_rotations()[rotation]))
              )
            }
          }
        }
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
          new_grid[[column]][[row]] <- 0
        }
      }

      private$step_grid <- new_grid
    },

    step_ripple = function(ripple) {
      if (!0 %in% unique(unlist(private$step_grid))) {
        return()
      }

      purrr::walk(seq_len(private$width), function(column) {
        purrr::walk(seq_len(private$height), function(row) {
          if (identical(private$step_grid[[column]][[row]], ripple)) {
            location <- list(column = column, row = row)

            for (next_cell in private$get_neighbour_cells(location)) {
              if (!is.null(next_cell)) {
                if (identical(private$step_grid[[next_cell$column]][[next_cell$row]], 0)) {
                  private$step_grid[[next_cell$column]][[next_cell$row]] <- ripple + 1
                  if (!identical(length(private$grid[[next_cell$column]][[next_cell$row]]$get_entropy()), 1)) {
                    self$recalculate_cell_entropy(next_cell$column, next_cell$row)
                  }
                }
              }
            }
          }
        })
      })

      self$step_ripple(ripple + 1)
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

      self$step_ripple(1)

      private$add_iteration()

      print("step done")
      return(TRUE)
    },

    process_neighbour_tiles = function(cell,
                                        neighbour_cell,
                                        direction_index,
                                        oposite_direction_index) {

        # neighbour has not been at least partially collapsed
        # all options are possible
        if (identical(length(neighbour_cell$get_possible_tiles()), length(private$tiles))) { # nolint
          return()
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

        print(direction_index)
        print(neighbour_sockets)
        print(filtered_tiles)

        cell$set_possible_tiles(filtered_tiles)
    },

    recalculate_cell_entropy = function(column, row) {
      # get cell
      cell <- private$grid[[column]][[row]]

      # get neighbours
      neighbours <- list(column = column, row = row) %>%
        private$get_neighbour_cells()

      # calculate tiles based on that info
      if (!is.null(neighbours$up)) {
        neighbour_cell <- private$grid[[neighbours$up$column]][[neighbours$up$row]]
        # print(neighbour_cell)
        self$process_neighbour_tiles(cell, neighbour_cell, 1, 3)
      }
      if (!is.null(neighbours$right)) {
        neighbour_cell <- private$grid[[neighbours$right$column]][[neighbours$right$row]]
        # print(neighbour_cell)
        self$process_neighbour_tiles(cell, neighbour_cell, 2, 4)
      }
      if (!is.null(neighbours$down)) {
        neighbour_cell <- private$grid[[neighbours$down$column]][[neighbours$down$row]]
        # print(neighbour_cell)
        self$process_neighbour_tiles(cell, neighbour_cell, 3, 1)
      }
      if (!is.null(neighbours$left)) {
        neighbour_cell <- private$grid[[neighbours$left$column]][[neighbours$left$row]]
        # print(neighbour_cell)
        self$process_neighbour_tiles(cell, neighbour_cell, 4, 2)
      }
    },

    solve = function() {
      while (TRUE) {
        step <- self$step()

        if (identical(step, FALSE)) {
          print(self)
          break
        }
      }
    },

    print = function(...) {
      cat("Tiles: \n")
      for (tile in private$tiles) {
        cat(tile$get_hash(), "\n")
      }

      cat("Iterations: \n")
      cat(private$iterations, "\n")

      cat("Last Grid: \n")
      for (column in private$grid) {
        for (row in column) {
          cat(row$get_entropy(), " ")
        }
        cat("\n")
      }

      invisible(self)
    },

    print_step_grid = function() {
      cat("Step Grid: \n")
      for (column in private$step_grid) {
        for (row in column) {
          cat(row, " ")
        }
        cat("\n")
      }

      invisible(self)
    }
  )
)

#' @export
wfc_grid <- function(...) {
  wfc_grid_class$new(...)
}
