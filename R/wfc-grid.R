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
      # print(paste("ripple:", ripple))
      # print(next_ripple_cells)
      # start.time <- Sys.time()

      # print(next_ripple_cells)
      if (length(next_ripple_cells) == 0 || !0 %in% unique(unlist(private$step_grid)) || ripple == 4) {
        # print("time to finish")
        # print(next_ripple_cells)
        return()
      }

      # print(Sys.time() - start.time)
      # start.time <- Sys.time()

      temp_next_ripple_cells <- list()

      for (next_ripple_cell in next_ripple_cells) {
        for (next_cell in private$get_neighbour_cells(next_ripple_cell)) {
          if (!is.null(next_cell)) {
            if (identical(private$step_grid[[next_cell$column]][[next_cell$row]], 0)) {
              private$step_grid[[next_cell$column]][[next_cell$row]] <- ripple + 1

              if (!identical(length(private$grid[[next_cell$column]][[next_cell$row]]$get_entropy()), 1)) {
                self$recalculate_cell_entropy(next_cell$column, next_cell$row)
              }

              temp_next_ripple_cells <- append(
                temp_next_ripple_cells,
                list(next_cell)
              )
            }
          }
        }
      }

      # self$print_step_grid()

      # purrr::walk(seq_len(private$width), function(column) {
      #   purrr::walk(seq_len(private$height), function(row) {
      #     if (identical(private$step_grid[[column]][[row]], ripple)) {
      #       location <- list(column = column, row = row)

      #       # private$step_grid[[column]][[row]]$reduce_entropy()

      #       for (next_cell in private$get_neighbour_cells(location)) {
      #         if (!is.null(next_cell)) {
      #           if (identical(private$step_grid[[next_cell$column]][[next_cell$row]], 0)) {
      #             private$step_grid[[next_cell$column]][[next_cell$row]] <- ripple + 1
      #             if (!identical(length(private$grid[[next_cell$column]][[next_cell$row]]$get_entropy()), 1)) {
      #               self$recalculate_cell_entropy(next_cell$column, next_cell$row)
      #             }
      #           }
      #         }
      #       }
      #     }
      #   })
      # })
      # print(Sys.time() - start.time)

      self$step_ripple(ripple + 1, temp_next_ripple_cells)
    },

    step = function() {
      # start.time <- Sys.time()

      # get random lowest entropy random
      starter_cell <- private$next_collapse_target()

      # print("next_collapse_target")
      # print(Sys.time() - start.time)
      # start.time <- Sys.time()

      # No more starter cells. Its collapsed
      if (identical(starter_cell, FALSE)) {
        return(FALSE)
      }

      self$reset_step_grid()

      # print("reset_step_grid")
      # print(Sys.time() - start.time)
      # start.time <- Sys.time()

      private$grid[[starter_cell$column]][[starter_cell$row]]$collapse()
      private$step_grid[[starter_cell$column]][[starter_cell$row]] <- 1

      # print("collapse")
      # print(Sys.time() - start.time)
      # start.time <- Sys.time()

      self$step_ripple(1, list(starter_cell))

      # print("step_ripple")
      # print(Sys.time() - start.time)
      # start.time <- Sys.time()

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

    allowed_tiles_from_neighbout = function(cell,
                                            neighbour_cell,
                                            direction_index,
                                            oposite_direction_index) {
      # neighbour has not been at least partially collapsed
      # all options are possible
      if (identical(neighbour_cell$get_entropy(), length(private$tiles))) { # nolint
        return(cell$get_possible_tiles())
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

      return(filtered_tiles)
    },

    recalculate_cell_entropy = function(column, row) {
      # get cell
      cell <- private$grid[[column]][[row]]

      # get neighbours
      neighbours <- list(column = column, row = row) %>%
        private$get_neighbour_cells()

      possible_tiles <- list(
        up = NULL,
        right = NULL,
        down = NULL,
        left = NULL
      )

      # self$print_step_grid()
      # self$print_entropy_grid()

      # calculate tiles based on that info
      if (!is.null(neighbours$up)) {
        neighbour_cell <- private$grid[[neighbours$up$column]][[neighbours$up$row]]
        # print(neighbour_cell)
        self$process_neighbour_tile(cell, neighbour_cell, 1, 3)
      }
      if (!is.null(neighbours$right)) {
        neighbour_cell <- private$grid[[neighbours$right$column]][[neighbours$right$row]]
        # print(neighbour_cell)
        self$process_neighbour_tile(cell, neighbour_cell, 2, 4)
      }
      if (!is.null(neighbours$down)) {
        neighbour_cell <- private$grid[[neighbours$down$column]][[neighbours$down$row]]
        # print(neighbour_cell)
        self$process_neighbour_tile(cell, neighbour_cell, 3, 1)
      }
      if (!is.null(neighbours$left)) {
        neighbour_cell <- private$grid[[neighbours$left$column]][[neighbours$left$row]]
        # print(neighbour_cell)
        self$process_neighbour_tile(cell, neighbour_cell, 4, 2)
      }

      # # calculate tiles based on that info
      # if (!is.null(neighbours$up)) {
      #   neighbour_cell <- private$grid[[neighbours$up$column]][[neighbours$up$row]]
      #   # print(neighbour_cell)
      #   possible_tiles$up <- self$allowed_tiles_from_neighbout(cell, neighbour_cell, 1, 3) %>%
      #     purrr::map(function(tile) {
      #       tile$get_hash()
      #     }) %>% unlist()
      # }
      # if (!is.null(neighbours$right)) {
      #   neighbour_cell <- private$grid[[neighbours$right$column]][[neighbours$right$row]]
      #   # print(neighbour_cell)
      #   possible_tiles$right <- self$allowed_tiles_from_neighbout(cell, neighbour_cell, 2, 4) %>%
      #     purrr::map(function(tile) {
      #       tile$get_hash()
      #     }) %>% unlist()
      # }
      # if (!is.null(neighbours$down)) {
      #   neighbour_cell <- private$grid[[neighbours$down$column]][[neighbours$down$row]]
      #   # print(neighbour_cell)
      #   possible_tiles$down <- self$allowed_tiles_from_neighbout(cell, neighbour_cell, 3, 1) %>%
      #     purrr::map(function(tile) {
      #       tile$get_hash()
      #     }) %>% unlist()
      # }
      # if (!is.null(neighbours$left)) {
      #   neighbour_cell <- private$grid[[neighbours$left$column]][[neighbours$left$row]]
      #   # print(neighbour_cell)
      #   possible_tiles$left <- self$allowed_tiles_from_neighbout(cell, neighbour_cell, 4, 2) %>%
      #     purrr::map(function(tile) {
      #       tile$get_hash()
      #     }) %>% unlist()
      # }

      # print(possible_tiles)
      # purrr::keep(cell$get_possible_tiles(), function(tile){
      #   if (is.null(possible_tiles$up) || tile$get_hash() %in% possible_tiles$up || possible_tiles$up == length(private$tiles)) {
      #       if (is.null(possible_tiles$right) || tile$get_hash() %in% possible_tiles$right || possible_tiles$up == length(private$tiles)) {
      #           if (is.null(possible_tiles$down) || tile$get_hash() %in% possible_tiles$down || possible_tiles$up == length(private$tiles)) {
      #               if (is.null(possible_tiles$left) || tile$get_hash() %in% possible_tiles$left || possible_tiles$up == length(private$tiles)) {
      #                   return(TRUE)
      #               }
      #           }
      #       }
      #   }

      #   return(FALSE)
      # }) %>% cell$set_possible_tiles()

      # new_cell_tiles <- possible_tiles$up
      # if (length(possible_tiles$right) != 0 && length(possible_tiles$right) < length(new_cell_tiles)) {
      #   new_cell_tiles <- possible_tiles$right
      # }
      # if (length(possible_tiles$down) != 0 && length(possible_tiles$down) < length(new_cell_tiles)) {
      #   new_cell_tiles <- possible_tiles$right
      # }
      # if (length(possible_tiles$left) != 0 && length(possible_tiles$left) < length(new_cell_tiles)) {
      #   new_cell_tiles <- possible_tiles$right
      # }
    },

    solve = function() {
      start.time <- Sys.time()

      while (TRUE) {
        step <- self$step()

        if (identical(step, FALSE)) {
          print(self)
          print("solve")
          print(Sys.time() - start.time)
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
      for (row in seq_len(length(private$grid[[1]]))) {
        for (column in seq_len(length(private$grid))) {
          cat(private$grid[[column]][[row]]$get_entropy(), " ")
        }
        cat("\n")
      }

      invisible(self)
    },

    print_step_grid = function() {
      cat("Step Grid: \n")
      for (row in seq_len(length(private$step_grid[[1]]))) {
        for (column in seq_len(length(private$step_grid))) {
          cat(private$step_grid[[column]][[row]], " ")
        }
        cat("\n")
      }

      invisible(self)
    },

    print_entropy_grid = function() {
      cat("Entropy Grid: \n")
      for (row in seq_len(length(private$step_grid[[1]]))) {
        for (column in seq_len(length(private$step_grid))) {
          cat(private$grid[[column]][[row]]$get_entropy(), " ")
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
