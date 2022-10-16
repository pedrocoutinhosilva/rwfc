#' @importFrom R6 R6Class
#' @importFrom purrr keep
#' @importFrom stringi stri_reverse
wfc_grid_class <- R6Class("wcfGrid",
  private = list(
    height = NULL,
    width = NULL,
    grid = NULL,

    iterations = 0,

    tiles = NULL,

    add_iteration = function(grid) {
      private$iterations <- private$iterations + 1
    },

    remove_iteration = function() {
      # purrr::map(seq_len(private$width), function(column) {
      #     purrr::map(seq_len(private$height), function(row) {
      #         private$grid[[column]][[row]]$step_back()
      #     })
      # })

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

    newEntropyGrid = function() {
      height <- private$width
      width <- private$width

      new_grid <- vector("list", width)

      for (index in seq_len(width)) {
        new_grid[[index]] <- vector("list", height)
      }

      for (column in seq_len(width)) {
        for (row in seq_len(height)) {
          new_grid[[column]][[row]] <- FALSE
        }
      }

      new_grid
    },

    updateEntropy = function(entropy_grid, cell) {
      affected_cells <- private$get_neighbour_cells(cell)

      if (!is.null(affected_cells$up)) {
        entropy_grid[[affected_cells$up$row]][[affected_cells$up$column]]$reduce_entropy() # nolint
      }


      # for (cell in affected_cells) {
      #    if (!is.null(cell)) {
      #     entropy_grid[[cell$row]][[cell$column]] <- TRUE
      #     )
      #    }
      # }
    },

    next_collapse_target = function() {
      entropy_grid <- unlist(lapply(seq_len(private$width), function(row) {
        lapply(seq_len(private$height), function(column) {
          private$grid[[row]][[column]]$get_entropy()
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
              row = sample(seq_len(private$height), 1),
              column = sample(seq_len(private$width), 1)
            )
        )
      }

      next_cell <- NULL
      for (column in seq_len(private$width)) {
        for (row in seq_len(private$height)) {
          if (identical(
            private$grid[[row]][[column]]$get_entropy(),
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
  ),

  public = list(
    initialize = function(width, height, tiles, rotations = TRUE) {
      private$width <- width
      private$height <- height

      private$tiles <- tiles

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

      new_grid <- vector("list", width)

      for (index in seq_len(width)) {
        new_grid[[index]] <- vector("list", height)
      }

      for (column in seq_len(width)) {
        for (row in seq_len(height)) {
          new_grid[[column]][[row]] <- wfc_cell(private$tiles)
        }
      }

      private$grid <- new_grid
      private$add_iteration(new_grid)
    },

    reset = function() {
      private$iterations <- 0

      width <- private$width
      height <- private$height

      new_grid <- vector("list", width)

      for (index in seq_len(width)) {
        new_grid[[index]] <- vector("list", height)
      }

      for (column in seq_len(width)) {
        for (row in seq_len(height)) {
          new_grid[[column]][[row]] <- wfc_cell(private$tiles)
        }
      }

      private$grid <- new_grid
      private$add_iteration(new_grid)
    },

    get_width = function() {
      length(private$grid)
    },

    get_height = function() {
      length(private$grid[[1]])
    },

    get_grid = function() {
      private$grid
    },

    get_tiles = function() {
      private$tiles
    },

    step_back = function() {
      private$remove_iteration()
    },

    reset_entropy = function(starter_cell) {
      private$grid[[starter_cell$row]][[starter_cell$column]]$set_possible_tiles(private$tiles)

      affected_cells <- private$get_neighbour_cells(starter_cell)

      if (!is.null(affected_cells$up)) {
        target_cell <- private$grid[[affected_cells$up$row]][[affected_cells$up$column]] # nolint

        # get the required socket value of the neighbour cell
        socket <- target_cell$get_possible_tiles()[[1]]$get_sockets()[3]

        filtered_tiles <- purrr::keep(
          private$grid[[starter_cell$row]][[starter_cell$column]]$get_possible_tiles(),
          function(tile) {
            identical(tile$get_sockets()[[1]], stringi::stri_reverse(socket))
          }
        )

        private$grid[[starter_cell$row]][[starter_cell$column]]$set_possible_tiles(filtered_tiles)

        if (length(filtered_tiles) < 1) {
          return(FALSE)
        }
      }

      if (!is.null(affected_cells$down)) {
        target_cell <- private$grid[[affected_cells$down$row]][[affected_cells$down$column]] # nolint

        # get the required socket value of the neighbour cell
        socket <- target_cell$get_possible_tiles()[[1]]$get_sockets()[1]

        filtered_tiles <- purrr::keep(
          private$grid[[starter_cell$row]][[starter_cell$column]]$get_possible_tiles(),
          function(tile) {
            identical(tile$get_sockets()[[3]], stringi::stri_reverse(socket))
          }
        )

        private$grid[[starter_cell$row]][[starter_cell$column]]$set_possible_tiles(filtered_tiles)

        if (length(filtered_tiles) < 1) {
          return(FALSE)
        }
      }

      if (!is.null(affected_cells$right)) {
        target_cell <- private$grid[[affected_cells$right$row]][[affected_cells$right$column]] # nolint

        # get the required socket value of the neighbour cell
        socket <- target_cell$get_possible_tiles()[[1]]$get_sockets()[2]

        filtered_tiles <- purrr::keep(
          private$grid[[starter_cell$row]][[starter_cell$column]]$get_possible_tiles(),
          function(tile) {
            identical(tile$get_sockets()[[4]], stringi::stri_reverse(socket))
          }
        )

        private$grid[[starter_cell$row]][[starter_cell$column]]$set_possible_tiles(filtered_tiles)

        if (length(filtered_tiles) < 1) {
          return(FALSE)
        }
      }

      if (!is.null(affected_cells$left)) {
        target_cell <- private$grid[[affected_cells$left$row]][[affected_cells$left$column]] # nolint

        # get the required socket value of the neighbour cell
        socket <- target_cell$get_possible_tiles()[[1]]$get_sockets()[4]

        filtered_tiles <- purrr::keep(
          private$grid[[starter_cell$row]][[starter_cell$column]]$get_possible_tiles(),
          function(tile) {
            identical(tile$get_sockets()[[2]], stringi::stri_reverse(socket))
          }
        )

        private$grid[[starter_cell$row]][[starter_cell$column]]$set_possible_tiles(filtered_tiles)

        if (length(filtered_tiles) < 1) {
          return(FALSE)
        }
      }

      print("bail success")
      return(TRUE)
    },

    step = function() {
      # get random lowest entropy random
      starter_cell <- private$next_collapse_target()

      # No more starter cells. Its collapsed
      if (identical(starter_cell, FALSE)) {
        return(FALSE)
      }

      if (length(private$grid[[starter_cell$row]][[starter_cell$column]]$get_possible_tiles()) < 1) {
        print("bail")

        reset_result <- self$reset_entropy(starter_cell)

        if (!reset_result) {
          print("bail failed")
          return(FALSE)
        }
      }

      # collapse the starting element
      temp_grid <- private$grid
      temp_grid[[starter_cell$row]][[starter_cell$column]]$collapse()

      # keep track of cells that need collapsing (neighbours of updated cells)
      # start with neighbours of the starting cell
      # UP RIGHT DOWN LEFT
      # entropy_grid <- private$newEntropyGrid()
      # entropy_grid <- private$updateEntropy(entropy_grid, starter_cell)

      # Get affected cells based on collased cell
      affected_cells <- private$get_neighbour_cells(starter_cell)

      starter_cell <- temp_grid[[starter_cell$row]][[starter_cell$column]]
      # update up cell rules regarding down
      if (!is.null(affected_cells$up)) {
        target_cell <- temp_grid[[affected_cells$up$row]][[affected_cells$up$column]] # nolint

        # get the required socket value of the collapsed cell
        socket <- starter_cell$get_possible_tiles()[[1]]$get_sockets()[1]

        filtered_tiles <- purrr::keep(
          target_cell$get_possible_tiles(),
          function(tile) {
            identical(tile$get_sockets()[[3]], stringi::stri_reverse(socket))
          }
        )

        temp_grid[[affected_cells$up$row]][[affected_cells$up$column]]$set_possible_tiles(filtered_tiles)

        if (length(filtered_tiles) < 1) {
          return(FALSE)
        }
      }
      # update right cell rules regarding left
      if (!is.null(affected_cells$right)) {
        target_cell <- temp_grid[[affected_cells$right$row]][[affected_cells$right$column]] # nolint

        # get the required socket value of the collapsed cell
        socket <- starter_cell$get_possible_tiles()[[1]]$get_sockets()[2]

        filtered_tiles <- purrr::keep(
          target_cell$get_possible_tiles(),
          function(tile) {
            identical(tile$get_sockets()[[4]], stringi::stri_reverse(socket))
          }
        )

        temp_grid[[affected_cells$right$row]][[affected_cells$right$column]]$set_possible_tiles(filtered_tiles)

        if (length(filtered_tiles) < 1) {
          return(FALSE)
        }
      }
      # update down cell rules regarding up
      if (!is.null(affected_cells$down)) {
        target_cell <- temp_grid[[affected_cells$down$row]][[affected_cells$down$column]] # nolint

        # get the required socket value of the collapsed cell
        socket <- starter_cell$get_possible_tiles()[[1]]$get_sockets()[3]

        filtered_tiles <- purrr::keep(
          target_cell$get_possible_tiles(),
          function(tile) {
            identical(tile$get_sockets()[[1]], stringi::stri_reverse(socket))
          }
        )

        temp_grid[[affected_cells$down$row]][[affected_cells$down$column]]$set_possible_tiles(filtered_tiles)

        if (length(filtered_tiles) < 1) {
          return(FALSE)
        }
      }
      # update left cell rules regarding right
      if (!is.null(affected_cells$left)) {
        target_cell <- temp_grid[[affected_cells$left$row]][[affected_cells$left$column]] # nolint

        # get the required socket value of the collapsed cell
        socket <- starter_cell$get_possible_tiles()[[1]]$get_sockets()[4]

        filtered_tiles <- purrr::keep(
          target_cell$get_possible_tiles(),
          function(tile) {
            identical(tile$get_sockets()[[2]], stringi::stri_reverse(socket))
          }
        )

        temp_grid[[affected_cells$left$row]][[affected_cells$left$column]]$set_possible_tiles(filtered_tiles)

        if (length(filtered_tiles) < 1) {
          return(FALSE)
        }
      }

      # print(entropy_grid)

      # for each neighbour tile, check the possible options that tile allows
      # and remove possible tiles from the tiles that do not comply

      # collapse remaining grid

      # store new grid
      # Update grid
      private$grid <- temp_grid
      private$add_iteration(temp_grid)

      print("step done")
      return(TRUE)
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
      for (row in private$grid) {
        for (column in row) {
          cat(column$get_entropy(), " ")
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
