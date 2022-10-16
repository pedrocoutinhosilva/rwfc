#' @importFrom R6 R6Class
wfc_tile_class <- R6Class("wfcTile",
  private = list(
    # A unique identifier for the tile
    hash = NULL,

    # The tile content. Can be anything that makes the
    # tile a tile (image data, string, number)
    data = NULL,

    # Data variations of the tile. Should be a vector of aditional data that,
    # when provided, will allow the grid
    # To automatically generate additional tiles based on the rotations. Each
    # element of the vector should be the data
    # for a 90 degree clockwise rotation of the previous element.
    # First element will be a 90 degree version of the tile second will be a
    # 180, third a 270.
    rotations = NULL,

    # Key identifier for each side of the the tile.
    # Socket keys represent what tile sides in all the tiles in the set can
    # connect to each other
    # Should be a vector of values with the value of each socket in this tile.
    # Values should be ordered with UP value being first and next
    # values moving clockwise. The socket value should ALWAYS be clockwise
    # even for the socket string
    # For a 4 sided tile, this means: c(UP, RIGHT, DOWN, LEFT)
    sockets = NULL
  ),

  public = list(
    # Runs when creating a new object of this class
    initialize = function(data, sockets, rotations = NULL) {
      private$hash <- generate_hash()

      private$data <- data
      private$sockets <- sockets
      private$rotations <- rotations
    },

    # Generates a new tile as a rotated version of this tile
    rotate = function(num_rotations, data) {
      hash <- generate_hash()
      sockets <- private$sockets

      # Sockets for the rotated version are created by moving the
      # original sockets num_rotations to the right
      for (iteration in seq_len(num_rotations)) {
        sockets <- c(tail(sockets, 1), head(sockets, -1))
      }

      wfc_tile(data, sockets)
    },

    # Get functions for the object data
    get_hash = function() {
      private$hash
    },
    get_sockets = function() {
      private$sockets
    },
    get_data = function() {
      private$data
    },
    get_rotations = function() {
      private$rotations
    },

    # Custom print for displaying the object in the console
    print = function(...) {
      cat("Cell: \n")
      cat("  Hash: ", private$hash, "\n", sep = "")
      cat("  data: ", private$data, "\n", sep = "")
      cat("  sockets: ", private$sockets, "\n", sep = "")
      cat("  rotations: ", private$rotations, "\n", sep = "")

      invisible(self)
    }
  )
)

#' @export
wfc_tile <- function(data, sockets, rotations = NULL) {
  wfc_tile_class$new(data, sockets, rotations)
}
