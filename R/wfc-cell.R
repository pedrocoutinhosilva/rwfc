#' @importFrom R6 R6Class
#' @importFrom magrittr "%>%"
wfc_cell_class <- R6Class("wfcCell",
  private = list(
    # Super position tiles for the cell
    possible_tiles = NULL,

    # Can this cell be collapsed based on its possible options
    is_impossible = FALSE,

    # Did the cell finish collapsing its value into a single tile
    collapsed = FALSE
  ),

  public = list(
    # Runs when creating a new object of this class
    initialize = function(possible_tiles) {
      private$possible_tiles <- possible_tiles

      private$collapsed <- private$possible_tiles %>%
        length() %>%
        identical(1)
    },

    # Collapses the value of a cell to a single tile
    collapse = function() {
      private$possible_tiles <- private$possible_tiles %>%
        sample(1)

      # if only one tile is left, mark it as collapsed
      private$collapsed <- private$possible_tiles %>%
        length() %>%
        identical(1)
    },

    # Get functions for the object data
    get_entropy = function() {
      if (private$collapsed) {
        return(NULL)
      }

      length(private$possible_tiles)
    },

    get_possible_tiles = function() {
      private$possible_tiles
    },

    # Set functions for the object data
    set_possible_tiles = function(tiles) {
      private$possible_tiles <- tiles
    },

    # Custom print for displaying the object in the console
    print = function(...) {
      cat("Cell: \n")
      cat("  possible_tiles: ", self$get_entropy(), "\n", sep = "")
      cat("  collapsed: ", private$collapsed, "\n", sep = "")

      invisible(self)
    }
  )
)

#' @export
wfc_cell <- function(tiles) {
  wfc_cell_class$new(tiles)
}
