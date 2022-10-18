library(shiny)
library(wfc)
library(imola)
library(imager)
# library(reactlog)

# reactlog_enable()

tiles <- list(
    wfc_tile("tile020", c("AAA", "ABC", "CBA", "AAA"), c("tile022", "tile060", "tile058")), #nolint
    wfc_tile("tile021", c("AAA", "ABC", "CCC", "CBA"), c("tile041", "tile059", "tile039")), #nolint
    wfc_tile("tile024", c("AAA", "AAA", "ABA", "AAA"), c("tile098", "tile062", "tile096")), #nolint
    wfc_tile("tile026", c("CCC", "CBA", "ABC", "CCC"), c("tile027", "tile046", "tile045")), #nolint
    wfc_tile("tile029", c("ABC", "CCC", "CBA", "ABA"), c("tile030", "tile049", "tile048")), #nolint
    wfc_tile("tile032", c("CBA", "ABA", "ABA", "ABC"), c("tile033", "tile052", "tile051")), #nolint
    wfc_tile("tile035", c("ABA", "ABA", "ABA", "ABA")),
    wfc_tile("tile040", c("CCC", "CCC", "CCC", "CCC")),
    wfc_tile("tile043", c("ABA", "AAA", "ABA", "AAA")),
    wfc_tile("tile054", c("CBA", "ABC", "CBA", "ABC"), c("tile055")),
    wfc_tile("tile083", c("AAA", "ABA", "ABC", "CBA"), c("tile087", "tile103", "tile105")), #nolint
    wfc_tile("tile084", c("AAA", "ABC", "CBA", "ABA"), c("tile106", "tile102", "tile086")), #nolint
    wfc_tile("tile089", c("AAA", "ABA", "ABA", "ABA"), c("tile090", "tile109", "tile108")), #nolint
    wfc_tile("tile092", c("AAA", "ABA", "ABA", "AAA"), c("tile093", "tile112", "tile111")), #nolint
    wfc_tile("tile097", c("AAA", "ABA", "AAA", "ABA"), c("tile097_1")),
    wfc_tile("tile100", c("AAA", "AAA", "AAA", "AAA"))
)

source_url <- function(content) {
  paste0("tileset/", content, ".png")
}

socket_preview <- function(content) {
  div(
    class = "socket-preview center-content",
    content
  )
}

tile_preview <- function(content) {
  div(
    class = "tile-preview center-content",
    tags$img(src = source_url(content))
  )
}

tile_details <- function(tile) {
  sockets <- tile$get_sockets()

  gridPanel(
    class = "tile-details",
    rows = "12px 1fr 12px",
    columns = "12px 1fr 12px",

    areas = c(
      "...  up      ...",
      "left preview right",
      "...  down    ..."
    ),

    up = socket_preview(sockets[1]),
    right = socket_preview(sockets[2]) |>
      tagAppendAttributes(style = "transform: rotate(90deg);"),
    down = socket_preview(sockets[3]) |>
      tagAppendAttributes(style = "transform: rotate(180deg);"),
    left = socket_preview(sockets[4]) |>
      tagAppendAttributes(style = "transform: rotate(-90deg);"),

    preview = tile_preview(tile$get_data())
  )
}

sidebar <- function(tiles) {
  do.call(
    gridPanel,
    append(
      list(columns = "1fr 1fr 1fr 1fr 1fr"),
      lapply(seq_len(length(tiles)), function(tile) {
        tile_details(tiles[[tile]])
      })
    )
  )
}

selected_cell_options <- function(cell) {
  div(
    p(
      class = "cell-details-title",
      "Over cell to preview tiles"
    ),
    div(
      class = "cell-details-tiles-wrapper",
      lapply(cell$get_possible_tiles(), function(tile) {
        div(
          class = "cell-details-tile center-content",
          tags$img(
            style = "height: 100%; width: 100%;",
            src = source_url(tile$get_data())
          )
        )
      })
    )
  )
}


preview_cell <- function(cell) {
  if (length(cell$get_possible_tiles()) > 0) {
    content <- tags$img(
      style = "height: 100%; width: 100%;",
      src = source_url(cell$get_possible_tiles()[[1]]$get_data())
    )
  } else {
    content <- "NO"
  }

  style <- "
    font-size: 71px;
    line-height: 71px;
  "

  if (cell$get_entropy() > 1) {
    content <- cell$get_entropy()
    style <- ""
  }

  if (identical(content, "NO")) {
    style <- ""
  }

  div(
    class = "preview-grid-cell center-content",
    style = style,

    content
  )
}
preview_grid <- function(grid_component) {
  num_columns <- grid_component$get_width()
  num_rows <- grid_component$get_height()

  do.call(
    gridPanel,

    append(
      list(
        rows = paste("repeat(", num_rows, ", 50px)"),
        columns = paste("repeat(", num_columns, ", 50px)"),
        tags$script("triggerAutoStep();")
      ),
      unlist(lapply(seq_len(num_rows), function(row) {
        lapply(seq_len(num_columns), function(column) {
          div(
            onmouseenter = "updatePreview(this);",
            class = "preview-cell",
            `data-row` = row,
            `data-column` = column,
            preview_cell(grid_component$get_grid()[[column]][[row]])
          )
        })
      }), recursive = FALSE)
    )
  )
}

ui <- gridPage(
  tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  tags$script(src = "script.js"),

  gap = "15px",
  columns = "500px 1fr 200px",

  uiOutput("sidebar"),
  uiOutput("preview"),
  div(
    actionButton("restart", "Restart"),
    actionButton("step", "Step"),
    actionButton("solve", "Solve"),
    checkboxInput("autoStep", "Auto Step"),
    uiOutput("tileOptions")
  )

)
server <- function(input, output, session) {
    final_grid <- wfc_grid(12, 12, tiles)

    output$preview <- renderUI({
        preview_grid(final_grid)
    })

    output$sidebar <- renderUI({
        sidebar(final_grid$get_tiles())
    })

    observeEvent(input$restart, {
        final_grid$reset()

        output$preview <- renderUI({
          preview_grid(final_grid)
        })
    })

    observeEvent(input$step, {
      final_grid$step()

      output$preview <- renderUI({
          preview_grid(final_grid)
      })
    })

    observeEvent(input$renderFinished, {
      if (input$autoStep) {
        final_grid$step()

        output$preview <- renderUI({
            preview_grid(final_grid)
        })
      }
    })


    observeEvent(input$back, {
      final_grid$step_back()

      print(final_grid)

      output$preview <- renderUI({
        preview_grid(final_grid)
      })
    })


    observeEvent(input$solve, {
        final_grid$solve()

        output$preview <- renderUI({
            preview_grid(final_grid)
        })
    })

    observeEvent(input$showCellTile, {
      output$tileOptions <- renderUI({
        selected_cell_options(final_grid$get_grid()[[as.integer(input$showCellTile$column)]][[as.integer(input$showCellTile$row)]])
      })
    })
}

shinyApp(ui, server)
