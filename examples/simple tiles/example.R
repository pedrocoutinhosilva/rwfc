library(shiny)
library(wfc)
library(imola)

tiles <- list(
    wfc_tile(" ", c("0000", "0000", "0000", "0000")),
    wfc_tile("●", c("0000", "0000", "0000", "0000")),
    wfc_tile("▩", c("1111", "1111", "1111", "1111")),
    wfc_tile("▶", c("0000", "0000", "0000", "1111"), c("▼", "◀", "▲")),
    wfc_tile("◥", c("1111", "1111", "0000", "0000"), c("◢", "◣", "◤")),
    wfc_tile("◧", c("1100", "0000", "0011", "1111"), c("⬒", "◨", "⬓")),
    wfc_tile("◱", c("0000", "0000", "0011", "1100"), c("◰", "◳", "◲"))
)
tiles <- list(
    wfc_tile(" ", c("0000", "0000", "0000", "0000")),
    wfc_tile("●", c("0000", "0000", "0000", "0000")),
    wfc_tile("▩", c("1111", "1111", "1111", "1111")),
    wfc_tile("▶", c("0000", "0000", "0000", "1111"), c("▼", "◀", "▲")),
    wfc_tile("◥", c("1111", "1111", "0000", "0000"), c("◢", "◣", "◤"))
)

socket_preview <- function(content) {
  div(
    class = "socket-preview center-content",
    content
  )
}

tile_preview <- function(content) {
  div(
    class = "tile-preview center-content",
    content
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
      list(columns = "1fr 1fr"),
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
          tile$get_data()
        )
      })
    )
  )
}


preview_cell <- function(cell) {
  if (length(cell$get_possible_tiles()) > 0) {
    content <- cell$get_possible_tiles()[[1]]$get_data()
  } else {
    "NO"
  }

  style <- "
    font-size: 71px;
    line-height: 71px;
  "

  if (cell$get_entropy() > 1) {
    content <- cell$get_entropy()
    style <- ""
  }

  div(
    class = "preview-grid-cell center-content",
    style = style,
    content
  )
}
preview_grid <- function(grid_component) {
  x <- grid_component$get_width()
  y <- grid_component$get_height()

  do.call(
    gridPanel,

    append(
      list(
        rows = paste("repeat(", x, ", 50px)"),
        columns = paste("repeat(", y, ", 50px)")
      ),
      unlist(lapply(seq_len(x), function(row) {
        lapply(seq_len(y), function(column) {
          div(
            onmouseenter = "updatePreview(this);",
            class = "preview-cell",
            `data-row` = row,
            `data-column` = column,
            preview_cell(grid_component$get_grid()[[row]][[column]])
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
  columns = "200px 1fr 200px",

  uiOutput("sidebar"),
  uiOutput("preview"),
  div(
    actionButton("restart", "Restart"),
    actionButton("step", "Step"),
    # actionButton("back", "Back"),
    actionButton("solve", "Solve"),
    uiOutput("tileOptions")
  )

)
server <- function(input, output, session) {
    final_grid <- wfc_grid(15, 15, tiles)

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
        selected_cell_options(final_grid$get_grid()[[as.integer(input$showCellTile$row)]][[as.integer(input$showCellTile$column)]])
      })
    })
}

shinyApp(ui, server)
