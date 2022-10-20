library(shiny)
library(wfc)
library(imola)
library(imager)
library(shinyjs)

tilesets <- list(
  walls = list(
    label = "Walls and brick",
    tiles = list(
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
    ),
    tile_render_callback = function(content) {
      tags$img(
        style = "height: 100%; width: 100%;",
        src = paste0("tileset/", content, ".png")
      )
    }
  ),
  abstract = list(
    label = "Abstract Shapes",
    tiles = list(
        wfc_tile(" ", c("0000", "0000", "0000", "0000")),
        wfc_tile("●", c("0000", "0000", "0000", "0000")),
        wfc_tile("▩", c("1111", "1111", "1111", "1111")),
        wfc_tile("▶", c("0000", "0000", "0000", "1111"), c("▼", "◀", "▲")),
        wfc_tile("◥", c("1111", "1111", "0000", "0000"), c("◢", "◣", "◤"))
    ),
    tile_render_callback = function(content) {
      content
    }
  )
)

socket_preview <- function(content) {
  div(
    class = "socket-preview center-content",
    content
  )
}

tile_preview <- function(content, render_callback) {
  div(
    class = "tile-preview center-content",
    render_callback(content)
  )
}

tile_details <- function(tile, render_callback) {
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

    preview = tile_preview(tile$get_data(), render_callback)
  )
}

tile_list <- function(tiles, render_callback) {
  do.call(
    flexPanel,
    append(
      list(flex = c("unset"), wrap = "wrap", gap = "18px"),
      lapply(seq_len(length(tiles)), function(tile) {
        tile_details(tiles[[tile]], render_callback)
      })
    )
  )
}

selected_cell_options <- function(cell, render_callback) {
  if (is.null(cell)) {
    return(div(
      class = "cell-details-title",
      "Hover a cell to preview its possible tiles"
    ))
  }

  div(
    div(
      class = "cell-details-tiles-wrapper",
      lapply(cell$get_possible_tiles(), function(tile) {
        div(
          class = "cell-details-tile center-content",
          render_callback(tile$get_data())
        )
      })
    )
  )
}


preview_cell <- function(cell, render_callback) {
  if (length(cell$get_possible_tiles()) > 0) {
    content <- render_callback(cell$get_possible_tiles()[[1]]$get_data())
  } else {
    content <- "NO"
  }

  style <- "
    font-size: 47px;
    line-height: 100%;
  "

  if (cell$get_entropy() > 1) {
    content <- cell$get_entropy()
    style <- "
      border: 1px dashed #9497a5;
      font-size: 1.2rem;

    "
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
preview_grid <- function(grid_component, render_callback) {
  num_columns <- grid_component$get_width()
  num_rows <- grid_component$get_height()

  do.call(
    gridPanel,

    append(
      list(
        rows = paste0("repeat(", num_rows, ", 36px)"),
        columns = paste0("repeat(", num_columns, ", 36px)")
      ),
      unlist(lapply(seq_len(num_rows), function(row) {
        lapply(seq_len(num_columns), function(column) {
          div(
            onmouseenter = "updatePreview(this);",
            onmouseleave = "updatePreview({dataset: {row: 0, column: 0}});",
            class = "preview-cell",
            `data-row` = row,
            `data-column` = column,
            preview_cell(
              grid_component$get_grid()[[column]][[row]],
              render_callback
            )
          )
        })
      }), recursive = FALSE)
    )
  )
}

ui <- gridPage(
  tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  tags$script(src = "script.js"),
  HTML('
    <link rel="preconnect" href="https://fonts.googleapis.com">
    <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
    <link href="https://fonts.googleapis.com/css2?family=Noto+Sans&display=swap" rel="stylesheet">
  '),

  gap = "30px",
  rows = "40px 85px minmax(0, 1fr)",
  columns = "200px minmax(0, 1fr)",

  areas = c(
    "header header",
    "sidebar main",
    "sidebar main"
  ),

  header = div("Wave function collapse in R / Shiny"),
  sidebar = div(
    sliderInput(
      "gridWidth",
      "Grid Width",
      20,
      min = 1,
      max = 50,
      width = "100%"
    ),
    sliderInput(
      "gridHeight",
      "Grid Height",
      12,
      min = 1,
      max = 50,
      width = "100%"
    ),

    div(class = "separator"),

    selectInput("tileset", "Change tileset", list(
      `Walls and Brick` = "walls",
      `Abstract Shapes` = "abstract"
    )),
    actionButton("showTiles", "View all available tiles"),

    div(class = "separator"),

    actionButton("restart", "Restart"),

    div(class = "separator"),

    tags$label("Solve Grid"),
    actionButton("step", "Collapse next cell"),
    actionButton("solve", "Collapse all cells"),

    div(class = "separator"),

    uiOutput("tileOptions")
  ),
  main = uiOutput("preview")
)

new_preview_grid <- function(width, height) {
  new_grid <- vector("list", width)

  for (index in seq_len(width)) {
    new_grid[[index]] <- vector("list", height)
  }

  for (column in seq_len(width)) {
    for (row in seq_len(height)) {
      new_grid[[column]][[row]] <- 0
    }
  }

  new_grid
}

update_preview_cells <- function(grid_component,
                                  render_callback,
                                  container,
                                  grid_preview,
                                  session) {

  num_columns <- grid_component$get_width()
  num_rows <- grid_component$get_height()

  for (row in seq_len(num_rows)) {
    for (column in seq_len(num_columns)) {

      cell <- grid_component$get_grid()[[column]][[row]]

      hash <- length(cell$get_possible_tiles())
      if (cell$get_entropy() == 1) {
        hash <- cell$get_possible_tiles()[[1]]$get_hash()
      }

      if (grid_preview[[column]][[row]] != hash) {
        grid_preview[[column]][[row]] <- hash

        session$sendCustomMessage("updateCell", list(
          container = container,
          cell = list(
            row = row,
            column = column
          ),
          content = preview_cell(
            grid_component$get_grid()[[column]][[row]],
            render_callback
          ) |> as.character()
        ))
      }
    }
  }

  session$sendCustomMessage("triggerAutoStep", "trigger")

  grid_preview
}

server <- function(input, output, session) {

  settings <- reactiveValues(
    tiles = NULL,
    tile_render_callback = NULL,
    final_grid = NULL,
    grid_preview = NULL,
    autoStepping = FALSE
  )

  grid_height <- debounce(reactive({
    input$gridHeight
  }), 200)
  grid_width <- debounce(reactive({
    input$gridWidth
  }), 200)


  restart <- function() {
    session$sendCustomMessage("toggleAutoTrigger", list(value = FALSE))
    settings$autoStepping <- FALSE

    width <- input$gridWidth
    height <- input$gridHeight

    settings$tiles <- tilesets[[input$tileset]]$tiles

    render_callback <- tilesets[[input$tileset]]$tile_render_callback
    settings$tile_render_callback <- render_callback

    settings$final_grid <- wfc_grid(width, height, settings$tiles)

    settings$grid_preview <- new_preview_grid(width, height)

    output$preview <- renderUI({
        preview_grid(settings$final_grid, settings$tile_render_callback)
    })

    output$tileList <- renderUI({
        tile_list(
          settings$final_grid$get_tiles(),
          settings$tile_render_callback
        )
    })

    outputOptions(output, "tileList", suspendWhenHidden = FALSE)
  }

  observeEvent(c(input$tileset, input$restart, grid_height(), grid_width()), {
    restart()
  })

  observeEvent(input$step, {
    session$sendCustomMessage("toggleAutoTrigger", list(value = FALSE))

    if (settings$final_grid$is_finished == TRUE) {
      return()
    }

    settings$final_grid$step()

    settings$grid_preview <- update_preview_cells(
      settings$final_grid,
      settings$tile_render_callback,
      "preview",
      settings$grid_preview,
      session
    )
  })

  observeEvent(input$solve, {
    if (settings$final_grid$is_finished == TRUE) {
      return()
    }

    settings$autoStepping <- TRUE

    if (settings$autoStepping) {
      session$sendCustomMessage("toggleAutoTrigger", list(value = TRUE))
      session$sendCustomMessage("triggerAutoStep", "trigger")
    }
  })
  observeEvent(input$autoTrigger, {
    if (!settings$autoStepping) {
      return()
    }

    if (!input$autoTrigger) {
      return()
    }

    if (settings$final_grid$is_finished == TRUE) {
      settings$autoStepping <- FALSE
      session$sendCustomMessage("toggleAutoTrigger", list(value = FALSE))
      return()
    }

    settings$final_grid$step()

    settings$grid_preview <- update_preview_cells(
      settings$final_grid,
      settings$tile_render_callback,
      "preview",
      settings$grid_preview,
      session
    )
  })

  output$tileOptions <- renderUI({
    selected_cell_options(NULL, NULL)
  })
  previous_preview <- NULL

  observeEvent(input$showCellTile, {
    output$tileOptions <- renderUI({
      target_column <- input$showCellTile$column |>
        as.integer()
      target_row <- input$showCellTile$row |>
        as.integer()

      if (target_row == 0) {
        selected_cell_options(NULL, NULL)
      } else {
        cell <- settings$final_grid$get_grid()[[target_column]][[target_row]]

        tiles <- lapply(cell$get_possible_tiles(), function(tile) {
          tile$get_hash()
        }) %>% unlist()

        if (identical(previous_preview, tiles)) {
          previous_preview <- tiles
          return()
        }

        previous_preview <- tiles
        selected_cell_options(cell, settings$tile_render_callback)
      }
    })
  })

  observeEvent(input$showTiles, {
    showModal(modalDialog(
      size = "l",
      title = "Available tiles",
      uiOutput("tileList"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
}

shinyApp(ui, server)
