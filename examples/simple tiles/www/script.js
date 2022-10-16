let updatePreview = function(cell){
  Shiny.setInputValue(
    "showCellTile",
    {
      row: cell.dataset.row,
      column: cell.dataset.column
    }
  )
}