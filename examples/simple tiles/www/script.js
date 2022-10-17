let updatePreview = function(cell){
  Shiny.setInputValue(
    "showCellTile",
    {
      row: cell.dataset.row,
      column: cell.dataset.column
    }
  )
}

let triggerAutoStep = function() {
  Shiny.setInputValue( "renderFinished", "trigger", {priority: "event"} )
}