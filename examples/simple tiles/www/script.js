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

// function scale() {
//   document.querySelectorAll('#preview').forEach(scaled => {
//       parent = scaled.parentNode,
//       ratio = (parent.offsetWidth / scaled.offsetWidth),
//       padding = scaled.offsetHeight * ratio;

//     scaled.style.transform = 'scale(' + ratio + ')';
//     scaled.style.transformOrigin = 'top left';

//     parent.style.paddingTop = padding; // keeps the parent height in ratio to child resize
//   })
// }

// window.addEventListener('resize', scale);