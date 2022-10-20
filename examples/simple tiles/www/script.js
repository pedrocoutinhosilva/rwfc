let updatePreview = function(cell){
  Shiny.setInputValue(
    "showCellTile",
    {
      row: cell.dataset.row,
      column: cell.dataset.column
    }
  )
}

Shiny.addCustomMessageHandler('updateCell', function(message) {
  let query = `#${message.container} [data-row = "${message.cell.row}"][data-column = "${message.cell.column}"]`
  document.querySelector(query).innerHTML = message.content
});

let autoTrigger = false
Shiny.addCustomMessageHandler('toggleAutoTrigger', function(message) {
  autoTrigger = message.value
});

let triggerAutoStep = function() {
  Shiny.setInputValue( "autoTrigger", autoTrigger, {priority: "event"} )
}
Shiny.addCustomMessageHandler('triggerAutoStep', function(message) {
  if (autoTrigger) {
    triggerAutoStep()
  }
});

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