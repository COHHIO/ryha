// Wait for the document to be fully loaded before executing JavaScript
$(document).ready(function() {
  // Attach a click event handler to the element with the id 'help-tab'
  $('#to-help-tab').click(function() {
    // Change input$go_to_help to trigger the observeEvent() in R
    Shiny.setInputValue('to_help', Math.random());
  });
});
