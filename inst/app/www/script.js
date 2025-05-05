// Keep popover open while hovering over it
$(document).ready(function() {
  // Initialize popovers with manual trigger and allow HTML content
  $('[data-bs-toggle="popover"]').popover({
    trigger: "manual",  // Prevents automatic toggling, giving full control over when to show/hide
    html: true,         // Allows HTML inside the popover content
    animation: false    // Disables animation for faster response
  })
  .on("mouseenter", function() {
    var _this = this; // Store reference to the trigger element

    // Show popover when mouse enters the trigger
    $(this).popover("show");

    // Get the dynamically generated popover ID assigned by Bootstrap
    var popoverId = bootstrap.Popover.getInstance(_this)._element.getAttribute("aria-describedby");
    var $popover = $("#" + popoverId); // Select the popover element using its ID

    // Prevent popover from hiding when hovering over it
    $popover.on("mouseenter", function() {
      clearTimeout($(_this).data("timeout")); // Cancel any pending hide action
    }).on("mouseleave", function() {
      hidePopoverWithDelay(_this); // Start the hide timer when mouse leaves popover
    });
  })
  .on("mouseleave", function() {
    hidePopoverWithDelay(this); // Start the hide timer when mouse leaves trigger
  });

  // Function to delay hiding the popover to allow smooth transitions
  function hidePopoverWithDelay(element) {
    var timeout = setTimeout(function() {
      // Get the popover ID for the given trigger element
      var popoverId = bootstrap.Popover.getInstance(element)._element.getAttribute("aria-describedby");
      var popover = document.getElementById(popoverId);

      // Hide popover only if both the trigger and popover are not hovered
      if (!popover?.matches(":hover") && !element.matches(":hover")) {
        $(element).popover("hide");
      }
    }, 100); // Delay ensures smoother transitions and prevents flickering

    // Store the timeout ID so it can be cleared if needed
    $(element).data("timeout", timeout);
  }
});
