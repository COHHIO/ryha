// Resize window on box maximize to properly display charts
$(document).ready(function() {
  $('.btn-tool[data-card-widget="maximize"]').click(function() {
    window.dispatchEvent(new Event('resize'));
  });
});

// Keep popover open while hovering over it
$(document).ready(function(){
  $('[data-toggle="popover"]').popover({
    trigger: "manual",
    html: true,
    animation: false
  })
  .on("mouseenter", function() {
    var _this = this;
    $(this).popover("show");
    $(".popover").on("mouseleave", function() {
      $(_this).popover('hide');
    });
  }).on("mouseleave", function() {
    var _this = this;
    setTimeout(function() {
      if (!$(".popover:hover").length) {
        $(_this).popover("hide");
      }
    }, 100);
  });
});
