$(document).on('shiny:connected', function() {
  $('#landscape_1-takeaway').addClass('clicked-link').click();
})


$(document).on('shiny:connected', function() {
  $('#workforce_1-viz_filters').addClass('clicked-link').click();
})


$(document).on('click', '.action-link', function() {
  $('.action-link').removeClass('clicked-link'); // Remove class from all links
  $(this).addClass('clicked-link'); // Add class to the clicked link
});


$(document).on('click', '.action-link-2', function() {
  $('.action-link-2').removeClass('clicked-link'); // Remove class from all links
  $(this).addClass('clicked-link'); // Add class to the clicked link
});



$(document).on("shiny:value", (e) => {
    if(!e.name.match("show_definitions$")){
        return;
    }

    console.log(e.name);
});
$(() => {
  let timeout;
  $('body').on('click', '.def-link', (e) => {
    timeout = setTimeout(() => {
      const tgt = $(e.currentTarget).attr("href");
      const visible = $(tgt).hasClass("show");

      if (visible) {
          $(e.currentTarget).find("i").removeClass("fa-plus");
          $(e.currentTarget).find("i").addClass("fa-minus");
          return;
      }

      $(e.currentTarget).find("i").addClass("fa-plus");
      $(e.currentTarget).find("i").removeClass("fa-minus");
    }, 500);
  });
})


// for jobs chart
function adjustXAxisOffset() {
  var chart = $('#workforce_1-figure_jobs').highcharts();
  var contentWidth = $('#resize-content').width();
  if (chart && contentWidth > 0) {
    var basePercentage = 0.53;  // Base percentage at shiny load
    var minWidth = 400;  // Minimum width for the content div before max adjustment
    var maxWidth = 1200; // Maximum width for the content div before minimum adjustment
    var offsetPercentage;

    if (contentWidth < minWidth) {
      offsetPercentage = 0.40;  // Decrease percentage as the window gets narrower
    } else if (contentWidth > maxWidth) {
      offsetPercentage = basePercentage;  // Use base percentage for wider windows
    } else {
      offsetPercentage = basePercentage + (0.60 - basePercentage) * ((maxWidth - contentWidth) / (maxWidth - minWidth));
    }

    var offsetValue = contentWidth * offsetPercentage;
    chart.xAxis[0].update({
      offset: -offsetValue
    });
  }
}

$(document).on('shiny:connected', function() {
  adjustXAxisOffset();
});

$(window).on('resize', function() {
  adjustXAxisOffset();
});
