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
