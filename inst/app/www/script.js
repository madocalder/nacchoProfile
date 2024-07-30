$(document).on('shiny:connected', function(event) {
  Shiny.addCustomMessageHandler('setNamespace', function(namespace) {
    console.log("Namespace received:", namespace); // Log the namespace
    var actionLink = $('#' + namespace + '-takeaway');
    actionLink.addClass('clicked-link');
  });
});

$(document).on('click', '.action-link', function() {
  $('.action-link').removeClass('clicked-link'); // Remove class from all links
  $(this).addClass('clicked-link'); // Add class to the clicked link
});
