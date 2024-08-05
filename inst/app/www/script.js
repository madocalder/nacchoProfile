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

$(document).on("shiny:value", (e) => {
    if(!e.name.match("show_definitions$")){
        return;
    }

    console.log(e.name);
});
$(() => {
    $('body').on('click', '.def-link', (e) => {
        const tgt = $(e.currentTarget).attr("href");
        const visible = $(tgt).hasClass("show");

        if (visible) {
            $(e.currentTarget).find("i").removeClass("fa-plus");
            $(e.currentTarget).find("i").addClass("fa-minus");
            return;
        }

        $(e.currentTarget).find("i").addClass("fa-plus");
        $(e.currentTarget).find("i").removeClass("fa-minus");
    });
})
