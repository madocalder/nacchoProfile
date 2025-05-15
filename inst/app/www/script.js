// Have the correct action links click upon load of app
// =============================================================================

$(document).on('shiny:connected', function() {
  // Set up a polling mechanism to ensure all elements are ready
  const waitForElements = setInterval(function() {
    const landscape = $('#landscape_1-takeaway');
    const workforce = $('#workforce_1-takeaway');
    const finance = $('#finance_1-takeaway');
    const services = $('#services_1-get_services');
    const topics = $('#topic_areas_1-takeaway');

    // Check if all elements exist and are visible
    if (landscape.length && workforce.length && finance.length && services.length && topics.length) {
      clearInterval(waitForElements); // Stop polling once all elements are found

      // Trigger clicks for each element
      landscape.addClass('clicked-link')[0].click();
      workforce.addClass('clicked-link')[0].click();
      finance.addClass('clicked-link')[0].click();
      services.addClass('clicked-link')[0].click();
      topics.addClass('clicked-link')[0].click();

      console.log('Auto-clicks triggered for all elements.');
    }
  }, 100); // Check every 100ms
});


// Add and remove class when toggling links
$(document).on('click', '.action-link', function() {
  $('.action-link').removeClass('clicked-link');
  $(this).addClass('clicked-link');
});

$(document).on('click', '.action-link-2', function() {
  $('.action-link-2').removeClass('clicked-link');
  $(this).addClass('clicked-link');
});

$(document).on('click', '.action-link-3', function() {
  $('.action-link-3').removeClass('clicked-link');
  $(this).addClass('clicked-link');
});


// triggers the occupational classifications to click
// =============================================================================
Shiny.addCustomMessageHandler('triggerClick', function(id) {
  document.getElementById(id).click();
});


Shiny.addCustomMessageHandler('triggerClickService', function(id) {
  document.getElementById(id).click();
});




// change the definitions link from plus to minus
// =============================================================================
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
});




// For the dropdown button popover
// =============================================================================
document.addEventListener('DOMContentLoaded', function() {
  var popoverTriggerList = [].slice.call(document.querySelectorAll('#workforce_1-slide_button'));
  var popoverList = popoverTriggerList.map(function (popoverTriggerEl) {
    return new bootstrap.Popover(popoverTriggerEl, {
      content: 'Change measure here',
      trigger: 'hover',
      placement: 'right'
    });
  });
});




// changes the position of the extra filter button based on window size
// =============================================================================
window.addEventListener('load', function() {

  function updatePosition() {
    const sidebar = document.querySelector('.sidebar'); // Select the sidebar class
    const fixedDiv = document.querySelector('.dropdown-button');

    if (window.innerWidth > 991) {
      const sidebarWidth = sidebar.offsetWidth;

      // Calculate new left position
      fixedDiv.style.left = `calc(100px + ${sidebarWidth}px)`;
    } else {
      // For screen widths 991px or smaller, set left to 0px
      fixedDiv.style.left = '0px';
    }
  }

  // Add smooth transition for the left property
  const fixedDiv = document.querySelector('.dropdown-button');
  fixedDiv.style.transition = 'left 0.3s ease-in-out';

  // Update position on load
  updatePosition();

  // Respond if the sidebar is toggled to collapse
  const toggleButton = document.querySelector('.collapse-toggle');
  toggleButton.addEventListener('click', function() {
    // Wait for the collapse/expand animation to complete (if any)
    setTimeout(updatePosition, 300); // Adjust timeout duration to match animation time
  });

  // Update position on window resize
  window.addEventListener('resize', updatePosition);
});




//---------------------------------------------------------- nav toggle /
$(document).ready(function() {
  // Toggle the visibility of the menu when the hamburger icon is clicked
  $('.toggle-hide').click(function() {
    $('.toggle-show').toggle();
  });

  // Ensure the menu reappears when the window height exceeds 600px
  $(window).resize(function() {
    if ($(window).height() > 600) {
      $('.toggle-show').show();  // Reset to visible when the screen height is large
    }
  });
});
