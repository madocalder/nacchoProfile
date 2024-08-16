$(document).on('shiny:connected', function() {
  $('#landscape_1-takeaway').addClass('clicked-link').click();
});


$(document).on('shiny:connected', function() {
  $('#workforce_1-takeaway').addClass('clicked-link').click();
});


$(document).on('click', '.action-link', function() {
  $('.action-link').removeClass('clicked-link'); // Remove class from all links
  $(this).addClass('clicked-link'); // Add class to the clicked link
});


$(document).on('click', '.action-link-2', function() {
  $('.action-link-2').removeClass('clicked-link'); // Remove class from all links
  $(this).addClass('clicked-link'); // Add class to the clicked link
});


Shiny.addCustomMessageHandler('triggerClick', function(id) {
  document.getElementById(id).click();
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
});




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
      offsetPercentage = 0.0;
      // Change alignment to "left" for narrow content widths
      chart.xAxis[0].update({
        labels: {
          align: 'left'
        }
      });
    } else {
      // Reset alignment to "center" for wider content widths
      chart.xAxis[0].update({
        labels: {
          align: 'center'
        }
      });
    }

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



// For the dropdown button popover
document.addEventListener('DOMContentLoaded', function() {
  var popoverTriggerList = [].slice.call(document.querySelectorAll('#workforce_1-slide_button'));
  var popoverList = popoverTriggerList.map(function (popoverTriggerEl) {
    return new bootstrap.Popover(popoverTriggerEl, {
      content: 'Click here to change measure.',
      trigger: 'hover',
      placement: 'right'
    });
  });
});



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
