var ExclusionHandler = (function() {
  // Private variables
  var selectedStudies = [];

  // reload selectedStudies from existing visual state
  function reload() {
    // Check if the element exists
    if ($("#setup_exclude-interface").length === 0) {
      // If not, wait and try again
      setTimeout(reload, 100);
      return;
    }

    // Find all groups with opacity of 0.3
    $("#setup_exclude-interface g[id^='setup_exclude-line']").each(function() {
      if ($(this).css("opacity") == "0.3") {
        var studyName = $(this).attr("data-study-name");
        if (studyName && selectedStudies.indexOf(studyName) === -1) {
          selectedStudies.push(studyName);
        }
      }
    });
  }

  // Public API
  return {
    init: function() {
      // Start trying to initialize after a delay
      setTimeout(reload, 200);

      // Shade lines on hover
      $("#setup_exclude-interface g[id^='setup_exclude-line']").on({
        mouseenter: function() {
          $(this).css("opacity", "1.0");
          $(this).find("rect").css("opacity", "0.5");
        },
        mouseleave: function() {
          // Restore to selected state or default
          var studyName = $(this).attr("data-study-name");
          if (selectedStudies.includes(studyName)) {
            $(this).css("opacity", "0.3");
          } else {
            $(this).css("opacity", "1.0");
          }
          $(this).find("rect").css("opacity", "0.0");
        }
      });

      // Click handler
      $("#setup_exclude-interface g[id^='setup_exclude-line']").on("click", function() {
        var clickedStudy = $(this).attr("data-study-name");

        // Toggle study selection
        var index = selectedStudies.indexOf(clickedStudy);
        if (index > -1) {
          selectedStudies.splice(index, 1);
        } else {
          selectedStudies.push(clickedStudy);
        }

        // Update opacity for all study lines
        $("#setup_exclude-interface g[data-study-name='" + clickedStudy + "']").each(function() {
          if (selectedStudies.includes(clickedStudy)) {
            $(this).css("opacity", "0.3");
          } else {
            $(this).css("opacity", "1.0");
          }
        });

        // Send selected studies to Shiny input
        Shiny.setInputValue("setup_exclude-exclusions", selectedStudies);
      });

    },
  };
})();

// Auto-initialize when document is ready
$(document).ready(function() {
  ExclusionHandler.init();
});
