/* ==========================================================================
   navbar-active.js - Handle active tab highlighting for shiny.router
   ========================================================================== */

(function() {
  'use strict';

  function updateActiveTab() {
    // Get current route from URL hash
    var hash = window.location.hash || '#!/';
    
    // Normalize hash - remove leading #! if present
    var route = hash.replace(/^#/, '');
    if (route.startsWith('!')) {
      route = route.substring(1);
    }
    
    // Get all nav links
    var navLinks = document.querySelectorAll('.navbar-bottom .navbar-nav > li > a');
    
    navLinks.forEach(function(link) {
      var href = link.getAttribute('href') || '';
      
      // Check if this link matches the current route
      // Links use format like "#!/" or "#!/detail"
      if (href === route || href === '#' + route) {
        // Add active class to parent li
        link.parentElement.classList.add('active');
      } else {
        // Remove active class
        link.parentElement.classList.remove('active');
      }
    });
  }

  // Run on page load
  window.addEventListener('load', updateActiveTab);

  // Run when hash changes (navigation)
  window.addEventListener('hashchange', updateActiveTab);

  // Also listen for Shiny router events if available
  if (window.Shiny && window.Shiny.addCustomMessageHandler) {
    // This handler can be used by server-side to trigger updates
    Shiny.addCustomMessageHandler('update-active-tab', updateActiveTab);
  }
})();
