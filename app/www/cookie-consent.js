/*=====================================================================
  cookie-consent.js  (Fixed Universal Analytics version)
  ---------------------------------------------------------------
  • Shows a CookieConsent banner with Accept / Deny buttons
  • Stores consent state and handles both accept/deny cases
  • Sends the consent state back to Shiny (analytics_consent input)
  • Provides Shiny custom message handlers for UA management
=====================================================================*/

(function () {
  /*-------------------------------------------------------------
    1 Initialize CookieConsent banner
  -------------------------------------------------------------*/
  window.addEventListener('load', function () {
    if (!window.cookieconsent) {
      console.error('CookieConsent library not loaded.');
      return;
    }

    window.cookieconsent.initialise({
      palette: {
        popup:   { background: '#303641' },  //#303641
        button:  { background: '#b8de29' },  // accept button
        highlight: { background: '#b4b4b4' } // deny button
      },
      type: 'opt-in',  // This enables the deny button
      theme: 'classic',
      position: 'bottom',
      content: {
        message: 'Používáme pouze analytické cookies pro sledování návštěvnosti stránek.',
        allow: 'Přijmout',     // Accept button text
        deny: 'Odmítnout',     // Deny button text
        link: 'Více...',
        href: '/#!/about',
        target: '_self'
      },

      onStatusChange: function (status, chosenBefore) {
        console.log('Cookie consent status:', status);
        
        if (status === 'allow') {
          // ----- USER ACCEPTED -----
          document.cookie =
            'analytics_consent=true;path=/;max-age=' + (60 * 60 * 24 * 365) +
            ';secure;SameSite=Lax';

          if (window.Shiny) {
            Shiny.setInputValue('analytics_consent', true, { priority: 'event' });
          }
          
        } else if (status === 'deny') {
          // ----- USER DENIED -----
          document.cookie =
            'analytics_consent=false;path=/;max-age=' + (60 * 60 * 24 * 365) +
            ';secure;SameSite=Lax';

          if (window.Shiny) {
            Shiny.setInputValue('analytics_consent', false, { priority: 'event' });
          }
        }
      },

      onInitialise: function (status) {
        // Check if user has already made a choice
        if (status === 'allow') {
          if (window.Shiny) {
            Shiny.setInputValue('analytics_consent', true, { priority: 'event' });
          }
        } else if (status === 'deny') {
          if (window.Shiny) {
            Shiny.setInputValue('analytics_consent', false, { priority: 'event' });
          }
        }
      }
    });
  });

  /*-------------------------------------------------------------
    2 Shiny custom message handlers (UA version)
  -------------------------------------------------------------*/

  /* ---------- LOAD UNIVERSAL ANALYTICS (analytics.js) ---------- */
  Shiny.addCustomMessageHandler('load-ua', function (msg) {
    // Prevent duplicate injection
    if (document.getElementById('ua-script')) return;

    console.log('Loading Universal Analytics...');
    
    // Insert the analytics.js script
    var s = document.createElement('script');
    s.async = true;
    s.src = 'https://www.google-analytics.com/analytics.js';
    s.id = 'ua-script';
    document.head.appendChild(s);

    // Initialize UA after the script loads
    s.onload = function () {
      var uaId = 'UA-79200582-7';  // Your actual UA property ID

      if (typeof ga === 'function') {
        ga('create', uaId, 'auto');
        ga('send', 'pageview');
        console.log('Universal Analytics initialized');
      } else {
        console.warn('Universal Analytics (ga) not available after script load.');
      }
    };
  });

  /* ---------- REMOVE UA SCRIPT ---------- */
  Shiny.addCustomMessageHandler('remove-ua', function (msg) {
    console.log('Removing Universal Analytics...');
    var uaTag = document.getElementById('ua-script');
    if (uaTag) {
      uaTag.parentNode.removeChild(uaTag);
    }
    
    // Clear any GA queued commands
    if (window.ga) {
      window.ga = undefined;
    }
  });

  /* ---------- DELETE CONSENT COOKIE ---------- */
  Shiny.addCustomMessageHandler('delete-consent-cookie', function (msg) {
    console.log('Deleting consent cookie...');
    // Remove analytics consent cookie
    document.cookie = 'analytics_consent=; path=/; expires=Thu, 01 Jan 1970 00:00:00 GMT; secure; SameSite=Lax';
    
    // Remove CookieConsent's own cookie to reset the banner
    document.cookie = 'cookieconsent_status=; path=/; expires=Thu, 01 Jan 1970 00:00:00 GMT';
  });

  /* ---------- CLEAR SHINY INPUT (set to NULL) ---------- */
  Shiny.addCustomMessageHandler('clear-consent-input', function (msg) {
    console.log('Clearing Shiny consent input...');
    if (window.Shiny) {
      Shiny.setInputValue('analytics_consent', null, { priority: 'event' });
    }
  });

  /* ---------- RESET CONSENT (force re-show banner) ---------- */
  Shiny.addCustomMessageHandler('reset-consent', function (msg) {
    console.log('Resetting cookie consent...');
    
    // Remove all related cookies
    document.cookie = 'analytics_consent=; path=/; expires=Thu, 01 Jan 1970 00:00:00 GMT; secure; SameSite=Lax';
    document.cookie = 'cookieconsent_status=; path=/; expires=Thu, 01 Jan 1970 00:00:00 GMT';
    
    // Remove UA script
    var uaTag = document.getElementById('ua-script');
    if (uaTag) {
      uaTag.parentNode.removeChild(uaTag);
    }
    
    // Clear GA
    if (window.ga) {
      window.ga = undefined;
    }
    
    // Clear Shiny input
    if (window.Shiny) {
      Shiny.setInputValue('analytics_consent', null, { priority: 'event' });
    }
    
    // Force reload to re-show banner
    setTimeout(function() {
      window.location.reload();
    }, 100);
  });

})();