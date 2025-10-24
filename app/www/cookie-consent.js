/*=====================================================================
  cookie-consent.js  (GA4 version)
  ---------------------------------------------------------------
  • Shows a CookieConsent banner with Accept / Deny buttons
  • Stores consent state and handles both accept/deny cases
  • Sends the consent state back to Shiny (analytics_consent input)
  • Provides Shiny custom message handlers for GA4 management
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
        popup:   { background: '#303641' },
        button:  { background: '#b8de29' },
        highlight: { background: '#b4b4b4' }
      },
      type: 'opt-in',
      theme: 'classic',
      position: 'bottom',
      content: {
        message: 'Používáme pouze analytické cookies pro sledování návštěvnosti stránek.',
        allow: 'Přijmout',
        deny: 'Odmítnout',
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
    2 Shiny custom message handlers (GA4 version)
  -------------------------------------------------------------*/

  /* ---------- LOAD GA4 (gtag.js) ---------- */
  Shiny.addCustomMessageHandler('load-ua', function (msg) {
    // Prevent duplicate injection
    if (document.getElementById('ga4-script')) return;

    console.log('Loading GA4...');
    
    // Insert the gtag.js script
    var s = document.createElement('script');
    s.async = true;
    s.src = 'https://www.googletagmanager.com/gtag/js?id=G-TMY15YFFH8';
    s.id = 'ga4-script';
    document.head.appendChild(s);

    // Initialize GA4 after the script loads
    s.onload = function () {
      window.dataLayer = window.dataLayer || [];
      function gtag(){dataLayer.push(arguments);}
      gtag('js', new Date());
      gtag('config', 'G-TMY15YFFH8');
      console.log('GA4 initialized with G-TMY15YFFH8');
    };
  });

  /* ---------- REMOVE GA4 SCRIPT ---------- */
  Shiny.addCustomMessageHandler('remove-ua', function (msg) {
    console.log('Removing GA4...');
    var ga4Tag = document.getElementById('ga4-script');
    if (ga4Tag) {
      ga4Tag.parentNode.removeChild(ga4Tag);
    }
    
    // Clear GA4 dataLayer and gtag function
    if (window.dataLayer) {
      window.dataLayer = [];
    }
    if (window.gtag) {
      window.gtag = undefined;
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
    
    // Remove GA4 script
    var ga4Tag = document.getElementById('ga4-script');
    if (ga4Tag) {
      ga4Tag.parentNode.removeChild(ga4Tag);
    }
    
    // Clear GA4
    if (window.dataLayer) {
      window.dataLayer = [];
    }
    if (window.gtag) {
      window.gtag = undefined;
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