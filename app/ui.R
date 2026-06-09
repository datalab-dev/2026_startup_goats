# UI bundler for the whole app
# sources all the individual page UIs and stitches them together into a single-page app with a nav bar and progress dots

# also includes the meta tags and font links in the head
# and imports the stylesheet for the design system

ui <- fluidPage(
 
  tags$head(
    # Inject iPad / PWA meta tags into the OUTER shinylive document so
    # "Add to Home Screen" treats this as a standalone app.
    tags$script(HTML("
      (function () {
        let doc;
        try { doc = window.parent.document; } catch (e) { doc = document; }
        if (!doc) doc = document;
        function meta(n, c) {
          if (doc.querySelector('meta[name=\"' + n + '\"]')) return;
          const m = doc.createElement('meta');
          m.name = n; m.content = c;
          doc.head.appendChild(m);
        }
        meta('apple-mobile-web-app-capable',         'yes');
        meta('mobile-web-app-capable',               'yes');
        meta('apple-mobile-web-app-status-bar-style','default');
        meta('apple-mobile-web-app-title',           'GOATvzn');
        meta('theme-color',                          '#2BB5A0');
        meta('viewport', 'width=device-width, initial-scale=1, viewport-fit=cover');
        doc.title = 'GOATvzn';
      })();
    ")),

    # Mark numeric inputs required so the :invalid CSS selector can flag empties.
    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        $('.num-wrap input[type=\"number\"]').attr('required', 'required');
      });
    ")),

    # Lexend font + design system stylesheet
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Lexend:wght@400;500;600;700;800&display=swap"),
    tags$link(rel = "stylesheet", href = "styles.css")
  ),

  # persistent app bar (logo + SOP reference)
  nav_bar(),

  # progress dots
  uiOutput("page_dots"),

  # the 6-step flow
  tabsetPanel(
    id = "page_tabs", type = "hidden",
    tabPanel("p1", page1_ui()),
    tabPanel("p2", page2_ui()),
    tabPanel("p3", page3_ui()),
    tabPanel("p4", page4_ui()),
    tabPanel("p5", page5_ui()),
    tabPanel("p6", page6_ui())
  ),

  # global back / next bar (Next only shows on the linear pages 1-3)
  div(class = "btn-row between", style = "margin-top:18px;",
      uiOutput("prev_button"),
      uiOutput("next_button"))
)

ui
