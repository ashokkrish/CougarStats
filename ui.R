ui <- tagList(withTags(html(
  head(
    ## CougarStats logo and styling
    link(rel = "stylesheet", type = "text/css", href = "cougarstats-styles.css"),
    link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),

    ## Authors CSS preloaded (as style) early so when modal inserts its <link rel=stylesheet>,
    ## fetch is from cache (no network delay). Link+content moved into Shadow by global
    ## observer for scoping (preflight scoped, no global pollution; main app unaffected).
    link(rel = "preload", href = "authors.css", as = "style"),

    ## ShinyDarkmode
    use_darkmode(),

    ## Font Awesome for icons in modal content.
    link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.1/css/all.min.css"),

    ## Amplitude Analytics
    script(src = "https://cdn.amplitude.com/libs/analytics-browser-2.11.1-min.js.gz"),
    script(src = "https://cdn.amplitude.com/libs/plugin-session-replay-browser-1.25.0-min.js.gz"),
    script(HTML(r"{
    window.amplitude.add(
      window.sessionReplay.plugin({ sampleRate: 1 })
    );
    window.amplitude.init(
      '9c16daacc728f3aa3e4fe91129eca5e8',
      { autocapture: { elementInteractions: true } }
    );}")),

    ## Global lightweight isolation for authors modal (runs early, no per-include cost).
    ## Observes for the <link authors.css> + content div inserted by includeHTML.
    ## Wraps them in shadow host (display:contents so layout unchanged), moves link+div in.
    ## This keeps full isolation (scoped preflight/utilities) but modal payload is tiny
    ## (just link + divs), no script/template in authors.html, no eval/clone delay on open.
    ## Preload makes the stylesheet apply instantly on insert.
    script(HTML(r"{
    (function() {
      'use strict';
      function isolatePair(linkEl, contentDiv) {
        if (!linkEl || !contentDiv || contentDiv.shadowRoot) return;
        var parent = linkEl.parentNode;
        if (!parent) return;
        var host = document.createElement('div');
        host.setAttribute('data-tailwind-isolated', 'true');
        host.style.display = 'contents';
        parent.insertBefore(host, linkEl);
        var shadow = host.attachShadow({mode: 'open'});
        shadow.appendChild(linkEl);
        shadow.appendChild(contentDiv);
        // Ensure close button works (BS delegation retargets across shadow)
        var hideBtn = contentDiv.querySelector('#authors-hide');
        if (hideBtn) {
          hideBtn.addEventListener('click', function(e) {
            var modal = host.closest('.modal') || document.getElementById('shiny-modal');
            if (modal && window.jQuery) {
              window.jQuery(modal).modal('hide');
            } else if (modal) {
              modal.classList.remove('show');
              modal.style.display = 'none';
            }
          });
        }
      }

      var observer = new MutationObserver(function(muts) {
        muts.forEach(function(mut) {
          var added = Array.prototype.slice.call(mut.addedNodes);
          added.forEach(function(node) {
            if (node.nodeType !== 1) return;
            // Look for the authors link + its following content div
            if (node.tagName === 'LINK' && node.rel === 'stylesheet' && node.href && node.href.indexOf('authors.css') !== -1) {
              var next = node.nextElementSibling;
              if (next && next.classList && next.classList.contains('text-zinc-100') && next.classList.contains('font-sans')) {
                isolatePair(node, next);
              }
            }
            // Also check inside added containers
            var links = node.querySelectorAll ? node.querySelectorAll('link[rel="stylesheet"][href*="authors.css"]') : [];
            for (var i=0; i<links.length; i++) {
              var l = links[i];
              var nxt = l.nextElementSibling;
              if (nxt && nxt.classList && nxt.classList.contains('text-zinc-100') && nxt.classList.contains('font-sans')) {
                isolatePair(l, nxt);
              }
            }
          });
        });
      });
      observer.observe(document, {childList: true, subtree: true});

      // Fallbacks for timing (e.g. if already in DOM or late insert)
      function tryExisting() {
        var link = document.querySelector('link[rel="stylesheet"][href*="authors.css"]');
        if (link) {
          var nxt = link.nextElementSibling;
          if (nxt && nxt.classList && nxt.classList.contains('text-zinc-100') && nxt.classList.contains('font-sans')) {
            isolatePair(link, nxt);
          }
        }
      }
      document.addEventListener('DOMContentLoaded', tryExisting);
      setTimeout(tryExisting, 0);
      setTimeout(tryExisting, 50);
    })();
    }")),

    ## NOTE: this is important to fix the Title; somehow there are a whack-tonne
    ## of title tags and none of theme are correct, and they all otherwise
    ## override the simple title= argument value.
    script(HTML(r"{
    (() => {
      const titleText = 'CougarStats';
      document.title = titleText;
      const head = document.head || document.documentElement;
      const titles = head.getElementsByTagName('title');
      while (titles.length > 1) titles[titles.length - 1].remove();
      if (titles.length === 0) {
        const t = document.createElement('title');
        t.textContent = titleText;
        head.appendChild(t);
      } else if (titles[0].textContent !== titleText) {
        titles[0].textContent = titleText;
      }
    })();
    }"))
  ),
  withTags(div(
    div(
      style = paste(
        sep = "; ",
        "background-color: #18536F",
        "display: flex",
        "align-items: flex-start",
        "justify-content: space-between"
      ),
      div(style = "align-self: center;",
          img(src = "CougarStatsLogo.png",
              style = "height: 100px; padding: 10px;"),
          span("CougarStats",
               style = paste(sep = "; ",
                             "color: var(--off-white)",
                             "font-weight: bold",
                             "font-style: italic",
                             "font-size: 24pt",
                             "vertical-align: middle"))),

      div(id = "top-level-action-buttons",
          style = "align-self: center; margin-right: 20px;",
          actionButton("togglemode", "Toggle Dark or Light Mode", icon = icon("sun")),
          actionButton("authors_show",
                       "Authors",
                       class = "btn btn-info",
                       onclick = "event.stopImmediatePropagation(); Shiny.setInputValue('authors_show', Date.now(), {priority: 'event'});"))
    ),
    navbarPage(
      NULL,# NOTE: title is intentionally NULL; see previous div.

      tabPanel("Descriptive Statistics", descStatsUI(id = "ds")),
      tabPanel("Probability Distributions", probDistUI(id = "pd")),
      tabPanel("Sample Size Estimation", sampSizeEstUI(id = "sse")),
      tabPanel("Statistical Inference", statInfrUI(id = "si")),
      tabPanel("Regression and Correlation", regressionAndCorrelationUI(id = "rc")),
      tabPanel("Machine Learning", machineLearningUI(id = "ml")),

      theme = bs_theme(version = 4, primary = "#18536F"),
      id = "methods-nav"
    )
  ))
)))
