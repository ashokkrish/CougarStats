// Copies a rendered plotly plot to the clipboard as a PNG image.
// Used by the "Copy to Clipboard" buttons in Probability Distributions,
// Simple Linear Regression and Polynomial Regression:
//   <button data-copy-plot="<plot id>" onclick="copyPlotToClipboard('<plot id>')">
// Loaded once from ui.R (it used to be repeated inline in each module's UI).
function copyPlotToClipboard(plotId) {
  var plotDiv = document.getElementById(plotId);
  if (!plotDiv) return;
  var btn = document.querySelector('[data-copy-plot="' + plotId + '"]');

  Plotly.toImage(plotDiv, {format: 'png', width: plotDiv.offsetWidth, height: plotDiv.offsetHeight})
    .then(function(dataUrl) { return fetch(dataUrl); })
    .then(function(res) { return res.blob(); })
    .then(function(blob) {
      return navigator.clipboard.write([new ClipboardItem({'image/png': blob})]);
    })
    .then(function() {
      if (btn) {
        var orig = btn.innerHTML;
        btn.innerHTML = '<i class="fa fa-check"></i> Copied!';
        btn.disabled = true;
        setTimeout(function() {
          btn.innerHTML = orig;
          btn.disabled = false;
        }, 2000);
      }
    })
    .catch(function(err) {
      alert('Could not copy to clipboard. Your browser may not support this feature, or the page must be served over HTTPS.');
    });
}
