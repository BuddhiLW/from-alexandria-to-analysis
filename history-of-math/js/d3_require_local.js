// d3-require for the history-of-math bundle (history-of-math.build swaps it
// in for the "d3-require" npm module through shadow-cljs :js-options
// :resolve).
//
// Clerk 0.18's render loads two libraries at runtime with d3-require:
// render-katex asks for "katex@0.16.4" and render-plotly for
// "plotly.js-dist@2.15.1". d3-require's default resolver fetches the
// package.json and then the script from cdn.jsdelivr.net. The build copies
// both scripts into <out-path>/vendor/<name>.js (history-of-math.build/vendor
// names the same two), and this resolver sends those two names there, so a
// built page loads them from its own origin.
//
// The vendor directory is found from the bundle's own <script> tag: the
// pages load it as <script type="module" src="[../]_data/<hash>.js">, so
// vendor/ is ../vendor/ relative to that src. Any other name (a page served
// without the built bundle, a library the series does not ship) goes through
// d3-require's default resolver unchanged.
"use strict";

var d3 = require("d3-require/dist/d3-require.js");

var VENDORED = ["katex@0.16.4", "plotly.js-dist@2.15.1"];

function vendorRoot() {
  if (typeof document === "undefined") return null;
  var s = document.querySelector('script[type="module"][src*="_data/"]');
  return s ? new URL("../vendor/", s.src).href : null;
}

function resolve(name, base) {
  var root = typeof name === "string" && VENDORED.indexOf(name) >= 0 && vendorRoot();
  return root ? root + name + ".js" : d3.require.resolve(name, base);
}

exports.require = d3.requireFrom(resolve);
exports.requireFrom = d3.requireFrom;
exports.resolveFrom = d3.resolveFrom;
exports.RequireError = d3.RequireError;
exports.VENDORED = VENDORED;
