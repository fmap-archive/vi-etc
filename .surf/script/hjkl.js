var ignoreOn = [];
var distance = 30;

function scroll (axis, dt) {
  switch (axis) {
    case 'X': window.scrollBy(dt, 0)
    case 'Y': window.scrollBy(0, dt)
  };
};

function lazyScroll (axis, dt) {
  return function() {
    scroll(axis, dt);
  };
}

function butBlacklist (blacklist, fn) {
  var currentURL  = window.location.href;
  var onBlacklist = false;
  blacklist.forEach(function(pat){
    if (pat.test(currentURL)) onBlacklist = true;
  });
  if (!onBlacklist) fn();
};

function butIgnored (fn) {
  butBlacklist(ignoreOn, fn);
}

function translateKey (code) {
  switch (code) {
    case 72: return 'h'
    case 74: return 'j'
    case 75: return 'k'
    case 76: return 'l'
  };
};

function butIgnoredScroll(axis, dt) {
  var fn = lazyScroll(axis, dt);
  butIgnored(fn);
};

function processKey (ev) {
  var key = translateKey(ev.keyCode);
  switch (key) {
    case 'h': butIgnoredScroll('X', -distance); break;
    case 'j': butIgnoredScroll('Y',  distance); break;
    case 'k': butIgnoredScroll('Y', -distance); break;
    case 'l': butIgnoredScroll('X',  distance); break;
  };
}

window.addEventListener('DOMContentLoaded', function() {
  document.addEventListener('keydown', processKey, false);
}, true);
