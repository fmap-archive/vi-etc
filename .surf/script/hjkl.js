var ignoreOn = [];
var distance = 30;
var keyQueue = [];

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

function queueKey(ev) {
  var chr;
  switch (ev.keyCode) {
    case 71: chr = 'g'; break;
    case 72: chr = 'h'; break;
    case 74: chr = 'j'; break;
    case 75: chr = 'k'; break;
    case 76: chr = 'l'; break;
  };
  keyQueue[0] = keyQueue[1];
  keyQueue[1] = ev.shiftKey ? chr.toUpperCase() : chr;
};

function butIgnoredScroll(axis, dt) {
  var fn = lazyScroll(axis, dt);
  butIgnored(fn);
};

function processKey (ev) {
  var key = queueKey(ev);
  switch (keyQueue[1]) {
    case 'h':  butIgnoredScroll('X', -distance);        break;
    case 'j':  butIgnoredScroll('Y',  distance);        break;
    case 'k':  butIgnoredScroll('Y', -distance);        break;
    case 'l':  butIgnoredScroll('X',  distance);        break;
    case 'G':  butIgnoredScroll('Y',  document.height); break;
    case 'g':  if (keyQueue[0]=='g') {
      butIgnoredScroll('Y', -document.height); break; 
    }
  };
}

document.addEventListener('keydown', processKey, false);
