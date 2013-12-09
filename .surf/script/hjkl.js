var ignoreOn = [];
var distance = 30;
var keyQueue = [];

function scroll (axis, dt) {
  switch (axis) {
    case 'X': window.scrollBy(dt, 0); break;
    case 'Y': window.scrollBy(0, dt); break;
  };
};

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

function onInput(ev) {
  var target = ev.target || ev.srcElement;
  var name   = target.nodeType==1 ? target.nodeName.toLowerCase() : ''
  return /input|select|textarea/.test(name)
}

function processKey (ev) {
  if (onInput(ev)) return;
  var key = queueKey(ev);
  switch (keyQueue[1]) {
    case 'h':  scroll('X', -distance);        break;
    case 'j':  scroll('Y',  distance);        break;
    case 'k':  scroll('Y', -distance);        break;
    case 'l':  scroll('X',  distance);        break;
    case 'G':  scroll('Y',  document.height); break;
    case 'g':  if (keyQueue[0]=='g') { scroll('Y', -document.height); break; }
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

butBlacklist(ignoreOn, function() {
  document.addEventListener('keydown', processKey, false);
});
