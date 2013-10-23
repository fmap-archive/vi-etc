function style(e) {
//if (document.URL.match("youtube.com/watch")) {
//  var videoURL = document.querySelector("[property='og:video']").getAttribute("content");
//  window.location = videoURL;
//}
  if (document.URL.match("gwern.net/")) {
    document.querySelector("#adsense").style.display = "none";
  }
  else if (document.URL.match("stackoverflow.com")) {
    document.querySelector(".everyonelovesstackoverflow").style.display = "none";
  }
  else if (document.URL.match("wikipedia.org/")) {
    document.querySelector("#mw-page-base").style.display = "none";
    document.querySelector("#mw-navigation").style.display = "none";
    document.querySelector(".mw-body").style.margin = "0";
    document.querySelector("#footer").style.display = "none";
    [].forEach.call(document.querySelectorAll('.mw-editsection'),
                    function(f) { f.style.display = "none"; });
  }
  else if (document.URL.match("wiki.aineko")) {
    document.querySelector("body").style.zoom = "1.25";
  }
}

document.addEventListener("DOMContentLoaded", style, true)
