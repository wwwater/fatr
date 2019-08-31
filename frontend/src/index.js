"use strict";

require("./index.html");
var Elm = require("./Main");

var storageKey = "token";
var now = new Date();
var date = now.toISOString().substring(0, 10);

var elm = Elm.Main.fullscreen({jwt: localStorage.getItem(storageKey) || "", initDate: date});

// var pairsToConnect;

elm.ports.save.subscribe(function(value) {
  // console.log("save port called with", value);
  localStorage.setItem(storageKey, value);
});

elm.ports.remove.subscribe(function() {
  // console.log("remove port called");
  localStorage.removeItem(storageKey);
});

elm.ports.drawConnections.subscribe(function(pairs) {
  // console.log("line port called with", pairs);
  // pairsToConnect = pairs;
  if (pairs.length > 0) {
    setTimeout(function () {return drawConnectingLines(pairs);}, 100);
  }
});

//window.onresize = function () { drawConnectingLines(pairsToConnect)};

function drawConnectingLines(pairs) {
    var existingLines = document.getElementsByClassName("incoming-line");
    for (var i = 0; i < existingLines.length; ++i) {
        existingLines[i].style.width = "0";
    }
    for (var i = 0; i < pairs.length; ++i) {
        connectPersons(pairs[i][0], pairs[i][1])
    }
}


function getOffset(el) {
    var rect = el.getBoundingClientRect();
    return {
        left: rect.left + window.pageXOffset,
        top: rect.top + window.pageYOffset,
        width: rect.width || el.offsetWidth,
        height: rect.height || el.offsetHeight
    };
}

function connectPersons(id1, id2) { // draw a line connecting elements
    var div1 = document.getElementById("person-" + id1);
    var div2 = document.getElementById("person-" + id2);
    var thickness = 2;
    var off1 = getOffset(div1);
    var off2 = getOffset(div2);
    // center
    var x1 = off1.left + off1.width / 2;
    var y1 = off1.top + off1.height / 2;
    // center 2
    var x2 = off2.left + off2.width / 2;
    var y2 = off2.top + off2.height / 2;
    // distance
    var length = Math.sqrt(((x2-x1) * (x2-x1)) + ((y2-y1) * (y2-y1)));
    // center
    var cx = ((x1 + x2) / 2) - (length / 2);
    var cy = ((y1 + y2) / 2) - (thickness / 2);
    // angle
    var angle = Math.atan2((y1-y2),(x1-x2))*(180/Math.PI);

    var l = document.getElementById("incoming-line-" + id2);
    l.style.left = (cx - off2.left) + "px";
    l.style.top = (cy - off2.top) + "px";
    l.style.width = length + "px";
    l.style.transform = "rotate(" + angle + "deg)";
    document.getElementById("tree-page-content").style.opacity = 1;
}

