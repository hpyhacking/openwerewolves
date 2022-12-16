import { v4 as uuidv4 } from 'https://jspm.dev/uuid';

let uuid = window.sessionStorage.getItem("uuid")
let nickname = window.sessionStorage.getItem("nickname")

if (uuid == null) {
  let uuid = uuidv4()
  window.sessionStorage.setItem("uuid", uuid)
}

document.getElementById("uuid").value = uuid;
document.getElementById("nickname").innerHTML = nickname;

var websocket;

var pin = document.getElementById("pin");
var server = document.getElementById("server");
var content = document.getElementById("content");
var output = document.getElementById("output");
var broadcast = document.getElementById("broadcast");

let start_timer = Date.now();

server.value = "ws://" + window.location.host + "/websocket";

function connect()
{
  let wsHost = server.value + "?uuid=" + uuid + "&nickname=" + nickname;
  websocket = new WebSocket(wsHost);
  showScreen('<b>Connecting to: ' +  wsHost + '</b>');

  websocket.onopen = function(evt) { on_open(evt) };
  websocket.onclose = function(evt) { on_close(evt) };
  websocket.onmessage = function(evt) { on_message(evt) };
  websocket.onerror = function(evt) { on_error(evt) };
};

function disconnect() {
  websocket.close();
};

document.getElementById("btn_connect").addEventListener("click", function() {
  if (websocket && websocket.readyState == websocket.OPEN) {
    disconnect();
  } else {
    connect();
  };
})

document.getElementById("btn_create").addEventListener("click", function() { 
  if (websocket.readyState == websocket.OPEN) {
    websocket.send(JSON.stringify({action: 'create', data: [1,2,3]}));
  } else {
    showScreen('websocket is not connected');
  };
});

document.getElementById("btn_join").addEventListener("click", function() {
  if (websocket.readyState == websocket.OPEN) {
    websocket.send(JSON.stringify({action: 'join', data: pin.value}));
  } else {
    showScreen('websocket is not connected');
  };
});

document.getElementById("btn_ready").addEventListener("click", function() {
  if (websocket.readyState == websocket.OPEN) {
    websocket.send(JSON.stringify({action: 'ready'}));
  } else {
    showScreen('websocket is not connected');
  };
});

document.getElementById("btn_start").addEventListener("click", function() {
  if (websocket.readyState == websocket.OPEN) {
    websocket.send(JSON.stringify({action: 'start'}));
  } else {
    showScreen('websocket is not connected');
  };
});

document.getElementById("btn_died").addEventListener("click", function() {
  if (websocket.readyState == websocket.OPEN) {
    websocket.send(JSON.stringify({action: 'died'}));
  } else {
    showScreen('websocket is not connected');
  };
});

function on_open(evt) {
  showScreen('<span style="color: green;">CONNECTED </span>');

  let pin = function() {
    websocket.send("ping")
  }

  window.setInterval(pin, 1000);
};

function on_message(evt) {
  let obj = JSON.parse(evt.data);

  if (obj && obj['action'] == 'create') {
    pin.value = obj['data']
  }

  if (obj && (obj['action'] == 'broadcast_waiting' || obj['action'] == 'broadcast_playing')) {
    showBroadcast(obj);
  } else {
    console.log(obj);
    showScreen('<span style="color: blue;">RESPONSE: ' + JSON.stringify(obj) + '</span>');
  }
};

function on_close(evt) {
  showScreen('<span style="color: red;">DISCONNECTED</span>');
};

function on_error(evt) {
  showScreen('<span style="color: red;">ERROR: ' + evt.data + '</span>');
};

function showScreen(html) {
  var el = document.createElement("p");
  el.innerHTML = html;
  output.insertBefore(el, output.firstChild);
};

function showBroadcast(obj) {
  let diff = Date.now() - start_timer;
  broadcast.innerText = JSON.stringify(obj) + " " + diff + "s"
}
