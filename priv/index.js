import { v4 as uuidv4 } from 'https://jspm.dev/uuid';
const PIN_REGEX = new RegExp('^[A-Z2-9]{4}$')

class Config {
  static get uuid() {
    let v = window.sessionStorage.getItem("uuid")

    if (v != null) { return v }

    v = uuidv4()
    window.sessionStorage.setItem("uuid", v)
    return v
  }

  static get is_exist_nickname() {
    return window.sessionStorage.getItem("nickname") != null
  }

  static get nickname() {
    return window.sessionStorage.getItem("nickname")
  }

  static set nickname(v) {
    window.sessionStorage.setItem("nickname", v)
  }

  static clear() {
    window.sessionStorage.clear()
  }
}

class Conn {
  #websocket

  constructor(uuid, nickname) {
    let endpoint = window.location.host + "/websocket"
    endpoint = endpoint + "?uuid=" + uuid + "&nickname=" + nickname

    if (location.protocol == 'https:') {
      endpoint = "wss://" + endpoint
    } else {
      endpoint = "ws://" + endpoint
    }

    this.#websocket = new WebSocket(endpoint);
  }

  on_data(callback) {
    this.#websocket.onmessage = function(evt) {
      callback(JSON.parse(evt.data))
    }
    return this
  }

  on_open(callback) {
    this.#websocket.onopen = function() {
      callback()
    }

    return this
  }

  send(action, data) {
    if (data) {
      this.#websocket.send(JSON.stringify({action: action, data: data}))
    } else {
      this.#websocket.send(JSON.stringify({action: action}))
    }
  }
}

$(document).ready(function() {
  let pin = window.location.hash.substr(1)

  if (PIN_REGEX.test(pin)) {
    // show game view
    if (!Config.is_exist_nickname) {
      $("#txt_nickname").show()
    }

    let conn;

    $("#btn_join").show().click(function() {
      conn = new Conn(Config.uuid, Config.nickname)
    })

  } else {
    // show create view
    $("#txt_nickname").show()
    $("#btn_create").show().click(function() {
      if ($("#txt_nickname").val() == '') { return }

      Config.nickname = $("#txt_nickname").val()

      let conn = new Conn(Config.uuid, Config.nickname)

      conn.on_open(function() {
        conn.send('create')
      })

      conn.on_data(function(response) {
        if (response.action == 'create') {
          window.location.assign('#' + response.data)
          window.location.reload()
        }
      })
    })
  }
})
//let uuid = window.sessionStorage.getItem("uuid")
//let nickname = window.sessionStorage.getItem("nickname")

//if (uuid == null) {
  //let uuid = uuidv4()
  //window.sessionStorage.setItem("uuid", uuid)
//}

//document.getElementById("uuid").value = uuid;
//document.getElementById("nickname").innerHTML = nickname;

//var websocket;

//var pin = document.getElementById("pin");
//var server = document.getElementById("server");
//var content = document.getElementById("content");
//var output = document.getElementById("output");
//var broadcast = document.getElementById("broadcast");

//let start_timer = Date.now();

//server.value = "ws://" + window.location.host + "/websocket";

//function connect()
//{
  //let wsHost = server.value + "?uuid=" + uuid + "&nickname=" + nickname;
  //websocket = new WebSocket(wsHost);
  //showScreen('<b>Connecting to: ' +  wsHost + '</b>');

  //websocket.onopen = function(evt) { on_open(evt) };
  //websocket.onclose = function(evt) { on_close(evt) };
  //websocket.onmessage = function(evt) { on_message(evt) };
  //websocket.onerror = function(evt) { on_error(evt) };
//};

//function disconnect() {
  //websocket.close();
//};

//document.getElementById("btn_connect").addEventListener("click", function() {
  //if (websocket && websocket.readyState == websocket.OPEN) {
    //disconnect();
  //} else {
    //connect();
  //};
//})

//document.getElementById("btn_create").addEventListener("click", function() { 
  //if (websocket.readyState == websocket.OPEN) {
    //websocket.send(JSON.stringify({action: 'create', data: [1,2,3]}));
  //} else {
    //showScreen('websocket is not connected');
  //};
//});

//document.getElementById("btn_join").addEventListener("click", function() {
  //if (websocket.readyState == websocket.OPEN) {
    //websocket.send(JSON.stringify({action: 'join', data: pin.value}));
  //} else {
    //showScreen('websocket is not connected');
  //};
//});

//document.getElementById("btn_ready").addEventListener("click", function() {
  //if (websocket.readyState == websocket.OPEN) {
    //websocket.send(JSON.stringify({action: 'ready'}));
  //} else {
    //showScreen('websocket is not connected');
  //};
//});

//document.getElementById("btn_start").addEventListener("click", function() {
  //if (websocket.readyState == websocket.OPEN) {
    //websocket.send(JSON.stringify({action: 'start'}));
  //} else {
    //showScreen('websocket is not connected');
  //};
//});

//document.getElementById("btn_died").addEventListener("click", function() {
  //if (websocket.readyState == websocket.OPEN) {
    //websocket.send(JSON.stringify({action: 'died'}));
  //} else {
    //showScreen('websocket is not connected');
  //};
//});

//function on_open(evt) {
  //showScreen('<span style="color: green;">CONNECTED </span>');

  //let pin = function() {
    //websocket.send("ping")
  //}

  //window.setInterval(pin, 1000);
//};

//function on_message(evt) {
  //let obj = JSON.parse(evt.data);

  //if (obj && obj['action'] == 'create') {
    //pin.value = obj['data']
  //}

  //if (obj && (obj['action'] == 'broadcast_waiting' || obj['action'] == 'broadcast_playing')) {
    //showBroadcast(obj);
  //} else {
    //console.log(obj);
    //showScreen('<span style="color: blue;">RESPONSE: ' + JSON.stringify(obj) + '</span>');
  //}
//};

//function on_close(evt) {
  //showScreen('<span style="color: red;">DISCONNECTED</span>');
//};

//function on_error(evt) {
  //showScreen('<span style="color: red;">ERROR: ' + evt.data + '</span>');
//};

//function showScreen(html) {
  //var el = document.createElement("p");
  //el.innerHTML = html;
  //output.insertBefore(el, output.firstChild);
//};

//function showBroadcast(obj) {
  //let diff = Date.now() - start_timer;
  //broadcast.innerText = JSON.stringify(obj) + " " + diff + "s"
//}
