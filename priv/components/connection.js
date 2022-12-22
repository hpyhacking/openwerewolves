import { v4 as uuidv4 } from 'https://jspm.dev/uuid';

class Connection {
  #ping = 0
  #websocket
  #callback = {} 

  get uuid() {
    let v = window.sessionStorage.getItem("uuid")
    if (v != null) { return v }

    v = uuidv4()
    window.sessionStorage.setItem("uuid", v)
    return v
  }

  get state() {
    return this.#websocket.readyState
  }

  get endpoint() {
    let endpoint = window.location.host + "/websocket"
    endpoint = endpoint + "?uuid=" + this.uuid

    if (location.protocol == 'https:') {
      endpoint = "wss://" + endpoint
    } else {
      endpoint = "ws://" + endpoint
    }

    return endpoint
  }

  constructor() {
    this.#connect()

    setInterval(() => {
      if (this.#websocket.readyState == WebSocket.OPEN) {
        this.#websocket.send("ping")
      }
    }, 5000);
  }

  ping(callback) {
    setInterval(() => {
      this.#ping = this.#ping + 1
      let callback = this.#callback["ping"]
      if (callback) {
        callback({ping: this.#ping, state: this.state})
      }
    }, 1000)
  }

  #connect() {
    this.#websocket = new WebSocket(this.endpoint);

    this.#websocket.onclose = () => {
      setTimeout(() => {
        this.#connect()
      }, 500)
    }

    this.#websocket.onmessage = (e) => {
      let rsp = JSON.parse(e.data)
      let callback = this.#callback[rsp.action]
      if (callback) {
        callback(rsp.data)
      }
    }
  }

  on(action, callback) {
    this.#callback[action] = callback
  }

  send(action, data) {
    if (data) {
      this.#websocket.send(JSON.stringify({action: action, data: data}))
    } else {
      this.#websocket.send(JSON.stringify({action: action}))
    }
  }
}

const ConnectionStatus = {
  data() {
    return {
      ping: 0,
      state: 0
    }
  },
  mounted() {
    this.connection.on("ping", (data) => {
      this.ping = data.ping
      this.state = data.state
    })
  },
  template: "<section> <span>{{ping}}</span> <span>{{state}}</span> </section>"
}

export default Connection
