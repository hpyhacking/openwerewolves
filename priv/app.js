const { createApp } = Vue
import Default from './default.js'
import Game from './game.js'

import { v4 as uuidv4 } from 'https://jspm.dev/uuid';

const PIN_REGEX = new RegExp('^[A-Z2-9]{4}$')

const app = createApp({
  data() {
    return {
      currentPath: window.location.hash
    }
  },
  computed: {
    gameHash() {
      let match = this.currentPath.slice(1).match(PIN_REGEX)
      return match && match[0]
    },

    currentView() {
      let path = this.currentPath.slice(1)

      if (path == '') { return Default }
      if (PIN_REGEX.test(path)) { return Game }
    }
  },
  mounted() {
    window.addEventListener('hashchange', () => {
		  this.currentPath = window.location.hash
		})
  },
  template: `<component :is="currentView" :gameHash="gameHash" />`
})

class Connection {
  #ping
  #websocket
  #callback

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

  #connect() {
    this.#websocket = new WebSocket(this.endpoint);

    this.#websocket.onclose = () => {
      setTimeout(() => {
        this.#connect()
      }, 500)
    }

    this.#websocket.onmessage = (e) => {
      if (this.#callback) {
        this.#callback(JSON.parse(e.data))
      }
    }
  }

  set on_data(callback) {
    this.#callback = callback
  }

  send(action, data) {
    if (data) {
      this.#websocket.send(JSON.stringify({action: action, data: data}))
    } else {
      this.#websocket.send(JSON.stringify({action: action}))
    }
  }
}

app.config.globalProperties.connection = new Connection()
app.mount('#app')
