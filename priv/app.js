const { createApp } = Vue
import Default from './default.js'
import Game from './game.js'

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
  #websocket

  get uuid() {
    let v = window.sessionStorage.getItem("uuid")
    if (v != null) { return v }

    v = uuidv4()
    window.sessionStorage.setItem("uuid", v)
    return v
  }

  constructor() {
    let endpoint = window.location.host + "/websocket"
    endpoint = endpoint + "?uuid=" + this.uuid

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

app.config.globalProperties.connection = new Connection()
app.mount('#app')
