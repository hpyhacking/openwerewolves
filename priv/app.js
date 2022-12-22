const { createApp } = Vue

import DefaultPage from './pages/default.js'
import GamePage from './pages/game.js'

import GuideView from './components/guide.js'
import Connection from './components/connection.js'

const PIN_REGEX = new RegExp('^[A-Z2-9]{4}$')

const app = createApp({
  data() {
    return {
      currentPath: window.location.hash
    }
  },
  components: {
    DefaultPage, GamePage, GuideView
  },
  computed: {
    hash() {
      let match = this.currentPath.slice(1).match(PIN_REGEX)
      return match && match[0]
    },

    in_game() {
      return PIN_REGEX.test(this.currentPath.slice(1))
    }
  },
  mounted() {
    window.addEventListener('hashchange', () => {
		  this.currentPath = window.location.hash
		})
  },
  template: "#app"
})

app.config.globalProperties.connection = new Connection()
app.mount('#app_wrapper')
