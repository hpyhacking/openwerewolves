const { createApp } = Vue
import Connection from './connection.js'
import Default from './default.js'
import Game from './game.js'
import Manual from './manual.js'

import { v4 as uuidv4 } from 'https://jspm.dev/uuid';

const PIN_REGEX = new RegExp('^[A-Z2-9]{4}$')

const app = createApp({
  data() {
    return {
      currentPath: window.location.hash
    }
  },
  components: {
    Default, Game, Manual
  },
  computed: {
    gameHash() {
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
  template: `
    <Game v-if='in_game' :gameHash="gameHash" />
    <Default v-else />
    <Manual />
  `
})


app.config.globalProperties.connection = new Connection()
app.mount('#app')
