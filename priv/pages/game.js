import JoiningView from '../components/game/joining.js'
import PlayingView from '../components/game/playing.js'
import WaitingView from '../components/game/waiting.js'
import WinningView from '../components/game/winning.js'

export default {
  props: ['hash'],
  data() {
    return {
      view: "join",
      players: [],
      winning_players: [],
      roles: undefined,
      topic: undefined,
      wins: undefined,
      uuid: this.connection.uuid
    }
  },
  methods: {
    start() { this.connection.send("start") },
    ready() { this.connection.send("ready") },
    died() { this.connection.send('died') },
    inspect() { this.connection.send('inspect') },
    restart() { this.wins = undefined }
  },
  computed: {
    in_joining() {
      return this.view == "join"
    },

    in_playing() {
      return this.view == "playing" && this.wins == undefined
    },

    in_waiting() {
      return this.view == "waiting" && this.wins == undefined
    },

    in_winning() {
      return this.wins != undefined
    }
  },
  components: {
    JoiningView, WaitingView, PlayingView, WinningView
  },
  mounted() {
    this.connection.on("join", (data) => {
      if (data != "ok") {
        window.location.assign("/")
      }
    })

    this.connection.on("broadcast_waiting", (data) => {
      this.view = 'waiting'
      this.roles = data.roles
      this.players = data.players
    })

    this.connection.on("broadcast_playing", (data) => {
      this.view = 'playing'
      this.roles = data.roles
      this.players = data.players
    })

    this.connection.on("broadcast_winning", (data) => {
      this.roles = data.roles
      this.winning_players = data.players
      this.wins = data.wins
    })

    this.connection.on("inspect", (data) => {

      if (data == "") {
        this.topic = "[blank]"
      } else {
        this.topic = data
      }

      setTimeout(() => { this.topic = undefined }, 3000)
    })
  },
  template: "#game"
}
