export default {
  props: ['gameHash'],
  data() {
    return {
      nickname: window.sessionStorage.getItem("nickname") || "",
      is_in_game: false,
      is_waiting: false,
      is_playing: false,
      roles: undefined,
      waiting_players: [],
      playing_players: [],
      win: [],
      uuid: this.connection.uuid
    }
  },
  computed: {
    is_ready() {
      return this.waiting_players.find(e => e.uuid == this.uuid).is_ready
    },
    is_can_go() {
      return this.roles != undefined
    },
    is_died() {
      return this.playing_players.find(e => e.uuid == this.uuid).is_died
    }
  },
  methods: {
    join() {
      this.connection.send('join', [this.gameHash, encodeURI(this.nickname.trim())])
      window.sessionStorage.setItem("nickname", this.nickname.trim())
    },
    ready() {
      this.connection.send('ready')
    },
    go() {
      this.connection.send('start')
    },
    died() {
      this.connection.send('died')
    }
  },
  mounted() {
    let $ = this

    this.connection.on_data(function(response) {
      if (response.action == "join" && response.data == "error") {
        window.location.assign("/")
      }

      if (response.action == "join" && response.data == "ok") {
        $.is_in_game = true
      }

      if (response.action == "broadcast_waiting") {
        $.is_waiting = true
        $.is_playing = false
        $.roles = response.data.roles
        $.waiting_players = response.data.players
      }

      if (response.action == "broadcast_playing") {
        $.is_waiting = false
        $.is_playing = true
        $.playing_players = response.data.players
      }

      if (response.action == "broadcast_win") {
        $.win = response.data
      }

      console.log("game on_data", response.action, response.data)
    })
  },
  template: `
    <section>
      <h2>{{ gameHash }}</h2>
    </section>
    <section v-if="is_in_game && is_waiting">
      <p v-if='win.length'>{{ win }}</p>
      <ul>
        <li v-for="p in waiting_players">
          <span> {{ decodeURI(p.nickname) }} </span>
          <span> {{ p.is_ready }} </span>
        </li>
      </ul>
      <button @click="ready" v-if='!is_ready'>準備開始</button>
      <p v-else>等待開始中...</p>
      <p v-if='is_can_go'>
        <span>平民 {{ roles[0] }}</span>
        <span>臥底 {{ roles[1] }}</span>
        <span>白板 {{ roles[2] }}</span>
      </p>
      <button @click="go" v-if='is_ready && is_can_go'>開始遊戲</button>
    </section>
    <section v-if="is_in_game && is_playing">
      <ul>
        <li v-for="p in playing_players">
          <span> {{ decodeURI(p.nickname) }} </span>
          <span> {{ p.is_died }} </span>
        </li>
      </ul>
      <button @click="died" v-if='!is_died'>投票出局</button>
      <h3 v-else>撲街了</h3>
    </section>
    <section v-if="!is_in_game">
      <input v-model="nickname" placeholder="玩家暱稱（必須）" />
      <button @click="join" :disabled='this.nickname.trim().length == 0'>加入遊戲</button>
    </section>
  `
}
