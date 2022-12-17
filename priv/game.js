export default {
  props: ['gameHash'],
  data() {
    return {
      nickname: window.sessionStorage.getItem("nickname") || "",
      in_game: false,
      waiting_players: [],
      playing_players: [],
      uuid: this.connection.uuid
    }
  },
  methods: {
    join() {
      this.connection.send('join', [this.gameHash, encodeURI(this.nickname.trim())])
      window.sessionStorage.setItem("nickname", this.nickname.trim())
    }
  },
  mounted() {
    let ths = this
    this.connection.on_data(function(response) {
      if (response.action == "join" && response.data == "ok") {
        ths.in_game = true
      }
      console.log("game on_data", response.action, response.data)
    })
  },
  template: `
    <h2>{{ gameHash }} {{ in_game }}</h2>
    <input v-model="nickname" placeholder="玩家暱稱（必須）" />
    <button @click="join" :disabled='this.nickname.trim().length == 0'>加入遊戲</button>
  `
}
