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
      uuid: this.connection.uuid,
      topic: undefined
    }
  },
  computed: {
    is_ready() {
      return this.waiting_players.find(e => e.uuid == this.uuid).is_ready
    },
    has_roles() {
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
    },
    inspect() {
      this.connection.send('inspect')
    }
  },
  mounted() {
    this.connection.on_data = (response) => {
      if (response.action == "join" && response.data == "error") {
        window.location.assign("/")
      }

      if (response.action == "join" && response.data == "ok") {
        this.is_in_game = true
      }

      if (response.action == "broadcast_waiting") {
        this.is_waiting = true
        this.is_playing = false
        this.roles = response.data.roles
        this.waiting_players = response.data.players
      }

      if (response.action == "broadcast_playing") {
        this.is_waiting = false
        this.is_playing = true
        this.roles = response.data.roles
        this.playing_players = response.data.players
      }

      if (response.action == "broadcast_win") {
        this.win = response.data
      }

      if (response.action == "inspect") {
        this.topic = response.data

        if (response.data == "") {
          this.topic = "[blank]"
        }

        setTimeout(() => { this.topic = undefined }, 3000)
      }
    }
  },
  template: `
    <section>
      <h1>谁是卧底 #{{ gameHash }}</h1>
    </section>
    <section v-if="is_in_game && is_waiting">
      <fieldset v-if='win.length'>
        <legend>胜利</legend>
        <ul class='players-list'>
          <li v-for="p in win">
            <span v-if="p == 'folk'">平民</span>
            <span v-if="p == 'fool'">白板</span>
            <span v-if="p == 'spy'">卧底</span>
          </li>
        </ul>
      </fieldset>
      <fieldset>
        <legend>玩家列表</legend>
        <ul class='players-list'>
          <li v-for="p in waiting_players">
            <span> {{ decodeURI(p.nickname) }} </span>
            <span v-if='p.is_ready' class='playing'>已准备</span>
            <span v-else class='waiting'>等待中</span>
          </li>
        </ul>
      </fieldset>
      <fieldset>
        <legend>等待区</legend>
        <p class='roles' v-if='has_roles'>
          <span>平民 {{ roles[0] }} | </span>
          <span>卧底 {{ roles[1] }} | </span>
          <span>白板 {{ roles[2] }}</span>
        </p>
        <button @click="ready" v-if='!is_ready'>准备开始</button>
        <p v-else>等待其他玩家...</p>
        <button @click="go" v-if='is_ready && has_roles'>开始游戏</button>
      </fieldset>
    </section>
    <section v-if="is_in_game && is_playing">
      <fieldset>
        <legend>玩家列表</legend>
        <ul class='players-list'>
          <li v-for="p in playing_players">
            <span> {{ decodeURI(p.nickname) }} </span>
            <span v-if='p.is_died'>已出局</span>
          </li>
        </ul>
      </fieldset>
      <fieldset>
        <legend>信息</legend>
        <p class='roles' v-if='has_roles'>
          <span>平民 {{ roles[0] }} | </span>
          <span>卧底 {{ roles[1] }} | </span>
          <span>白板 {{ roles[2] }}</span>
        </p>
        <button @click="inspect" v-if='!topic'>查看词</button>
        <h3 class='topic' v-else-if="topic == '[blank]'">[白板]（祝你好运）</h3>
        <h3 class='topic' v-else>{{ topic }}</h3>
      </fieldset>
      <fieldset v-if='!is_died'>
        <legend>投票</legend>
        <button @click="died">出局</button>
      </fieldset>
    </section>
    <section v-if="!is_in_game">
      <fieldset>
        <legend>加入游戏</legend>
        <label>玩家昵称（必填）</label>
        <input v-model="nickname" id="nickname" placeholder="阿猫/阿狗" type='text'/>
        <button @click="join" :disabled='this.nickname.trim().length == 0'>加入游戏</button>
      </fieldset>
    </section>
  `
}
