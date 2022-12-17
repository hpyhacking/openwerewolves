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
        $.roles = response.data.roles
        $.playing_players = response.data.players
      }

      if (response.action == "broadcast_win") {
        $.win = response.data
      }

      if (response.action == "inspect") {
        $.topic = response.data
        if (response.data == "") {
          $.topic = "[blank]"
        }
        setTimeout(function() { $.topic = undefined }, 3000)
      }

      console.log("game on_data", response.action, response.data)
    })
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
    <section>
      <h3>游戏流程</h3>
      <ul>
        <li>共有卧底、平民与白板三种角色，根据参与游戏人数每种角色人数不同</li>
        <li>游戏开始后，每位玩家获得一个词，但此时并不知道自己与其他玩家的角色</li>
        <li>位于玩家列表首位的玩家先发言，然后按照座次顺序依次发言</li>
        <li>发言中，请隐约地描述、暗示你看到的词，唯一的限制是不可说到词中包含的字或词</li>
        <li>发言后，由本轮首先发言的玩家主持投票 - 谁是卧底，得票最多的玩家出局</li>
        <li>若投票时得票相同，玩家可自行决定如何分出胜（例如猜拳或进行决斗）</li>
        <li>若投票后任何一方达到胜利条件则游戏结束</li>
        <li>若未达到胜利条件，从被投票出局的玩家的下一位玩家开始重复发言与投票环节</li>
      </ul>

      <h3>角色目标</h3>
      <ul>
        <li>平民的词汇与卧底不同，甄别出人数众多的其他平民，携手将卧底投票出局</li>
        <li>卧底的词汇与平民不同，掩饰身份并甄别出队友（也许没有），将尽量多的平民投票出局</li>
        <li>白板看到的词汇是一片空白（废话），观察并合理发言努力活到最后（祝你好运）</li>
      </ul>

      <h3>胜利条件</h3>
      <ul>
        <li>平民：当卧底全部被投票出局</li>
        <li>卧底：当平民数量减至与卧底初始数量一致</li>
        <li>白板：当平民获胜时仍未出局</li>
      </ul>
    </section>
    <section>
    </section>
  `
}
