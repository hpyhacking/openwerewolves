import RolesView from './roles.js'

export default {
  props: ['roles', 'players'],
  emits: ['start', 'ready'],
  components: {
    RolesView
  },
  computed: {
    is_ready() {
      let p = this.players.find(e => e.uuid == this.connection.uuid)
      return p?.is_ready
    },

    is_ready_to_go() {
      let p = this.players.find(e => e.uuid == this.connection.uuid)
      return p?.is_ready && this.roles
    }
  },
  methods: {
    start() { this.$emit('start') },
    ready() { this.$emit('ready') }
  },
  template: "#waiting"
}

