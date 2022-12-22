import RolesView from './roles.js'

export default {
  props: ['roles', 'players', 'topic', 'uuid'],
  emits: ['died', 'inspect'],
  methods: {
    died() { this.$emit('died') },
    inspect() { this.$emit('inspect') }
  },
  components: {
    RolesView
  },
  computed: {
    is_died() {
      return this.players.find(e => e.uuid == this.uuid).is_died
    }
  },
  mounted() {
  },
  template: "#playing"
}
