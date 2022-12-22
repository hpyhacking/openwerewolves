import RolesView from './roles.js'

export default {
  props: ['roles', 'players', 'wins'],
  emits: ['restart'],
  methods: {
    restart() { this.$emit('restart') },
  },
  components: {
    RolesView
  },
  computed: {
  },
  mounted() {
    console.log(this.players)
  },
  template: "#winning"
}

