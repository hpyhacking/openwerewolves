export default {
  methods: {
    create() {
      this.connection.send('create')
    }
  },
  mounted() {
    this.connection.on('create', (hash) => {
      window.location.assign("#" + hash)
    })
  },
  template: "#default"
}

