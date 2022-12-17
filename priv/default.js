export default {
  data() {
    return { }
  },
  methods: {
    create() {
      this.connection.send('create')
    }
  },
  mounted() {
    this.connection.on_data(function(response) {
      console.log("default on_data")
      if (response.action == 'create') {
        window.location.assign("#" + response.data)
      }
    })
  },
  template: `<div><button @click="create">创建游戏</button></div>`
}

