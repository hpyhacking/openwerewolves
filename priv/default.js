export default {
  data() {
    return {
      state: []
    }
  },
  computed: {
  },
  methods: {
    create() {
      this.connection.send('create')
    }
  },
  mounted() {
    this.connection.on_data = (response) => {
      if (response.action == 'create') {
        window.location.assign("#" + response.data)
      }
    }
  },
  template: `<div> <button @click="create">创建游戏</button> </div>`
}

