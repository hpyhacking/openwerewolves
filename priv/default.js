export default {
  data() {
    return {
      state: []
    }
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
  template: `
  <section>
    <fieldset>
      <legend>谁是卧底</legend>
      <button @click="create">创建游戏</button>
    </fieldset>
  </section>
  `
}

