export default {
  props: ['hash'],
  data() {
    return {
      nickname: window.sessionStorage.getItem("nickname") || ""
    }
  },
  methods: {
    join() {
      this.connection.send('join', [this.hash, encodeURI(this.nickname.trim())])
      window.sessionStorage.setItem("nickname", this.nickname.trim())
    }
  },
  template: "#joining"
}
