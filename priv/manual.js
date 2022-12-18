export default {
  data() {
    return {
    }
  },
  template: `
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
  `
}


