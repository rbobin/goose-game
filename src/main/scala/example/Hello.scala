package example

import example.GameEngine.Player

import scala.util.matching.Regex


object Hello extends App {
  println("SIGTERM for exit")

  val stream = Stream.continually({ () => scala.io.StdIn.readLine })
  stream.foldLeft(GameState())(GameEngine.processInput)
}

object GameEngine {

  type Player = String
  type Message = String
  val addPlayerRegex: Regex = """add player (\w+)""".r

  def processInput(currentState: GameState, input: () => String): GameState = {
    val state = input() match {
      case addPlayerRegex(player) => addPlayer(player, currentState)
      case _ => currentState.copy(message = "Unrecognized input")
    }
    Console.println(state.message)
    state
  }

  def addPlayer(player: Player, state: GameState): GameState =
    if (state.players.contains(player)) {
      state.copy(message = s"$player: already existing player")
    } else {
      val players = player :: state.players
      state.copy(players = players, message = s"players: ${players.mkString(", ")}")
    }

}

case class GameState(players: List[Player] = Nil, message: String = "")