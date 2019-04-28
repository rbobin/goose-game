package example

import example.GameEngine.Player

import scala.collection.immutable.ListMap
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
      case addPlayerRegex(name) => addPlayer(name, currentState)
      case _ => currentState.copy(message = "Unrecognized input")
    }
    Console.println(state.message)
    state
  }

  def addPlayer(name: Player, state: GameState): GameState =
    if (state.players.contains(name)) {
      state.copy(message = s"$name: already existing player")
    } else {
      val players = state.players + (name -> 0)
      state.copy(players = players, message = s"players: ${players.keySet.mkString(", ")}")
    }

}

case class GameState(players: ListMap[Player, Int] = ListMap.empty, message: String = "")
