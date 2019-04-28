package example

import example.GameEngine.{Player, Position}

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

  final case class Position(value: Int) extends AnyVal {
    def asString: String = value match {
      case 0 => "Start"
      case v => v.toString
    }
  }

  val addPlayerRegex: Regex = """add player (\w+)""".r
  val movePlayerRegex: Regex = """move (\w+) (\d+), (\d+)""".r

  def processInput(currentState: GameState, input: () => String): GameState = {
    val state = input() match {
      case addPlayerRegex(name) => addPlayer(name, currentState)
      case movePlayerRegex(name, r1, r2) => movePlayer(name, r1.toInt, r2.toInt, currentState)
      case _ => currentState.copy(message = "Unrecognized input")
    }
    Console.println(state.message)
    state
  }

  def addPlayer(name: Player, state: GameState): GameState =
    if (state.players.contains(name)) {
      state.copy(message = s"$name: already existing player")
    } else {
      val players = state.players + (name -> Position(0))
      state.copy(players = players, message = s"players: ${players.keySet.mkString(", ")}")
    }

  def movePlayer(name: Player,
                 diceRollOne: Int,
                 diceRollTwo: Int,
                 state: GameState): GameState = {
    val oldPosition = state.players(name)
    val newPosition = Position(oldPosition.value + diceRollOne + diceRollTwo)
    val newMessage = s"$name rolls $diceRollOne, $diceRollTwo. $name moves from ${oldPosition.asString} to " +
      s"${newPosition.asString}"
    val newPlayers = state.players + (name -> newPosition)
    val newState = state.copy(message = newMessage, players = newPlayers)
    newState
  }

}

case class GameState(players: ListMap[Player, Position] = ListMap.empty, message: String = "")
